//! Backend: IR to Wasm.

use crate::cfg::CFGInfo;
use crate::entity::EntityRef;
use crate::ir::{
    ExportKind, Func, FuncDecl, FunctionBody, ImportKind, Module, Type, Value, ValueDef,
};
use crate::passes::rpo::RPO;
use crate::Operator;
use anyhow::Result;
use std::borrow::Cow;
use std::collections::HashMap;

pub mod stackify;
use stackify::{Context as StackifyContext, WasmBlock};
pub mod treeify;
use treeify::Trees;
pub mod localify;
use localify::Localifier;

pub struct WasmFuncBackend<'a> {
    body: &'a FunctionBody,
    rpo: RPO,
    trees: Trees,
    ctrl: Vec<WasmBlock<'a>>,
    locals: Localifier,
}

macro_rules! op {
    ($name:tt) => {
        Some(wasm_encoder::Instruction::$name)
    };
}

impl<'a> WasmFuncBackend<'a> {
    pub fn new(body: &'a FunctionBody) -> Result<WasmFuncBackend<'a>> {
        log::debug!("Backend compiling:\n{}\n", body.display_verbose("| "));
        let cfg = CFGInfo::new(body);
        let rpo = RPO::compute(body);
        log::debug!("RPO:\n{:?}\n", rpo);
        let trees = Trees::compute(body);
        log::debug!("Trees:\n{:?}\n", trees);
        let ctrl = StackifyContext::new(body, &cfg, &rpo)?.compute();
        log::debug!("Ctrl:\n{:?}\n", ctrl);
        let locals = Localifier::compute(body, &cfg, &trees);
        log::debug!("Locals:\n{:?}\n", locals);
        Ok(WasmFuncBackend {
            body,
            rpo,
            trees,
            ctrl,
            locals,
        })
    }

    pub fn compile(&self) -> Result<wasm_encoder::Function> {
        let mut func = wasm_encoder::Function::new(
            self.locals
                .locals
                .values()
                .skip(self.body.blocks[self.body.entry].params.len())
                .map(|&ty| (1, wasm_encoder::ValType::from(ty)))
                .collect::<Vec<_>>(),
        );

        for block in &self.ctrl {
            self.lower_block(block, &mut func);
        }

        // If the last block was a Block, Loop or If, then the type
        // may not match, so end with an Unreachable.
        match self.ctrl.last() {
            Some(&WasmBlock::Block { .. })
            | Some(&WasmBlock::Loop { .. })
            | Some(&WasmBlock::If { .. }) => {
                func.instruction(&wasm_encoder::Instruction::Unreachable);
            }
            _ => {}
        }
        func.instruction(&wasm_encoder::Instruction::End);

        log::debug!("Compiled to:\n{:?}\n", func);

        Ok(func)
    }

    fn lower_block(&self, block: &WasmBlock<'_>, func: &mut wasm_encoder::Function) {
        match block {
            WasmBlock::Block { body, .. } => {
                func.instruction(&wasm_encoder::Instruction::Block(
                    wasm_encoder::BlockType::Empty,
                ));
                for sub_block in &body[..] {
                    self.lower_block(sub_block, func);
                }
                func.instruction(&wasm_encoder::Instruction::End);
            }
            WasmBlock::Loop { body, .. } => {
                func.instruction(&wasm_encoder::Instruction::Loop(
                    wasm_encoder::BlockType::Empty,
                ));
                for sub_block in &body[..] {
                    self.lower_block(sub_block, func);
                }
                func.instruction(&wasm_encoder::Instruction::End);
            }
            WasmBlock::Br { target } => {
                func.instruction(&wasm_encoder::Instruction::Br(target.index()));
            }
            WasmBlock::If {
                cond,
                if_true,
                if_false,
            } => {
                self.lower_value(*cond, func);
                func.instruction(&wasm_encoder::Instruction::If(
                    wasm_encoder::BlockType::Empty,
                ));
                for sub_block in &if_true[..] {
                    self.lower_block(sub_block, func);
                }
                if if_false.len() > 0 {
                    func.instruction(&wasm_encoder::Instruction::Else);
                    for sub_block in &if_false[..] {
                        self.lower_block(sub_block, func);
                    }
                }
                func.instruction(&wasm_encoder::Instruction::End);
            }
            WasmBlock::Select {
                selector,
                targets,
                default,
            } => {
                self.lower_value(*selector, func);
                func.instruction(&wasm_encoder::Instruction::BrTable(
                    Cow::Owned(
                        targets
                            .iter()
                            .map(|label| label.index())
                            .collect::<Vec<_>>(),
                    ),
                    default.index(),
                ));
            }
            WasmBlock::Leaf { block } => {
                for &inst in &self.body.blocks[*block].insts {
                    // If this value is "owned", do nothing: it will be lowered in
                    // the one place it's used.
                    if self.trees.owner.contains_key(&inst) {
                        return;
                    }
                    self.lower_inst(inst, /* root = */ true, func);
                }
            }
            WasmBlock::BlockParams { from, to } => {
                debug_assert_eq!(from.len(), to.len());
                for (&from, &(_, to)) in from.iter().zip(to.iter()) {
                    if self.locals.values[to].is_empty() {
                        continue;
                    }
                    self.lower_value(from, func);
                }
                for &(_, to) in to.iter().rev() {
                    if self.locals.values[to].is_empty() {
                        continue;
                    }
                    self.lower_set_value(to, func);
                }
            }
            WasmBlock::Return { values } => {
                for &value in &values[..] {
                    self.lower_value(value, func);
                }
                func.instruction(&wasm_encoder::Instruction::Return);
            }
            WasmBlock::Unreachable => {
                func.instruction(&wasm_encoder::Instruction::Unreachable);
            }
        }
    }

    fn lower_value(&self, value: Value, func: &mut wasm_encoder::Function) {
        let value = self.body.resolve_alias(value);
        let local = match &self.body.values[value] {
            &ValueDef::BlockParam(..) | &ValueDef::Operator(..) => self.locals.values[value][0],
            &ValueDef::PickOutput(orig_value, idx, _) => self.locals.values[orig_value][idx],
            _ => unreachable!(),
        };
        func.instruction(&wasm_encoder::Instruction::LocalGet(local.index() as u32));
    }

    fn lower_set_value(&self, value: Value, func: &mut wasm_encoder::Function) {
        debug_assert_eq!(
            self.locals.values[value].len(),
            1,
            "Value {} has no local",
            value
        );
        let local = self.locals.values[value][0];
        func.instruction(&wasm_encoder::Instruction::LocalSet(local.index() as u32));
    }

    fn lower_inst(&self, value: Value, root: bool, func: &mut wasm_encoder::Function) {
        match &self.body.values[value] {
            &ValueDef::Operator(ref op, ref args, _) => {
                for &arg in &args[..] {
                    if self.trees.owner.contains_key(&arg) {
                        self.lower_inst(arg, /* root = */ false, func);
                    } else {
                        self.lower_value(arg, func);
                    }
                }
                self.lower_op(op, func);
                if root {
                    for &local in &self.locals.values[value] {
                        func.instruction(
                            &wasm_encoder::Instruction::LocalSet(local.index() as u32),
                        );
                    }
                }
            }
            &ValueDef::PickOutput(..) => {
                self.lower_value(value, func);
            }
            def => unreachable!("Unexpected inst: {:?}", def),
        }
    }

    fn lower_op(&self, op: &Operator, func: &mut wasm_encoder::Function) {
        let inst = match op {
            Operator::Unreachable => Some(wasm_encoder::Instruction::Unreachable),
            Operator::Nop => None,
            Operator::Call { function_index } => Some(wasm_encoder::Instruction::Call(
                function_index.index() as u32,
            )),
            Operator::CallIndirect {
                sig_index,
                table_index,
            } => Some(wasm_encoder::Instruction::CallIndirect {
                ty: sig_index.index() as u32,
                table: table_index.index() as u32,
            }),
            Operator::Return => Some(wasm_encoder::Instruction::Return),
            Operator::Select => Some(wasm_encoder::Instruction::Select),
            Operator::TypedSelect { ty } => Some(wasm_encoder::Instruction::TypedSelect(
                wasm_encoder::ValType::from(*ty),
            )),
            Operator::GlobalGet { global_index } => Some(wasm_encoder::Instruction::GlobalGet(
                global_index.index() as u32,
            )),
            Operator::GlobalSet { global_index } => Some(wasm_encoder::Instruction::GlobalSet(
                global_index.index() as u32,
            )),
            Operator::I32Load { memory } => Some(wasm_encoder::Instruction::I32Load(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load { memory } => Some(wasm_encoder::Instruction::I64Load(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::F32Load { memory } => Some(wasm_encoder::Instruction::F32Load(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::F64Load { memory } => Some(wasm_encoder::Instruction::F64Load(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I32Load8S { memory } => Some(wasm_encoder::Instruction::I32Load8S(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I32Load8U { memory } => Some(wasm_encoder::Instruction::I32Load8U(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I32Load16S { memory } => Some(wasm_encoder::Instruction::I32Load16S(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I32Load16U { memory } => Some(wasm_encoder::Instruction::I32Load16U(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load8S { memory } => Some(wasm_encoder::Instruction::I64Load8S(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load8U { memory } => Some(wasm_encoder::Instruction::I64Load8U(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load16S { memory } => Some(wasm_encoder::Instruction::I64Load16S(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load16U { memory } => Some(wasm_encoder::Instruction::I64Load16U(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load32S { memory } => Some(wasm_encoder::Instruction::I64Load32S(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Load32U { memory } => Some(wasm_encoder::Instruction::I64Load32U(
                wasm_encoder::MemArg::from(*memory),
            )),

            Operator::I32Store { memory } => Some(wasm_encoder::Instruction::I32Store(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Store { memory } => Some(wasm_encoder::Instruction::I64Store(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::F32Store { memory } => Some(wasm_encoder::Instruction::F32Store(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::F64Store { memory } => Some(wasm_encoder::Instruction::F64Store(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I32Store8 { memory } => Some(wasm_encoder::Instruction::I32Store8(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I32Store16 { memory } => Some(wasm_encoder::Instruction::I32Store16(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Store8 { memory } => Some(wasm_encoder::Instruction::I64Store8(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Store16 { memory } => Some(wasm_encoder::Instruction::I64Store16(
                wasm_encoder::MemArg::from(*memory),
            )),
            Operator::I64Store32 { memory } => Some(wasm_encoder::Instruction::I64Store32(
                wasm_encoder::MemArg::from(*memory),
            )),

            Operator::I32Const { value } => {
                Some(wasm_encoder::Instruction::I32Const(*value as i32))
            }
            Operator::I64Const { value } => {
                Some(wasm_encoder::Instruction::I64Const(*value as i64))
            }
            Operator::F32Const { value } => {
                Some(wasm_encoder::Instruction::F32Const(f32::from_bits(*value)))
            }
            Operator::F64Const { value } => {
                Some(wasm_encoder::Instruction::F64Const(f64::from_bits(*value)))
            }

            Operator::I32Eqz => op!(I32Eqz),
            Operator::I32Eq => op!(I32Eq),
            Operator::I32Ne => op!(I32Ne),
            Operator::I32LtS => op!(I32LtS),
            Operator::I32LtU => op!(I32LtU),
            Operator::I32GtS => op!(I32GtS),
            Operator::I32GtU => op!(I32GtU),
            Operator::I32LeS => op!(I32LeS),
            Operator::I32LeU => op!(I32LeU),
            Operator::I32GeS => op!(I32GeS),
            Operator::I32GeU => op!(I32GeU),

            Operator::I64Eqz => op!(I64Eqz),

            Operator::I64Eq => op!(I64Eq),
            Operator::I64Ne => op!(I64Ne),
            Operator::I64LtS => op!(I64LtS),
            Operator::I64LtU => op!(I64LtU),
            Operator::I64GtU => op!(I64GtU),
            Operator::I64GtS => op!(I64GtS),
            Operator::I64LeS => op!(I64LeS),
            Operator::I64LeU => op!(I64LeU),
            Operator::I64GeS => op!(I64GeS),
            Operator::I64GeU => op!(I64GeU),

            Operator::F32Eq => op!(F32Eq),
            Operator::F32Ne => op!(F32Ne),
            Operator::F32Lt => op!(F32Lt),
            Operator::F32Gt => op!(F32Gt),
            Operator::F32Le => op!(F32Le),
            Operator::F32Ge => op!(F32Ge),

            Operator::F64Eq => op!(F64Eq),
            Operator::F64Ne => op!(F64Ne),
            Operator::F64Lt => op!(F64Lt),
            Operator::F64Gt => op!(F64Gt),
            Operator::F64Le => op!(F64Le),
            Operator::F64Ge => op!(F64Ge),

            Operator::I32Clz => op!(I32Clz),
            Operator::I32Ctz => op!(I32Ctz),
            Operator::I32Popcnt => op!(I32Popcnt),

            Operator::I32Add => op!(I32Add),
            Operator::I32Sub => op!(I32Sub),
            Operator::I32Mul => op!(I32Mul),
            Operator::I32DivS => op!(I32DivS),
            Operator::I32DivU => op!(I32DivU),
            Operator::I32RemS => op!(I32RemS),
            Operator::I32RemU => op!(I32RemU),
            Operator::I32And => op!(I32And),
            Operator::I32Or => op!(I32Or),
            Operator::I32Xor => op!(I32Xor),
            Operator::I32Shl => op!(I32Shl),
            Operator::I32ShrS => op!(I32ShrS),
            Operator::I32ShrU => op!(I32ShrU),
            Operator::I32Rotl => op!(I32Rotl),
            Operator::I32Rotr => op!(I32Rotr),

            Operator::I64Clz => op!(I64Clz),
            Operator::I64Ctz => op!(I64Ctz),
            Operator::I64Popcnt => op!(I64Popcnt),

            Operator::I64Add => op!(I64Add),
            Operator::I64Sub => op!(I64Sub),
            Operator::I64Mul => op!(I64Mul),
            Operator::I64DivS => op!(I64DivS),
            Operator::I64DivU => op!(I64DivU),
            Operator::I64RemS => op!(I64RemS),
            Operator::I64RemU => op!(I64RemU),
            Operator::I64And => op!(I64And),
            Operator::I64Or => op!(I64Or),
            Operator::I64Xor => op!(I64Xor),
            Operator::I64Shl => op!(I64Shl),
            Operator::I64ShrS => op!(I64ShrS),
            Operator::I64ShrU => op!(I64ShrU),
            Operator::I64Rotl => op!(I64Rotl),
            Operator::I64Rotr => op!(I64Rotr),

            Operator::F32Abs => op!(F32Abs),
            Operator::F32Neg => op!(F32Neg),
            Operator::F32Ceil => op!(F32Ceil),
            Operator::F32Floor => op!(F32Floor),
            Operator::F32Trunc => op!(F32Trunc),
            Operator::F32Nearest => op!(F32Nearest),
            Operator::F32Sqrt => op!(F32Sqrt),

            Operator::F32Add => op!(F32Add),
            Operator::F32Sub => op!(F32Sub),
            Operator::F32Mul => op!(F32Mul),
            Operator::F32Div => op!(F32Div),
            Operator::F32Min => op!(F32Min),
            Operator::F32Max => op!(F32Max),
            Operator::F32Copysign => op!(F32Copysign),

            Operator::F64Abs => op!(F64Abs),
            Operator::F64Neg => op!(F64Neg),
            Operator::F64Ceil => op!(F64Ceil),
            Operator::F64Floor => op!(F64Floor),
            Operator::F64Trunc => op!(F64Trunc),
            Operator::F64Nearest => op!(F64Nearest),
            Operator::F64Sqrt => op!(F64Sqrt),

            Operator::F64Add => op!(F64Add),
            Operator::F64Sub => op!(F64Sub),
            Operator::F64Mul => op!(F64Mul),
            Operator::F64Div => op!(F64Div),
            Operator::F64Min => op!(F64Min),
            Operator::F64Max => op!(F64Max),
            Operator::F64Copysign => op!(F64Copysign),

            Operator::I32WrapI64 => op!(I32WrapI64),
            Operator::I32TruncF32S => op!(I32TruncF32S),
            Operator::I32TruncF32U => op!(I32TruncF32U),
            Operator::I32TruncF64S => op!(I32TruncF64S),
            Operator::I32TruncF64U => op!(I32TruncF64U),
            Operator::I64ExtendI32S => op!(I64ExtendI32S),
            Operator::I64ExtendI32U => op!(I64ExtendI32U),
            Operator::I64TruncF32S => op!(I64TruncF32S),
            Operator::I64TruncF32U => op!(I64TruncF32U),
            Operator::I64TruncF64S => op!(I64TruncF64S),
            Operator::I64TruncF64U => op!(I64TruncF64U),
            Operator::F32ConvertI32S => op!(F32ConvertI32S),
            Operator::F32ConvertI32U => op!(F32ConvertI32U),
            Operator::F32ConvertI64S => op!(F32ConvertI64S),
            Operator::F32ConvertI64U => op!(F32ConvertI64U),
            Operator::F32DemoteF64 => op!(F32DemoteF64),
            Operator::F64ConvertI32S => op!(F64ConvertI32S),
            Operator::F64ConvertI32U => op!(F64ConvertI32U),
            Operator::F64ConvertI64S => op!(F64ConvertI64S),
            Operator::F64ConvertI64U => op!(F64ConvertI64U),
            Operator::F64PromoteF32 => op!(F64PromoteF32),
            Operator::I32Extend8S => op!(I32Extend8S),
            Operator::I32Extend16S => op!(I32Extend16S),
            Operator::I64Extend8S => op!(I64Extend8S),
            Operator::I64Extend16S => op!(I64Extend16S),
            Operator::I64Extend32S => op!(I64Extend32S),
            Operator::I32TruncSatF32S => op!(I32TruncSatF32S),
            Operator::I32TruncSatF32U => op!(I32TruncSatF32U),
            Operator::I32TruncSatF64S => op!(I32TruncSatF64S),
            Operator::I32TruncSatF64U => op!(I32TruncSatF64U),
            Operator::I64TruncSatF32S => op!(I64TruncSatF32S),
            Operator::I64TruncSatF32U => op!(I64TruncSatF32U),
            Operator::I64TruncSatF64S => op!(I64TruncSatF64S),
            Operator::I64TruncSatF64U => op!(I64TruncSatF64U),
            Operator::F32ReinterpretI32 => op!(F32ReinterpretI32),
            Operator::F64ReinterpretI64 => op!(F64ReinterpretI64),
            Operator::I32ReinterpretF32 => op!(I32ReinterpretF32),
            Operator::I64ReinterpretF64 => op!(I64ReinterpretF64),

            Operator::TableGet { table_index } => Some(wasm_encoder::Instruction::TableGet(
                table_index.index() as u32,
            )),
            Operator::TableSet { table_index } => Some(wasm_encoder::Instruction::TableSet(
                table_index.index() as u32,
            )),
            Operator::TableGrow { table_index } => Some(wasm_encoder::Instruction::TableGrow(
                table_index.index() as u32,
            )),
            Operator::TableSize { table_index } => Some(wasm_encoder::Instruction::TableSize(
                table_index.index() as u32,
            )),
            Operator::MemorySize { mem } => {
                Some(wasm_encoder::Instruction::MemorySize(mem.index() as u32))
            }
            Operator::MemoryGrow { mem } => {
                Some(wasm_encoder::Instruction::MemoryGrow(mem.index() as u32))
            }
        };

        if let Some(inst) = inst {
            func.instruction(&inst);
        }
    }
}

pub fn compile(module: &Module<'_>) -> anyhow::Result<Vec<u8>> {
    let mut into_mod = wasm_encoder::Module::new();

    let mut types = wasm_encoder::TypeSection::new();
    for (_sig, sig_data) in module.signatures() {
        let params = sig_data
            .params
            .iter()
            .map(|&ty| wasm_encoder::ValType::from(ty));
        let returns = sig_data
            .returns
            .iter()
            .map(|&ty| wasm_encoder::ValType::from(ty));
        types.function(params, returns);
    }
    into_mod.section(&types);

    let mut imports = wasm_encoder::ImportSection::new();
    let import_map = module
        .imports()
        .filter_map(|import| match &import.kind {
            &ImportKind::Func(func) => Some((func, (&import.module[..], &import.name[..]))),
            _ => None,
        })
        .collect::<HashMap<Func, (&str, &str)>>();
    let mut num_imports = 0;
    for (i, (func, func_decl)) in module.funcs().enumerate() {
        match func_decl {
            FuncDecl::Import(sig) => {
                let (module_name, func_name) = import_map.get(&func).unwrap_or(&("", ""));
                imports.import(
                    module_name,
                    func_name,
                    wasm_encoder::EntityType::Function(sig.index() as u32),
                );
            }
            FuncDecl::Body(..) => {
                num_imports = i;
                break;
            }
        }
    }
    into_mod.section(&imports);

    let mut funcs = wasm_encoder::FunctionSection::new();
    for (func, func_decl) in module.funcs().skip(num_imports) {
        match func_decl {
            FuncDecl::Import(_) => anyhow::bail!("Import comes after func with body: {}", func),
            FuncDecl::Body(sig, _) => {
                funcs.function(sig.index() as u32);
            }
        }
    }
    into_mod.section(&funcs);

    let mut tables = wasm_encoder::TableSection::new();
    for (_table, table_data) in module.tables() {
        tables.table(wasm_encoder::TableType {
            element_type: wasm_encoder::ValType::from(table_data.ty),
            minimum: table_data
                .func_elements
                .as_ref()
                .map(|elt| elt.len())
                .unwrap_or(0) as u32,
            maximum: table_data.max,
        });
    }
    into_mod.section(&tables);

    let mut memories = wasm_encoder::MemorySection::new();
    for (_mem, mem_data) in module.memories() {
        memories.memory(wasm_encoder::MemoryType {
            minimum: mem_data.initial_pages as u64,
            maximum: mem_data.maximum_pages.map(|val| val as u64),
            memory64: false,
            shared: false,
        });
    }
    into_mod.section(&memories);

    let mut globals = wasm_encoder::GlobalSection::new();
    for (_global, global_data) in module.globals() {
        globals.global(
            wasm_encoder::GlobalType {
                val_type: wasm_encoder::ValType::from(global_data.ty),
                mutable: global_data.mutable,
            },
            &const_init(global_data.ty, global_data.value),
        );
    }
    into_mod.section(&globals);

    let mut exports = wasm_encoder::ExportSection::new();
    for export in module.exports() {
        match &export.kind {
            &ExportKind::Table(table) => {
                exports.export(
                    &export.name[..],
                    wasm_encoder::ExportKind::Table,
                    table.index() as u32,
                );
            }
            &ExportKind::Func(func) => {
                exports.export(
                    &export.name[..],
                    wasm_encoder::ExportKind::Func,
                    func.index() as u32,
                );
            }
            &ExportKind::Memory(mem) => {
                exports.export(
                    &export.name[..],
                    wasm_encoder::ExportKind::Memory,
                    mem.index() as u32,
                );
            }
            &ExportKind::Global(global) => {
                exports.export(
                    &export.name[..],
                    wasm_encoder::ExportKind::Global,
                    global.index() as u32,
                );
            }
        }
    }
    into_mod.section(&exports);

    if let Some(start) = module.start_func {
        let start = wasm_encoder::StartSection {
            function_index: start.index() as u32,
        };
        into_mod.section(&start);
    }

    let mut elem = wasm_encoder::ElementSection::new();
    for (table, table_data) in module.tables() {
        if let Some(elts) = &table_data.func_elements {
            for (i, &elt) in elts.iter().enumerate() {
                if elt.is_valid() {
                    elem.active(
                        Some(table.index() as u32),
                        &wasm_encoder::ConstExpr::i32_const(i as i32),
                        wasm_encoder::ValType::FuncRef,
                        wasm_encoder::Elements::Functions(&[elt.index() as u32]),
                    );
                }
            }
        }
    }
    into_mod.section(&elem);

    let mut code = wasm_encoder::CodeSection::new();
    for (_func, func_decl) in module.funcs().skip(num_imports) {
        let body = func_decl.body().unwrap();
        let body = WasmFuncBackend::new(body)?.compile()?;
        code.function(&body);
    }
    into_mod.section(&code);

    let mut data = wasm_encoder::DataSection::new();
    for (mem, mem_data) in module.memories() {
        for segment in &mem_data.segments {
            data.active(
                mem.index() as u32,
                &wasm_encoder::ConstExpr::i32_const(segment.offset as i32),
                segment.data.iter().copied(),
            );
        }
    }
    into_mod.section(&data);

    Ok(into_mod.finish())
}

fn const_init(ty: Type, value: Option<u64>) -> wasm_encoder::ConstExpr {
    let bits = value.unwrap_or(0);
    match ty {
        Type::I32 => wasm_encoder::ConstExpr::i32_const(bits as u32 as i32),
        Type::I64 => wasm_encoder::ConstExpr::i64_const(bits as i64),
        Type::F32 => wasm_encoder::ConstExpr::f32_const(f32::from_bits(bits as u32)),
        Type::F64 => wasm_encoder::ConstExpr::f64_const(f64::from_bits(bits as u64)),
        _ => unimplemented!(),
    }
}
