//! Very basic WASI implementation for interpreter: sufficient to let stdout work.

use crate::interp::{read_u32, write_u32, ConstVal, InterpMemory};
use smallvec::{smallvec, SmallVec};

pub fn call_wasi(
    mem: &mut InterpMemory,
    name: &str,
    args: &[ConstVal],
) -> Option<SmallVec<[ConstVal; 2]>> {
    match name {
        "fd_prestat_get" => {
            Some(smallvec![ConstVal::I32(8)]) // BADF
        }
        "args_sizes_get" => {
            let p_argc = args[0].as_u32().unwrap();
            let p_argv_size = args[1].as_u32().unwrap();
            write_u32(mem, p_argc, 0);
            write_u32(mem, p_argv_size, 0);
            Some(smallvec![ConstVal::I32(0)])
        }
        "environ_sizes_get" => {
            let p_environ_count = args[0].as_u32().unwrap();
            let p_environ_buf_size = args[0].as_u32().unwrap();
            write_u32(mem, p_environ_count, 0);
            write_u32(mem, p_environ_buf_size, 0);
            Some(smallvec![ConstVal::I32(0)])
        }
        "args_get" => Some(smallvec![ConstVal::I32(0)]),
        "fd_fdstat_get" => {
            let fd = args[0].as_u32().unwrap();
            let p_fdstat_t = args[1].as_u32().unwrap();
            if fd == 1 {
                write_u32(mem, p_fdstat_t + 0, 2); // filetype = CHAR
                write_u32(mem, p_fdstat_t + 4, 0); // flags = 0
                write_u32(mem, p_fdstat_t + 8, 0x40); // rights_base = WRITE
                write_u32(mem, p_fdstat_t + 12, 0); // rights_inheriting = 0
                Some(smallvec![ConstVal::I32(0)])
            } else {
                None
            }
        }
        "fd_write" => {
            let fd = args[0].as_u32().unwrap();
            let p_iovs = args[1].as_u32().unwrap();
            let iovs_len = args[2].as_u32().unwrap();
            let p_nwritten = args[3].as_u32().unwrap();
            if fd == 1 {
                let mut written = 0;
                for i in 0..iovs_len {
                    let iov_entry = p_iovs + 8 * i;
                    let base = read_u32(mem, iov_entry) as usize;
                    let len = read_u32(mem, iov_entry + 4) as usize;
                    let data = &mem.data[base..(base + len)];
                    print!("{}", std::str::from_utf8(data).unwrap());
                    written += len;
                }
                write_u32(mem, p_nwritten, written as u32);
                Some(smallvec![ConstVal::I32(0)])
            } else {
                None
            }
        }
        "proc_exit" => {
            eprintln!("WASI exit: {:?}", args[0]);
            None
        }
        "clock_time_get" => {
            let p_time = args[2].as_u32().unwrap();
            write_u32(mem, p_time, 0);
            Some(smallvec![ConstVal::I32(0)])
        }
        "clock_res_get" => {
            let p_res = args[1].as_u32().unwrap();
            write_u32(mem, p_res, 1);
            Some(smallvec![ConstVal::I32(0)])
        }
        _ => None,
    }
}
