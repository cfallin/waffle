//! WAFFLE command-line tool.

use anyhow::Result;
use log::debug;
use std::path::PathBuf;
use structopt::StructOpt;
use waffle::Module;

#[derive(Debug, StructOpt)]
#[structopt(name = "waffle-util", about = "WAFFLE utility.")]
struct Options {
    #[structopt(short, long)]
    debug: bool,

    #[structopt(
        help = "Do basic optimizations: GVN and const-prop",
        long = "basic-opts"
    )]
    basic_opts: bool,

    #[structopt(help = "Transform to maximal SSA", long = "max-ssa")]
    max_ssa: bool,

    #[structopt(subcommand)]
    command: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    #[structopt(name = "print-ir", about = "Parse Wasm and print resulting IR")]
    PrintIR {
        #[structopt(help = "Wasm file to parse")]
        wasm: PathBuf,
    },
    #[structopt(name = "roundtrip", about = "Round-trip Wasm through IR")]
    RoundTrip {
        #[structopt(help = "Wasm file to parse", short = "i")]
        input: PathBuf,
        #[structopt(help = "Wasm file to produce", short = "o")]
        output: PathBuf,
    },
}

fn main() -> Result<()> {
    let opts = Options::from_args();

    let mut logger = env_logger::Builder::from_default_env();
    if opts.debug {
        logger.filter_level(log::LevelFilter::Debug);
    }
    let _ = logger.try_init();

    match opts.command {
        Command::PrintIR { wasm } => {
            let bytes = std::fs::read(wasm)?;
            debug!("Loaded {} bytes of Wasm data", bytes.len());
            let mut module = Module::from_wasm_bytes(&bytes[..])?;
            if opts.basic_opts {
                module.optimize();
            }
            if opts.max_ssa {
                module.convert_to_max_ssa();
            }
            println!("{}", module.display());
        }
        Command::RoundTrip { input, output } => {
            let bytes = std::fs::read(input)?;
            debug!("Loaded {} bytes of Wasm data", bytes.len());
            let mut module = Module::from_wasm_bytes(&bytes[..])?;
            if opts.basic_opts {
                module.optimize();
            }
            if opts.max_ssa {
                module.convert_to_max_ssa();
            }
            let produced = module.to_wasm_bytes()?;
            std::fs::write(output, &produced[..])?;
        }
    }

    Ok(())
}
