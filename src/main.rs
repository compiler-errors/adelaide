#[macro_use]
extern crate maplit;
#[macro_use]
extern crate log;
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate r#proc;
extern crate self as adelaide;

mod ctx;
mod file;
mod read;
mod util;
mod lexer;
mod parser;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use clap::Clap;
use ctx::{AdelaideDatabase, AdelaideContext};
use file::initialize_from_path_arguments;
use util::{AResult, Pretty};

#[derive(Clap)]
#[clap()]
struct Cmd {
    mode: Mode,
    input: Vec<String>,
    #[clap(short, long, parse(from_occurrences))]
    verbose: u8,
}

#[derive(Debug)]
enum Mode {
    Noop,
    Lex,
    Parse,
}

impl std::str::FromStr for Mode {
    type Err = String;

    fn from_str(s: &str) -> Result<Mode, String> {
        match s {
            "noop" | "n" => Ok(Mode::Noop),
            "lex" | "l" => Ok(Mode::Lex),
            "parse" | "p" => Ok(Mode::Parse),
            m => Err(format!("Invalid compiler mode `{}`", m)),
        }
    }
}

fn main() {
    let mut ctx = AdelaideDatabase::default();

    if let Err(err) = try_main(&mut ctx) {
        let diagnostic = err.into();
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        codespan_reporting::term::emit(&mut writer.lock(), &config, &ctx, &diagnostic).unwrap();
    }
}

fn try_main(ctx: &mut AdelaideDatabase) -> AResult<()> {
    let Cmd {
        mode,
        input,
        verbose,
    } = Cmd::parse();
    init_logging(verbose);
    initialize_from_path_arguments(ctx, input)?;
    let root = ctx.mod_tree_root();

    match mode {
        Mode::Noop => println!("{:#?}", Pretty(root, ctx)),
        Mode::Lex => {
            ctx.lex_mod(root)?;
        },
        Mode::Parse => {
            let m = ctx.parse_mod(root)?;
            println!("{:#?}", Pretty(m, ctx))
        },
    }

    Ok(())
}

fn init_logging(verbose: u8) {
    let mut builder =
        env_logger::from_env(env_logger::Env::default().default_filter_or(match verbose {
            0 => "warning",
            1 => "info",
            2 => "debug",
            _ => "trace",
        }));

    builder.filter(Some("salsa"), log::LevelFilter::Warn);
    builder.init();
}

