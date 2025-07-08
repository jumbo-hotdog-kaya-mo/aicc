#![forbid(unsafe_code)]

mod ail;
mod conf;
mod parser;

use crate::ail::parse;
use crate::conf::Configuration;

use std::{env::args, error::Error, fs::File};

fn main() -> Result<(), Box<dyn Error>> {
    let args = args().collect::<Vec<_>>();
    let usage = format!("usage: {} srcfile outfile", args.get(0).unwrap());
    let filename = args.get(1).ok_or(usage.clone())?;
    let outname = args.get(2).ok_or(usage)?;
    let mut srcfile = File::open(filename)?;
    let mut outfile = File::options().write(true).open(outname)?;

    parse(filename, &mut srcfile)?.write(&mut outfile)?;
    Ok(())
}
