#![forbid(unsafe_code)]

mod ail;
mod conf;
mod parser;

use crate::ail::parse;
use crate::conf::Configuration;

use std::{env::args, error::Error, fs::File};

use png::{BitDepth, ColorType, Encoder, chunk::ChunkType};
use xmltree::{Element, XMLNode};

fn main() -> Result<(), Box<dyn Error>> {
    let args = args().collect::<Vec<_>>();
    let usage = format!("usage: {} [-p] srcfile outfile", args.get(0).unwrap());
    let is_png = args.get(1).as_ref().map(|s| s.as_str()) == Some("-p");
    let filename = args.get(if is_png { 2 } else { 1 }).ok_or(usage.clone())?;
    let outname = args.get(if is_png { 3 } else { 2 }).ok_or(usage)?;
    let mut srcfile = File::open(filename)?;
    let parsed = parse(filename, &mut srcfile)?;

    if is_png {
        for (i, block) in parsed.children.into_iter()
            .filter_map(|node| if let XMLNode::Element(el) = node {
                Some(el)
            } else {
                None
            })
            .filter(|el| &el.name == "block")
            .map(|el| xml!(xml ("xmlns" => "http://www.w3.org/1999/xhtml") [el el])).enumerate() {
            let mut xml = vec![];
            block.write(&mut xml)?;

            let pngname = format!("{outname}{i}.png");
            let pngfile = File::create(pngname)?;
            let mut enc = Encoder::new(pngfile, 1, 1);

            enc.set_color(ColorType::Rgba);
            enc.set_depth(BitDepth::Eight);

            let mut hdr = enc.write_header()?;
            hdr.write_chunk(ChunkType(*b"coDe"), &xml)?;
            hdr.write_image_data(&[255; 4])?;
            hdr.finish()?;
        }
    } else {
        let mut outfile = File::create(outname)?;
        parsed.write(&mut outfile)?;
    }
    Ok(())
}
