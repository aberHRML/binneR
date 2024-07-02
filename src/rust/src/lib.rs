use extendr_api::{prelude::*, IntoRobj};
use flate2::read::GzDecoder;
use mzdata::{
    io::MzMLReader,
    params::{ParamDescribed, ParamLike},
    prelude::{ByteArrayView, SpectrumLike},
    spectrum::{ArrayType, RefPeakDataLevel},
};
use std::fs::File;
use std::io::{BufReader, Read, Seek};

#[derive(Debug, IntoDataFrameRow)]
struct Header {
    scan: usize,
    polarity: String,
    scan_filter: String,
}

#[extendr]
fn parse_mzml(path: &str) -> List {
    let mut file = File::open(path).unwrap();
    let decoder = GzDecoder::new(BufReader::new(&mut file));

    let reader: Box<dyn Read> = match decoder.header() {
        Some(_) => Box::new(decoder),
        None => {
            let _ = file.rewind();
            Box::new(file)
        }
    };

    let mzml_reader = MzMLReader::new(reader);
    let mut header_data: Vec<Header> = Vec::new();
    let mut peak_data: Vec<Robj> = Vec::new();

    for scan in mzml_reader {
        let peaks = scan.peaks();

        let mz = match peaks {
            RefPeakDataLevel::RawData(arrays) => {
                let array = &arrays.byte_buffer_map[&ArrayType::MZArray];
                array.to_f64().unwrap()
            }
            _ => todo!(),
        }
        .to_vec();

        let intensities = match peaks {
            RefPeakDataLevel::RawData(arrays) => {
                let array = &arrays.byte_buffer_map[&ArrayType::IntensityArray];
                array.to_f32().unwrap()
            }
            _ => todo!(),
        }
        .to_vec();

        peak_data.push(data_frame!(mz = mz, intensity = intensities));

        let header = scan.description();
        header_data.push(Header {
            scan: header.index + 1,
            polarity: header.polarity.to_string(),
            scan_filter: header.acquisition.scans[0]
                .get_param_by_name("filter string")
                .unwrap()
                .value()
                .to_string(),
        });
    }
    list!(
        header = header_data.into_dataframe().unwrap(),
        peaks = peak_data.into_robj()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_mzml() {
        test! {
            let mzml = parse_mzml("../../inst/example-data/1.mzML.gz");
            assert_eq!(mzml.dollar("header").unwrap().inherits("data.frame"),true);
            assert_eq!(mzml.dollar("peaks").unwrap().len(), 156);
        }
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod binneR;
    fn parse_mzml;
}
