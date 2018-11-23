use std::result::Result;
use std::num;

pub fn parse_number(s: String) -> Result<i64, num::ParseIntError> {
    let skip = |n: usize| -> String {
        s.chars().skip(n).collect()
    };

    if s.starts_with("0x") {
        let s = skip(2);
        i64::from_str_radix(s.as_str(), 16)
    } else if s.starts_with("0b") {
        let s = skip(2);
        i64::from_str_radix(s.as_str(), 8)
    } else {
        i64::from_str_radix(s.as_str(), 10)
    }
}
