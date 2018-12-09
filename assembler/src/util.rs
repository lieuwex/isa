use std::num;
use std::result::Result;

pub fn parse_number(s: &str) -> Result<i64, num::ParseIntError> {
    let s2;
    let sign;
    if s.starts_with("-") {
        s2 = &s[1..];
        sign = -1;
    } else {
        s2 = &s[..];
        sign = 1;
    }

    let r = if s2.starts_with("0x") {
        i64::from_str_radix(&s2[2..], 16)
    } else if s2.starts_with("0b") {
        i64::from_str_radix(&s2[2..], 8)
    } else {
        i64::from_str_radix(s2, 10)
    };
    r.map(|n| n * sign)
}
