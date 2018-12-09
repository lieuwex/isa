use std::num;
use std::result::Result;

pub fn parse_number(s: &str) -> Result<i64, num::ParseIntError> {
    let (s, sign) = if s.starts_with('-') {
        (&s[1..], -1)
    } else {
        (s, 1)
    };

    let n = if s.starts_with("0x") {
        i64::from_str_radix(&s[2..], 16)
    } else if s.starts_with("0b") {
        i64::from_str_radix(&s[2..], 8)
    } else {
        i64::from_str_radix(s, 10)
    };

    n.map(|n| n * sign)
}
