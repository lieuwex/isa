use std::{num, result::Result};

pub fn parse_number(s: &str) -> Result<i64, num::ParseIntError> {
    let (s, sign) = if s.starts_with('-') {
        (&s[1..], -1)
    } else {
        (s, 1)
    };

    let (s, radix) = if s.starts_with("0x") {
        (&s[2..], 16)
    } else if s.starts_with("0b") {
        (&s[2..], 2)
    } else {
        (s, 10)
    };

    i64::from_str_radix(s, radix).map(|n| n * sign)
}
