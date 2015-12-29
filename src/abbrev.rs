// Handle the abbreviation table

use leb128::ULeb128Ref;

use std::fmt;
use std::mem;

use {Tag, Attribute, Form};

pub fn read_table(input: &[u8]) -> AbbrevTable {
    let mut result = vec![];
    let mut count = 0;

    loop {
        let (entry, len) = read_abbrev_entry(&input[count..]);
        result.push(entry);
        count += len;
        if input[count] == 0 {
            return AbbrevTable(result);
        }
        assert!(count < input.len(), "Overran abbreviation table");
    }
}

fn read_abbrev_entry(input: &[u8]) -> (AbbrevEntry, usize) {
    let (code_leb, code_len) = read_unsigned_leb128(input);
    let mut count = code_len;
    let (tag_leb, tag_len) = read_unsigned_leb128(&input[count..]);
    count += tag_len;
    let child_byte = input[count];
    count += 1;
    let (attr_lebs, attr_len) = read_pairs_to_null(&input[count..]);
    count += attr_len;

    (AbbrevEntry {
        code: code_leb.expect_u64(),
        tag: Tag::from_u16(tag_leb.expect_u16()),
        children: Children::from_u8(child_byte),
        attributes: attr_lebs.into_iter().map(|(a, f)| {
            (Attribute::from_u16(a.expect_u16()), Form::from_u8(f.expect_u8()))
        }).collect(),
    }, count)
}


fn read_pairs_to_null(input: &[u8]) -> (Vec<(ULeb128Ref, ULeb128Ref)>, usize) {
    static NULL_BYTES: [u8; 1] = [0];

    let null = ULeb128Ref::from_bytes(&NULL_BYTES);
    let mut result = vec![];
    let mut count = 0;
    loop {
        let (u1, u1_len) = read_unsigned_leb128(&input[count..]);
        let (u2, u2_len) = read_unsigned_leb128(&input[count + u1_len..]);
        count += u1_len + u2_len;
        if u1 == null && u2 == null {
            return (result, count);
        }
        result.push((u1, u2));
    }
}

fn read_unsigned_leb128(input: &[u8]) -> (ULeb128Ref, usize) {
    let result = ULeb128Ref::from_bytes(input);
    (result, result.byte_count())
}

#[derive(Debug)]
pub struct AbbrevTable(Vec<AbbrevEntry>);

#[derive(Debug)]
pub struct AbbrevEntry {
    code: u64,
    tag: Tag,
    children: Children,
    attributes: Vec<(Attribute, Form)>,
}

impl fmt::Display for AbbrevEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "{}    {:?} [children: {:?}]", self.code, self.tag, self.children));
        for a in &self.attributes {
            try!(writeln!(f, "    {:?}    {:?}", a.0, a.1))
        }

        Ok(())
    }
}

impl fmt::Display for AbbrevTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Display;

        for ae in &self.0 {
            try!(ae.fmt(f));
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum Children {
    No = 0,
    Yes = 1,
}

impl Children {
    pub fn as_u8(self) -> u8 {
        unsafe { mem::transmute(self) }
    }

    pub fn from_u8(input: u8) -> Children {
        assert!(input <= 1, "Invalid children: {}", input);
        unsafe { mem::transmute(input) }
    }
}
