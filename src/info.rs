// Reading the debug info entries

// TODO kinda busted because we don't take into account relocations

use std::collections::HashMap;
use std::fmt;
use std::mem;

use abbrev::{AbbrevTable, AbbrevEntry, Children};
use {AttrData, Attribute, read_unsigned_leb128};

use zero::{read, Pod};

pub fn read_info<'a>(mut input: &'a [u8], abbrev_table: &'a AbbrevTable) -> Vec<Unit<'a>> {
    let mut result = vec![];

    while input.len() > 0 {
        let header = read_header(input);
        let header_size = mem::size_of::<Header>();
        let end = header.unit_length as usize + mem::size_of_val(&header.unit_length);
        println!("{} {}", header_size, end);
        let (dies, offsets) = read_dies(&input[header_size..end], header, abbrev_table);

        let unit = Unit {
            header: header,
            dies: dies,
            offsets: offsets,
        };
        result.push(unit);

        input = &input[end..];
    }

    result
}

fn read_header<'a>(input: &'a [u8]) -> &'a Header {
    let header = read::<Header>(input);
    assert!(header.unit_length < 0xfffffff0, "64 bit DWARF or extended DWARF");
    header
}

fn read_dies<'a>(input: &'a [u8], header: &'a Header, abbrev_table: &'a AbbrevTable) -> (Vec<Option<Die<'a>>>, HashMap<usize, usize>) {
    let abbrev_slice = &abbrev_table.0[header.debug_abbrev_offset as usize..];
    let mut result = vec![];
    let mut offsets = HashMap::new();
    let mut offset = 0;

    let len = input.len();

    while len - offset > 0 {
        offsets.insert(offset, result.len());
        let (die, size) = read_die(&input[offset..], abbrev_slice, result.len());
        result.push(die);
        offset += size;
    }

    (result, offsets)
}

pub fn read_die<'a>(input: &'a [u8], abbrev_table: &'a [AbbrevEntry], index: usize) -> (Option<Die<'a>>, usize) {
    let (abbrev_number, ab_size) = read_unsigned_leb128(input);
    let abbrev_number = abbrev_number.expect_u64();
    if abbrev_number == 0 {
        return (None, ab_size);
    }

    let abbrev = &abbrev_table[abbrev_number as usize];

    let mut data = HashMap::new();
    let mut size = ab_size;

    for &(attr, form) in &abbrev.attributes {
        let (datum, attr_size) = AttrData::from_data(&input[size..], form);
        size += attr_size;
        data.insert(attr, datum);
    }

    (Some(Die {
        index: index,
        abbrev_number: abbrev_number,
        abbrev: abbrev,
        data: data,
    }), size)
}

#[derive(Debug)]
#[repr(packed)]
pub struct Header {
    unit_length: u32,
    version: u16,
    debug_abbrev_offset: u32,
    address_size: u8,
}

unsafe impl Pod for Header {}

impl<'a> fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "unit length:         {}", self.unit_length));
        try!(writeln!(f, "version:             {}", self.version));
        try!(writeln!(f, "debug abbrev offset: {}", self.debug_abbrev_offset));
        try!(writeln!(f, "address size:        {}", self.address_size));
        Ok(())
    }
}

// The DIEs for a single compilation unit.
#[derive(Debug)]
pub struct Unit<'a> {
    pub header: &'a Header,
    pub dies: Vec<Option<Die<'a>>>,
    // Map offsets to indices.
    pub offsets: HashMap<usize, usize>,
}

impl<'a> Unit<'a> {
    pub fn die_at_offset(&self, offset: usize) -> Option<&Die<'a>> {
        assert!(self.offsets.contains_key(&offset));
        self.dies[self.offsets[&offset]].as_ref()
    }
}

impl<'a> fmt::Display for Unit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(self.header.fmt(f));
        try!(writeln!(f, ""));
        for (i, die) in self.dies.iter().enumerate() {
            match *die {
                None => {
                    try!(writeln!(f, "<{}>: abbreviation 0", i));
                }
                Some(ref die) => {
                    try!(die.fmt(f));
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Die<'a> {
    index: usize,
    abbrev_number: u64,
    abbrev: &'a AbbrevEntry,
    data: HashMap<Attribute, AttrData<'a>>,
}

impl<'a> Die<'a> {
    pub fn children<'b>(&self, unit: &'b Unit<'a>) -> ChildIterator<'b, 'a>
        where 'a: 'b
    {
        if self.abbrev.children == Children::No {
            ChildIterator { children: &[] }
        } else {
            ChildIterator { children: &unit.dies[self.index..] }
        }
    }
}

impl<'a> fmt::Display for Die<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ab = self.abbrev;
        try!(writeln!(f, "<{}>: abbreviation {} ({:?})", self.index, self.abbrev_number, ab.tag));
        for &(ref attr, form) in &ab.attributes {
            try!(writeln!(f, "    {:?}: {:?} ({:?})", attr, self.data[attr], form));
        }
        Ok(())
    }
}

// Just exists to make testing easier.
pub trait HasChildren {
    fn has_children(&self) -> Children;
}

impl<'a> HasChildren for Die<'a> {
    fn has_children(&self) -> Children {
        self.abbrev.children
    }
}

pub struct ChildIterator<'b, 'a: 'b> {
    children: &'b [Option<Die<'a>>]
}

impl<'b, 'a: 'b> Iterator for ChildIterator<'b, 'a> {
    type Item = &'b Die<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.children.len() == 0 {
            return None;
        }

        let result = self.children[0].as_ref();
        if result.is_some() {
            self.children = next_sibling(self.children);
        }

        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.children.len()))
    }
}

fn next_sibling<T: HasChildren>(children: &[Option<T>]) -> &[Option<T>] {
    assert!(children.len() > 0 && children[0].is_some());

    match children[0].as_ref().unwrap().has_children() {
        Children::Yes => {
            let mut result = &children[1..];
            loop {
                if result.len() == 0 {
                    return result;
                }

                if result[0].is_none() {
                    return &result[1..];
                }

                result = next_sibling(result);
            }
        }
        Children::No => &children[1..],
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::next_sibling;
    use abbrev::Children;

    impl HasChildren for Children {
        fn has_children(&self) -> Children {
            *self
        }        
    }

    #[test]
    fn test_next_sibling() {
        use abbrev::Children::*;

        let input = &[Some(No)];
        assert!(next_sibling(input) == &[]);

        let input = &[Some(No), Some(No)];
        assert!(next_sibling(input) == &input[1..]);

        let input = &[Some(No), Some(Yes), Some(No), None];
        assert!(next_sibling(input) == &input[1..]);

        let input = &[Some(Yes), None, Some(No)];
        assert!(next_sibling(input) == &input[2..]);

        let input = &[Some(Yes), Some(No), Some(No), None, Some(No)];
        assert!(next_sibling(input) == &input[4..]);

        let input = &[Some(Yes), Some(Yes), Some(No), None, Some(No), None, Some(No)];
        assert!(next_sibling(input) == &input[6..]);

        let input = &[Some(Yes), Some(No), Some(Yes), Some(No), None, None, Some(No)];
        assert!(next_sibling(input) == &input[6..]);
    }
}
