use std::mem;
use std::fmt;

use leb128::{ULeb128Ref, ILeb128Ref};

use parsing::{read, parse, parse_str, parse_strs_to_null, parse_array};

// Read the lines program and compute the lines table.

pub fn read_lines(input: &[u8]) -> LinesProgram {
    let header = read_header(input);

    let input = &input[10 + header.header_start.header_length as usize ..];
    let program = read_statements(&header, input);

    LinesProgram {
        header: header,
        program: program,
    }
}

pub fn decode_lines<'a>(mut lines: LinesProgram<'a>) -> LinesTable<'a> {
    let matrix = {
        let mut machine = StateMachine::new(&mut lines.header);
        for s in &lines.program {
            machine.execute(s);
        }
        machine.matrix
    };

    LinesTable {
        header: lines.header,
        lines: matrix,
    }
}

fn read_header(input: &[u8]) -> Header {
    let header_start_size = mem::size_of::<HeaderStart>();
    let header_start = parse::<HeaderStart>(&input[..header_start_size]);
    assert!(header_start.length < 0xfffffff0, "64 bit DWARF or extended DWARF");

    let lengths_len = (header_start.opcode_base - 1) as usize;
    let standard_opcode_lengths =
        parse_array(&input[header_start_size..header_start_size + lengths_len]);
    let include_directories = parse_strs_to_null(&input[header_start_size + lengths_len..]);
    let next_offset = header_start_size + lengths_len + include_directories.iter().fold(0, |a, s| a + s.len() + 1) + 1;
    let file_names = read_file_entries(&input[next_offset..]);

    Header {
        header_start: header_start,
        standard_opcode_lengths: standard_opcode_lengths,
        include_directories: include_directories,
        file_names: file_names,
    }
}

fn read_file_entries<'a>(mut input: &'a [u8]) -> Vec<FileEntry<'a>> {
    let mut result = vec![];
    while let Some((fe, len)) = read_file_entry(input) {
        result.push(fe);
        input = &input[len..]
    }
    result
}

fn read_file_entry<'a>(input: &'a [u8]) -> Option<(FileEntry<'a>, usize)> {
    if input[0] == 0u8 {
        return None;
    }

    let name = parse_str(&input[0]);
    let mut count = name.len() + 1;
    let dir_index = ULeb128Ref::from_bytes(&input[count..]);
    count += dir_index.byte_count();
    let last_modified = ULeb128Ref::from_bytes(&input[count..]);
    count += last_modified.byte_count();
    let size = ULeb128Ref::from_bytes(&input[count..]);
    count += size.byte_count();

    Some((FileEntry {
        name: name,
        dir_index: dir_index.expect_u64(),
        last_modified: last_modified.expect_u64(),
        size: size.expect_u64(),
    }, count))
}

fn read_statements<'a>(header: &Header<'a>, mut input: &'a [u8]) -> Vec<Statement<'a>> {
    let mut result = vec![];
    loop {
        if input.len() == 0 {
            return result;
        }
        let (s, len) = read_statement(header, input);
        result.push(s);
        input = &input[len..]
    }
}

fn read_statement<'a>(header: &Header<'a>, input: &'a [u8]) -> (Statement<'a>, usize) {
    let first_byte = input[0];
    if first_byte == 0 {
        return read_statement_extended(input);
    }
    if first_byte >= header.header_start.opcode_base {
        return (Statement::Special(first_byte - header.header_start.opcode_base), 1);
    }

    match first_byte {
        1 => (Statement::Copy, 1),
        2 => {
            let arg = ULeb128Ref::from_bytes(&input[1..]);
            (Statement::AdvancePc(arg.expect_u32()), 1 + arg.byte_count())
        }
        3 => {
            let arg = ILeb128Ref::from_bytes(&input[1..]);
            (Statement::AdvanceLine(arg.expect_i32()), 1 + arg.byte_count())
        }
        4 => {
            let arg = ULeb128Ref::from_bytes(&input[1..]);
            (Statement::SetFile(arg.expect_u32()), 1 + arg.byte_count())
        }
        5 => {
            let arg = ULeb128Ref::from_bytes(&input[1..]);
            (Statement::SetColumn(arg.expect_u32()), 1 + arg.byte_count())
        }
        6 => (Statement::NegateStmt, 1),
        7 => (Statement::SetBasicBlock, 1),
        8 => (Statement::ConstAddPc, 1),
        9 => (Statement::FixedAdvancePc(read(&input[1..3])), 3),
        10 => (Statement::SetPrologueEnd, 1),
        11 => (Statement::SetEpilogueBeing, 1),
        12 => {
            let arg = ULeb128Ref::from_bytes(&input[1..]);
            (Statement::SetIsa(arg.expect_u32()), 1 + arg.byte_count())
        }
        _ => unreachable!(),
    }
}

// The length component of the returned value should include the first byte
// which indicates that the statement has an extended opcode.
fn read_statement_extended<'a>(input: &'a [u8]) -> (Statement<'a>, usize) {
    let size = ULeb128Ref::from_bytes(&input[1..]);
    let actual_size = size.expect_u64() as usize + 1 + size.byte_count();
    let offset = 1 + size.byte_count();
    let op_code = input[offset];
    let offset = offset + 1;

    match op_code {
        1 => (Statement::EndSequence, actual_size),
        2 => {
            let arg = ULeb128Ref::from_bytes(&input[offset..]);
            (Statement::SetAddress(arg.expect_u64()), actual_size)
        }
        3 => {
            let name = parse_str(&input[offset]);
            let mut count = offset + name.len() + 1;
            let arg1 = ULeb128Ref::from_bytes(&input[count..]);
            count += arg1.byte_count();
            let arg2 = ULeb128Ref::from_bytes(&input[count..]);
            count += arg2.byte_count();
            let arg3 = ULeb128Ref::from_bytes(&input[count..]);
            count += arg3.byte_count();
            assert!(count <= actual_size, "size mismatch");
            (Statement::DefineFile(name,
                                   arg1.expect_u64(),
                                   arg2.expect_u64(),
                                   arg3.expect_u64()), actual_size)
        }
        4 => {
            let arg = ULeb128Ref::from_bytes(&input[offset..]);
            (Statement::SetDiscriminator(arg.expect_u32()), actual_size)
        }
        _ => unreachable!("But found {} {}", op_code, size.expect_u32()),
    }
}

// The fixed-size portion of the header
#[derive(Debug)]
#[repr(packed)]
struct HeaderStart {
    length: u32,
    version: u16,
    header_length: u32,
    minimum_instruction_length: u8,
    // FIXME only present in version 4 or above
    //maximum_operations_per_instruction: u8,
    default_is_stmt: u8,
    line_base: i8,
    line_range: u8,
    opcode_base: u8,
}

#[derive(Debug)]
struct Header<'a> {
    header_start: &'a HeaderStart,
    standard_opcode_lengths: &'a [u8],
    include_directories:  Vec<&'a str>,
    file_names: Vec<FileEntry<'a>>,
}

impl<'a> fmt::Display for Header<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "Lines program header:"));
        try!(self.header_start.fmt(f));
        try!(writeln!(f, "    standard_opcode_lengths:"));
        for l in self.standard_opcode_lengths {
            try!(writeln!(f, "        {}", l));
        }
        try!(writeln!(f, "    include_directories:"));
        for id in &self.include_directories {
            try!(writeln!(f, "        {}", id));
        }
        try!(writeln!(f, "    file_names:"));
        for n in &self.file_names {
            try!(writeln!(f, "        {}, {}, {}, {}", n.name, n.dir_index, n.last_modified, n.size));
        }
        Ok(())
    }
}

impl fmt::Display for HeaderStart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "    length:           {:?}", self.length));
        try!(writeln!(f, "    version:          {:?}", self.version));
        try!(writeln!(f, "    header_length:    {:?}", self.header_length));
        try!(writeln!(f,
                      "    minimum_instruction_length:         {:?}",
                      self.minimum_instruction_length));
        // try!(writeln!(f,
        //               "    maximum_operations_per_instruction: {:?}",
        //               self.maximum_operations_per_instruction));
        try!(writeln!(f, "    default_is_stmt:  {:?}", self.default_is_stmt));
        try!(writeln!(f, "    line_base:        {:?}", self.line_base));
        try!(writeln!(f, "    line_range:       {:?}", self.line_range));
        try!(writeln!(f, "    opcode_base:      {:?}", self.opcode_base));
        Ok(())
    }
}

#[derive(Debug)]
struct FileEntry<'a> {
    name: &'a str,
    dir_index: u64,
    last_modified: u64,
    size: u64,
}

#[derive(Debug)]
pub struct LinesProgram<'a> {
    header: Header<'a>,
    program: Vec<Statement<'a>>,
}

impl<'a> fmt::Display for LinesProgram<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "Lines program:"));
        try!(self.header.fmt(f));
        try!(writeln!(f, "Statements:"));
        for s in &self.program {
            try!(writeln!(f, "    {:?}", s));
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum Statement<'a> {
    Special(u8),
    Copy,
    AdvancePc(u32),
    AdvanceLine(i32),
    SetFile(u32),
    SetColumn(u32),
    NegateStmt,
    SetBasicBlock,
    ConstAddPc,
    FixedAdvancePc(u16),
    SetPrologueEnd,
    SetEpilogueBeing,
    SetIsa(u32),
    EndSequence,
    // FIXME u32 on 32bit targets
    SetAddress(u64),
    DefineFile(&'a str, u64, u64, u64),
    SetDiscriminator(u32),
}

#[derive(Debug)]
pub struct LinesTable<'a> {
    header: Header<'a>,
    // TODO should we keep an efficient way to access lines by address?
    lines: Vec<Line>,
}

impl<'a> fmt::Display for LinesTable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "Lines table:"));
        for l in &self.lines {
            let file_name = self.header.file_names[(l.file - 1) as usize].name;
            try!(writeln!(f, "    0x{:x}: {}:{}", l.address, file_name, l.line));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Line {
    address: u64,
    // FIXME version 4+ only
    //op_index: u8,
    file: u32,
    line: u32,
    column: u32,
    is_stmt: bool,
    basic_block: bool,
    end_sequence: bool,
    prologue_end: bool,
    epilogue_begin: bool,
    isa: u32,
    discriminator: u32,
}

impl Line {
    fn new(is_stmt: bool) -> Line {
        Line {
            address: 0,
            file: 1,
            line: 1,
            column: 0,
            is_stmt: is_stmt,
            basic_block: false,
            end_sequence: false,
            prologue_end: false,
            epilogue_begin: false,
            isa: 0,
            discriminator: 0,            
        }
    }
}

#[derive(Debug)]
struct StateMachine<'a, 'b: 'a> {
    header: &'a mut Header<'b>,
    matrix: Vec<Line>,
    line: Line,
}

impl<'a, 'b: 'a> StateMachine<'a, 'b> {
    fn new(header: &'a mut Header<'b>) -> StateMachine<'a, 'b> {
        let is_stmt = header.header_start.default_is_stmt == 1;
        StateMachine {
            header: header,
            matrix: vec![],
            line: Line::new(is_stmt),
        }
    }

    fn execute(&mut self, stmt: &Statement<'b>) {
        match *stmt {
            Statement::Special(arg) => {
                let op_advance = arg as u64 / self.header.header_start.line_range as u64;
                self.line.line = (self.line.line as i64 +
                                  self.header.header_start.line_base as i64 +
                                  (arg % self.header.header_start.line_range) as i64) as u32;
                self.line.address += self.header.header_start.minimum_instruction_length as u64 *
                                     op_advance;
                self.matrix.push(self.line);
                self.line.discriminator = 0;
                self.line.basic_block = false;
                self.line.prologue_end = false;
                self.line.epilogue_begin = false;
            }
            Statement::Copy => {
                self.matrix.push(self.line);
                self.line.discriminator = 0;
                self.line.basic_block = false;
                self.line.prologue_end = false;
                self.line.epilogue_begin = false;
            }
            Statement::AdvancePc(arg) => {
                self.line.address += self.header.header_start.minimum_instruction_length as u64 *
                                     arg as u64;
            }
            Statement::AdvanceLine(arg) => {
                self.line.line = (self.line.line as i64 + arg as i64) as u32;
            }
            Statement::SetFile(arg) => {
                self.line.file = arg;
            }
            Statement::SetColumn(arg) => {
                self.line.column = arg;
            }
            Statement::NegateStmt => {
                self.line.is_stmt != self.line.is_stmt;
            }
            Statement::SetBasicBlock => {
                self.line.basic_block = true;
            }
            Statement::ConstAddPc => {
                let arg = 255 - self.header.header_start.opcode_base;
                let op_advance = arg as u64 / self.header.header_start.line_range as u64;
                self.line.address += self.header.header_start.minimum_instruction_length as u64 *
                                     op_advance;
            }
            Statement::FixedAdvancePc(arg) => {
                self.line.address += arg as u64;
                //self.op_index = 0;
            }
            Statement::SetPrologueEnd => {
                self.line.prologue_end = true;
            }
            Statement::SetEpilogueBeing => {
                self.line.epilogue_begin = true;
            }
            Statement::SetIsa(arg) => {
                self.line.isa = arg;
            }
            Statement::EndSequence => {
                self.line.end_sequence = true;
                self.matrix.push(self.line);
                self.reset_registers();
            }
            Statement::SetAddress(arg) => {
                self.line.address = arg;
                //self.op_index = 0;
            }
            Statement::DefineFile(name, dir_index, last_modified, size) => {
                self.header.file_names.push(FileEntry{
                    name: name,
                    dir_index: dir_index,
                    last_modified: last_modified,
                    size: size,
                });
            }
            Statement::SetDiscriminator(arg) => {
                self.line.discriminator = arg;
            }
        }
    }

    fn reset_registers(&mut self) {
        self.line = Line::new(self.header.header_start.default_is_stmt == 1);
    }
}
