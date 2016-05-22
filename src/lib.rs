extern crate xmas_elf;
extern crate leb128;
extern crate zero;

pub mod lines;
pub mod abbrev;
pub mod info;

use xmas_elf::ElfFile;
use leb128::{ULeb128, ILeb128};
use zero::{read, read_str};
use abbrev::{AbbrevTable, Children};
use lines::LinesTable;
use info::{Unit, HasChildren};

use std::mem;

pub struct DwarfFile<'a> {
    pub abbrev: Box<AbbrevTable>,
    pub lines: LinesTable<'a>,
    pub info: Vec<Unit<'a>>,
    pub data: &'a ElfFile<'a>,
}

impl<'a> DwarfFile<'a> {
    pub fn new(input: &'a ElfFile<'a>) -> DwarfFile<'a> {
        let abbrev = raw_data(input, SectionName::Abbrev);
        let abbrev = Box::new(abbrev::read_table(abbrev));
        let lines = raw_data(input, SectionName::Line);
        let lines = lines::decode_lines(lines::read_lines(lines));
        
        let info = raw_data(input, SectionName::Info);
        // This transmute dance is because we can't persuade the borrow checker
        // that result.abbrev will live for 'a. It will because it is owned by
        // result. I guess be careful.
        let info = info::read_info(info, unsafe { mem::transmute(&*abbrev) });

        DwarfFile {
            abbrev: abbrev,
            lines: lines,
            info: info,
            data: input,
        }
    }
}

fn raw_data<'a>(elf_file: &'a ElfFile<'a>, name: SectionName) -> &'a [u8] {
    use xmas_elf::sections::SectionData;

    let sect = elf_file.find_section_by_name(name.as_str()).unwrap();
    if let SectionData::Undefined(data) = sect.get_data(&elf_file) {
        data
    } else {
        unreachable!()
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum SectionName {
    // Abbreviations used in the .debug_info section.
    Abbrev,
    // Lookup table for mapping addresses to compilation units.
    Aranges,
    // Call frame information.
    Frame,
    // The core DWARF information section.
    Info,
    // Line number information.
    Line,
    // Location lists used in DW_AT_location attributes.
    Loc,
    // Macro information.
    MacInfo,
    // Lookup table for mapping object and function names to compilation units.
    PubNames,
    // Lookup table for mapping type names to compilation units.
    PubTypes,
    // Address ranges used in DW_AT_ranges attributes.
    Ranges,
    // String table used in .debug_info.
    Str,
}

impl SectionName {
    fn as_str(self) -> &'static str {
        match self {
            SectionName::Abbrev => ".debug_abbrev",
            SectionName::Aranges => ".debug_aranges",
            SectionName::Frame => ".debug_frame",
            SectionName::Info => ".debug_info",
            SectionName::Line => ".debug_line",
            SectionName::Loc => ".debug_loc",
            SectionName::MacInfo => ".debug_macinfo",
            SectionName::PubNames => ".debug_pubnames",
            SectionName::PubTypes => ".debug_pubtypes",
            SectionName::Ranges => ".debug_ranges",
            SectionName::Str => ".debug_str",
        }
    }
}


#[derive(Debug, Clone)]
pub enum Tag {
    None,
    ArrayType,
    ClassType,
    EntryPoint,
    EnumerationType,
    FormalParameter,
    ImportedDeclaration,
    Label,
    LexicalBlock,
    Member,
    PointerType,
    ReferenceType,
    CompileUnit,
    StringType,
    StructureType,
    SubroutineType,
    Typedef,
    UnionType,
    UnspecifiedParameters,
    Variant,
    CommonBlock,
    CommonInclusion,
    Inheritance,
    InlinedSubroutine,
    Module,
    PtrToMemberType,
    SetType,
    SubrangeType,
    WithStmt,
    AccessDeclaration,
    BaseType,
    CatchBlock,
    ConstType,
    Constant,
    Enumerator,
    FileType,
    Friend,
    Namelist,
    NamelistItem,
    PackedType,
    Subprogram,
    TemplateTypeParameter,
    TemplateValueParameter,
    ThrownType,
    TryBlock,
    VariantPart,
    Variable,
    VolatileType,
    DwarfProcedure,
    RestrictType,
    InterfaceType,
    Namespace,
    ImportedModule,
    UnspecifiedType,
    PartialUnit,
    ImportedUnit,
    Condition,
    SharedType,
    TypeUnit,
    RvalueReferenceType,
    TemplateAlias,
    User(u16),
}

impl Tag {
    pub fn as_u16(&self) -> u16 {
        match *self {
            Tag::None => 0,
            Tag::ArrayType => 0x01,
            Tag::ClassType => 0x02,
            Tag::EntryPoint => 0x03,
            Tag::EnumerationType => 0x04,
            Tag::FormalParameter => 0x05,
            Tag::ImportedDeclaration => 0x08,
            Tag::Label => 0x0a,
            Tag::LexicalBlock => 0x0b,
            Tag::Member => 0x0d,
            Tag::PointerType => 0x0f,
            Tag::ReferenceType => 0x10,
            Tag::CompileUnit => 0x11,
            Tag::StringType => 0x12,
            Tag::StructureType => 0x13,
            Tag::SubroutineType => 0x15,
            Tag::Typedef => 0x16,
            Tag::UnionType => 0x17,
            Tag::UnspecifiedParameters => 0x18,
            Tag::Variant => 0x19,
            Tag::CommonBlock => 0x1a,
            Tag::CommonInclusion => 0x1b,
            Tag::Inheritance => 0x1c,
            Tag::InlinedSubroutine => 0x1d,
            Tag::Module => 0x1e,
            Tag::PtrToMemberType => 0x1f,
            Tag::SetType => 0x20,
            Tag::SubrangeType => 0x21,
            Tag::WithStmt => 0x22,
            Tag::AccessDeclaration => 0x23,
            Tag::BaseType => 0x24,
            Tag::CatchBlock => 0x25,
            Tag::ConstType => 0x26,
            Tag::Constant => 0x27,
            Tag::Enumerator => 0x28,
            Tag::FileType => 0x29,
            Tag::Friend => 0x2a,
            Tag::Namelist => 0x2b,
            Tag::NamelistItem => 0x2c,
            Tag::PackedType => 0x2d,
            Tag::Subprogram => 0x2e,
            Tag::TemplateTypeParameter => 0x2f,
            Tag::TemplateValueParameter => 0x30,
            Tag::ThrownType => 0x31,
            Tag::TryBlock => 0x32,
            Tag::VariantPart => 0x33,
            Tag::Variable => 0x34,
            Tag::VolatileType => 0x35,
            Tag::DwarfProcedure => 0x36,
            Tag::RestrictType => 0x37,
            Tag::InterfaceType => 0x38,
            Tag::Namespace => 0x39,
            Tag::ImportedModule => 0x3a,
            Tag::UnspecifiedType => 0x3b,
            Tag::PartialUnit => 0x3c,
            Tag::ImportedUnit => 0x3d,
            Tag::Condition => 0x3f,
            Tag::SharedType => 0x40,
            Tag::TypeUnit => 0x41,
            Tag::RvalueReferenceType => 0x42,
            Tag::TemplateAlias => 0x43,
            Tag::User(x) => x,
        }
    }

    pub fn from_u16(input: u16) -> Tag {
        match input {
            0x0 => Tag::None,
            0x01 => Tag::ArrayType,
            0x02 => Tag::ClassType,
            0x03 => Tag::EntryPoint,
            0x04 => Tag::EnumerationType,
            0x05 => Tag::FormalParameter,
            0x08 => Tag::ImportedDeclaration,
            0x0a => Tag::Label,
            0x0b => Tag::LexicalBlock,
            0x0d => Tag::Member,
            0x0f => Tag::PointerType,
            0x10 => Tag::ReferenceType,
            0x11 => Tag::CompileUnit,
            0x12 => Tag::StringType,
            0x13 => Tag::StructureType,
            0x15 => Tag::SubroutineType,
            0x16 => Tag::Typedef,
            0x17 => Tag::UnionType,
            0x18 => Tag::UnspecifiedParameters,
            0x19 => Tag::Variant,
            0x1a => Tag::CommonBlock,
            0x1b => Tag::CommonInclusion,
            0x1c => Tag::Inheritance,
            0x1d => Tag::InlinedSubroutine,
            0x1e => Tag::Module,
            0x1f => Tag::PtrToMemberType,
            0x20 => Tag::SetType,
            0x21 => Tag::SubrangeType,
            0x22 => Tag::WithStmt,
            0x23 => Tag::AccessDeclaration,
            0x24 => Tag::BaseType,
            0x25 => Tag::CatchBlock,
            0x26 => Tag::ConstType,
            0x27 => Tag::Constant,
            0x28 => Tag::Enumerator,
            0x29 => Tag::FileType,
            0x2a => Tag::Friend,
            0x2b => Tag::Namelist,
            0x2c => Tag::NamelistItem,
            0x2d => Tag::PackedType,
            0x2e => Tag::Subprogram,
            0x2f => Tag::TemplateTypeParameter,
            0x30 => Tag::TemplateValueParameter,
            0x31 => Tag::ThrownType,
            0x32 => Tag::TryBlock,
            0x33 => Tag::VariantPart,
            0x34 => Tag::Variable,
            0x35 => Tag::VolatileType,
            0x36 => Tag::DwarfProcedure,
            0x37 => Tag::RestrictType,
            0x38 => Tag::InterfaceType,
            0x39 => Tag::Namespace,
            0x3a => Tag::ImportedModule,
            0x3b => Tag::UnspecifiedType,
            0x3c => Tag::PartialUnit,
            0x3d => Tag::ImportedUnit,
            0x3f => Tag::Condition,
            0x40 => Tag::SharedType,
            0x41 => Tag::TypeUnit,
            0x42 => Tag::RvalueReferenceType,
            0x43 => Tag::TemplateAlias,
            x if x >= 0x4080 => Tag::User(x),
            x  => panic!("Invalid tag: {}", x),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum Form {
    Addr = 0x01,
    Block2 = 0x03,
    Block4 = 0x04,
    Data2 = 0x05,
    Data4 = 0x06,
    Data8 = 0x07,
    String = 0x08,
    Block = 0x09,
    Block1 = 0x0a,
    Data1 = 0x0b,
    Flag = 0x0c,
    Sdata = 0x0d,
    Strp = 0x0e,
    Udata = 0x0f,
    RefAddr = 0x10,
    Ref1 = 0x11,
    Ref2 = 0x12,
    Ref4 = 0x13,
    Ref8 = 0x14,
    RefUdata = 0x15,
    Indirect = 0x16,
    SecOffset = 0x17,
    Exprloc = 0x18,
    // FIXME DWARF v4 stuff
    FlagPresent = 0x19,
    // RefSig8 = 0x20,
}

impl Form {
    pub fn as_u8(self) -> u8 {
        unsafe { mem::transmute(self) }
    }

    pub fn from_u8(input: u8) -> Form {
        //assert!((input <= 0x19 && input != 2) || input == 0x20, "Invalid form: {}", input);
        assert!(input <= 0x19 && input != 2, "Invalid form: {}", input);
        unsafe { mem::transmute(input) }
    }
}

// Data stored for an attribute, the 'type' corresponds with the form specified in
// the abbreviation.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AttrData<'a> {
    // Assumes target is 64 bit.
    Addr(u64),
    Block(&'a [u8]),
    // FIXME If the target machine is big-endian, the data forms will be broken.
    Data1(u8),
    Data2(u16),
    Data4(u32),
    Data8(u64),
    Udata(ULeb128<'a>),
    Sdata(ILeb128<'a>),
    String(&'a str),
    FlagPresent,
    // RefSig8(u64),
}

impl<'a> AttrData<'a> {
    // Returns the parsed data and the size in input.
    fn from_data(input: &'a [u8], form: Form) -> (AttrData, usize) {
        match form {
            Form::Addr => (AttrData::Addr(*read(input)), 8),
            Form::Block | Form::Exprloc => {
                let (len, size) = read_unsigned_leb128(input);
                let len = len.expect_usize();
                (AttrData::Block(&input[size..size + len]), size + len)
            },
            Form::Block1 => {
                let len = *read::<u8>(input) as usize;
                (AttrData::Block(&input[1..len + 1]), len + 1)
            },
            Form::Block2 => {
                let len = *read::<u16>(input) as usize;
                (AttrData::Block(&input[2..len + 2]), len + 2)
            },
            Form::Block4 => {
                let len = *read::<u32>(input) as usize;
                (AttrData::Block(&input[4..len + 4]), len + 4)
            },
            Form::Data1 | Form::Ref1 | Form::Flag => (AttrData::Data1(*read(input)), 1),
            Form::Data2 | Form::Ref2 => (AttrData::Data2(*read(input)), 2),
            Form::Data4 | Form::Ref4 | Form::RefAddr | Form::Strp | Form::SecOffset => (AttrData::Data4(*read(input)), 4),
            Form::Data8 | Form::Ref8 => (AttrData::Data8(*read(input)), 8),
            Form::Sdata => {
                let (len, size) = read_signed_leb128(input);
                (AttrData::Sdata(len), size)
            },
            Form::Udata | Form::RefUdata => {
                let (len, size) = read_unsigned_leb128(input);
                (AttrData::Udata(len), size)
            },
            Form::String => {
                let s = read_str(input);
                (AttrData::String(s), s.len() + 1)
            },
            Form::FlagPresent => (AttrData::FlagPresent, 0),
            // TODO
            Form::Indirect => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Attribute {
    Sibling,
    Location,
    Name,
    Ordering,
    ByteSize,
    BitOffset,
    BitSize,
    StmtList,
    LowPc,
    HighPc,
    Language,
    Discr,
    DiscrValue,
    Visibility,
    Import,
    StringLength,
    CommonReference,
    CompDir,
    ConstValue,
    ContainingType,
    DefaultValue,
    Inline,
    IsOptional,
    LowerBound,
    Producer,
    Prototyped,
    ReturnAddr,
    StartScope,
    BitStride,
    UpperBound,
    AbstractOrigin,
    Accessibility,
    AddressClass,
    Artificial,
    BaseTypes,
    CallingConvention,
    Count,
    DataMemberLocation,
    DeclColumn,
    DeclFile,
    DeclLine,
    Declaration,
    DiscrList,
    Encoding,
    External,
    FrameBase,
    Friend,
    IdentifierCase,
    MacroInfo,
    NamelistItem,
    Priority,
    Segment,
    Specification,
    StaticLink,
    Type,
    UseLocation,
    VariableParameter,
    Virtuality,
    VtableElemLocation,
    Allocated,
    Associated,
    DataLocation,
    ByteStride,
    EntryPc,
    UseUtf8,
    Extension,
    Ranges,
    Trampoline,
    CallColumn,
    CallFile,
    CallLine,
    Description,
    BinaryScale,
    DecimalScale,
    Small,
    DecimalSign,
    DigitCount,
    PictureString,
    Mutable,
    ThreadsScaled,
    Explicit,
    ObjectPointer,
    Endianity,
    Elemental,
    Pure,
    Recursive,
    Signature,
    MainSubprogram,
    DataBitOffset,
    ConstExpr,
    EnumClass,
    LinkageName,
    User(u16),
}

impl Attribute {
    pub fn as_u16(&self) -> u16 {
        match *self {
            Attribute::Sibling => 0x01,
            Attribute::Location => 0x02,
            Attribute::Name => 0x03,
            Attribute::Ordering => 0x09,
            Attribute::ByteSize => 0x0b,
            Attribute::BitOffset => 0x0c,
            Attribute::BitSize => 0x0d,
            Attribute::StmtList => 0x10,
            Attribute::LowPc => 0x11,
            Attribute::HighPc => 0x12,
            Attribute::Language => 0x13,
            Attribute::Discr => 0x15,
            Attribute::DiscrValue => 0x16,
            Attribute::Visibility => 0x17,
            Attribute::Import => 0x18,
            Attribute::StringLength => 0x19,
            Attribute::CommonReference => 0x1a,
            Attribute::CompDir => 0x1b,
            Attribute::ConstValue => 0x1c,
            Attribute::ContainingType => 0x1d,
            Attribute::DefaultValue => 0x1e,
            Attribute::Inline => 0x20,
            Attribute::IsOptional => 0x21,
            Attribute::LowerBound => 0x22,
            Attribute::Producer => 0x25,
            Attribute::Prototyped => 0x27,
            Attribute::ReturnAddr => 0x2a,
            Attribute::StartScope => 0x2c,
            Attribute::BitStride => 0x2e,
            Attribute::UpperBound => 0x2f,
            Attribute::AbstractOrigin => 0x31,
            Attribute::Accessibility => 0x32,
            Attribute::AddressClass => 0x33,
            Attribute::Artificial => 0x34,
            Attribute::BaseTypes => 0x35,
            Attribute::CallingConvention => 0x36,
            Attribute::Count => 0x37,
            Attribute::DataMemberLocation => 0x38,
            Attribute::DeclColumn => 0x39,
            Attribute::DeclFile => 0x3a,
            Attribute::DeclLine => 0x3b,
            Attribute::Declaration => 0x3c,
            Attribute::DiscrList => 0x3d,
            Attribute::Encoding => 0x3e,
            Attribute::External => 0x3f,
            Attribute::FrameBase => 0x40,
            Attribute::Friend => 0x41,
            Attribute::IdentifierCase => 0x42,
            Attribute::MacroInfo => 0x43,
            Attribute::NamelistItem => 0x44,
            Attribute::Priority => 0x45,
            Attribute::Segment => 0x46,
            Attribute::Specification => 0x47,
            Attribute::StaticLink => 0x48,
            Attribute::Type => 0x49,
            Attribute::UseLocation => 0x4a,
            Attribute::VariableParameter => 0x4b,
            Attribute::Virtuality => 0x4c,
            Attribute::VtableElemLocation => 0x4d,
            Attribute::Allocated => 0x4e,
            Attribute::Associated => 0x4f,
            Attribute::DataLocation => 0x50,
            Attribute::ByteStride => 0x51,
            Attribute::EntryPc => 0x52,
            Attribute::UseUtf8 => 0x53,
            Attribute::Extension => 0x54,
            Attribute::Ranges => 0x55,
            Attribute::Trampoline => 0x56,
            Attribute::CallColumn => 0x57,
            Attribute::CallFile => 0x58,
            Attribute::CallLine => 0x59,
            Attribute::Description => 0x5a,
            Attribute::BinaryScale => 0x5b,
            Attribute::DecimalScale => 0x5c,
            Attribute::Small => 0x5d,
            Attribute::DecimalSign => 0x5e,
            Attribute::DigitCount => 0x5f,
            Attribute::PictureString => 0x60,
            Attribute::Mutable => 0x61,
            Attribute::ThreadsScaled => 0x62,
            Attribute::Explicit => 0x63,
            Attribute::ObjectPointer => 0x64,
            Attribute::Endianity => 0x65,
            Attribute::Elemental => 0x66,
            Attribute::Pure => 0x67,
            Attribute::Recursive => 0x68,
            Attribute::Signature => 0x69,
            Attribute::MainSubprogram => 0x6a,
            Attribute::DataBitOffset => 0x6b,
            Attribute::ConstExpr => 0x6c,
            Attribute::EnumClass => 0x6d,
            Attribute::LinkageName => 0x6e,
            Attribute::User(x) => x,
        }
    }

    pub fn from_u16(input: u16) -> Attribute {
        match input {
            0x01 => Attribute::Sibling,
            0x02 => Attribute::Location,
            0x03 => Attribute::Name,
            0x09 => Attribute::Ordering,
            0x0b => Attribute::ByteSize,
            0x0c => Attribute::BitOffset,
            0x0d => Attribute::BitSize,
            0x10 => Attribute::StmtList,
            0x11 => Attribute::LowPc,
            0x12 => Attribute::HighPc,
            0x13 => Attribute::Language,
            0x15 => Attribute::Discr,
            0x16 => Attribute::DiscrValue,
            0x17 => Attribute::Visibility,
            0x18 => Attribute::Import,
            0x19 => Attribute::StringLength,
            0x1a => Attribute::CommonReference,
            0x1b => Attribute::CompDir,
            0x1c => Attribute::ConstValue,
            0x1d => Attribute::ContainingType,
            0x1e => Attribute::DefaultValue,
            0x20 => Attribute::Inline,
            0x21 => Attribute::IsOptional,
            0x22 => Attribute::LowerBound,
            0x25 => Attribute::Producer,
            0x27 => Attribute::Prototyped,
            0x2a => Attribute::ReturnAddr,
            0x2c => Attribute::StartScope,
            0x2e => Attribute::BitStride,
            0x2f => Attribute::UpperBound,
            0x31 => Attribute::AbstractOrigin,
            0x32 => Attribute::Accessibility,
            0x33 => Attribute::AddressClass,
            0x34 => Attribute::Artificial,
            0x35 => Attribute::BaseTypes,
            0x36 => Attribute::CallingConvention,
            0x37 => Attribute::Count,
            0x38 => Attribute::DataMemberLocation,
            0x39 => Attribute::DeclColumn,
            0x3a => Attribute::DeclFile,
            0x3b => Attribute::DeclLine,
            0x3c => Attribute::Declaration,
            0x3d => Attribute::DiscrList,
            0x3e => Attribute::Encoding,
            0x3f => Attribute::External,
            0x40 => Attribute::FrameBase,
            0x41 => Attribute::Friend,
            0x42 => Attribute::IdentifierCase,
            0x43 => Attribute::MacroInfo,
            0x44 => Attribute::NamelistItem,
            0x45 => Attribute::Priority,
            0x46 => Attribute::Segment,
            0x47 => Attribute::Specification,
            0x48 => Attribute::StaticLink,
            0x49 => Attribute::Type,
            0x4a => Attribute::UseLocation,
            0x4b => Attribute::VariableParameter,
            0x4c => Attribute::Virtuality,
            0x4d => Attribute::VtableElemLocation,
            0x4e => Attribute::Allocated,
            0x4f => Attribute::Associated,
            0x50 => Attribute::DataLocation,
            0x51 => Attribute::ByteStride,
            0x52 => Attribute::EntryPc,
            0x53 => Attribute::UseUtf8,
            0x54 => Attribute::Extension,
            0x55 => Attribute::Ranges,
            0x56 => Attribute::Trampoline,
            0x57 => Attribute::CallColumn,
            0x58 => Attribute::CallFile,
            0x59 => Attribute::CallLine,
            0x5a => Attribute::Description,
            0x5b => Attribute::BinaryScale,
            0x5c => Attribute::DecimalScale,
            0x5d => Attribute::Small,
            0x5e => Attribute::DecimalSign,
            0x5f => Attribute::DigitCount,
            0x60 => Attribute::PictureString,
            0x61 => Attribute::Mutable,
            0x62 => Attribute::ThreadsScaled,
            0x63 => Attribute::Explicit,
            0x64 => Attribute::ObjectPointer,
            0x65 => Attribute::Endianity,
            0x66 => Attribute::Elemental,
            0x67 => Attribute::Pure,
            0x68 => Attribute::Recursive,
            0x69 => Attribute::Signature,
            0x6a => Attribute::MainSubprogram,
            0x6b => Attribute::DataBitOffset,
            0x6c => Attribute::ConstExpr,
            0x6d => Attribute::EnumClass,
            0x6e => Attribute::LinkageName,
            x if x >= 0x2000 && x <= 0x3fff => Attribute::User(x),
            x => panic!("Invalid attribute: {}", x),
        }
    }
}


fn read_unsigned_leb128(input: &[u8]) -> (ULeb128, usize) {
    let result = ULeb128::from_bytes(input);
    (result, result.byte_count())
}

fn read_signed_leb128(input: &[u8]) -> (ILeb128, usize) {
    let result = ILeb128::from_bytes(input);
    (result, result.byte_count())
}
