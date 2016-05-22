extern crate xmas_elf;
extern crate xmas_dwarf;

use xmas_elf::ElfFile;
use xmas_dwarf::DwarfFile;
use xmas_dwarf::abbrev::Children;
use xmas_dwarf::info::HasChildren;

fn main() {
    let input = xmas_elf::open_file("foo.o");
    let elf_file = ElfFile::new(&input);
    let dwarf_file = DwarfFile::new(&elf_file);
    // println!("{}", dwarf_file.abbrev);
    // println!("{}", dwarf_file.lines);
    // println!("{}", dwarf_file.lines);
    for unit in &dwarf_file.info {
        println!("{}", unit);

        for die in &unit.dies {
            if let &Some(ref die) = die {
                if die.has_children() == Children::Yes {
                    println!("children:");
                    for c in die.children(&unit) {
                        println!("{}", c);
                    }
                }
            }
        }
    }
}
