# xmas-dwarf

A Rust library for reading and navigating DWARF files.

DWARF is used to represent debug information on Unix-like systems. xmas-dwarf is
a type-safe library written in Rust for parsing DWARF files.

* [Introduction to DWARF](http://dwarfstd.org/doc/Debugging%20using%20DWARF-2012.pdf)
* [Spec](http://www.dwarfstd.org/doc/DWARF4.pdf)

## Supported

* debug_info section (the DIEs)
* abbreviation table
* lines table

## limitations

* no support for sections not listed above
* assumes DWARF embedded in ELF
* only supports 32bit DWARF (note this is unrelated to the pointer width of the target machine) and kinda, sorta version 3
* does not take into account ELF relocation info
* no support for DWARF expressions
