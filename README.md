# gmsh_msh_format_fortran

Fortran module for reading `$MeshFormat` in the [Gmsh][GmshReferenceManualTop] [MSH file format][GmshReferenceManualMshFileFormat].  

`$MeshFormat` section is used in [Gmsh][GmshReferenceManualTop] [MSH file format][GmshReferenceManualMshFileFormat] version [2][GmshReferenceManualMsh2] and [4][GmshReferenceManualMsh4].

## Requirements

This module requires a Fortran compiler with the following features:

- Fortran 2003 or later standard
  - `iso_c_binding` intrinsic module
    - `c_double` type for C interoperability
    - `c_int` type for C interoperability
  - `is_iostat_eor` function
- Fortran 2008 or later standard
  - User-defined derived-type I/O (defined I/O) support for formatted READ operations

## How to use

### Manual Compilation

This library is composed of a single Fortran `module` written in one `.f90` file.  
To use the library, include this [source file](src/gmsh_msh_format_fortran.f90) in your compilation process.

### Using this `module` with [`fpm`][FpmGitHubRepository]

To use this `module` within your [`fpm`][FpmGitHubRepository]

To use `stdlib` within your `fpm` project, add the following lines to your `fpm.toml` file:
```toml
[dependencies]
gmsh_msh_format_fortran = { git="https://github.com/DSCF-1224/gmsh_msh_format_fortran" }
```

## Documentation

Documentation is available at [https://dscf-1224.github.io/gmsh_msh_format_fortran/](https://dscf-1224.github.io/gmsh_msh_format_fortran/).

## Dependents

- [gmsh_msh2_reader_fortran](https://github.com/DSCF-1224/gmsh_msh2_reader_fortran)

[FpmGitHubRepository]: https://github.com/fortran-lang/fpm
[GmshReferenceManualTop]: https://gmsh.info/doc/texinfo/gmsh.html
[GmshReferenceManualMshFileFormat]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format
[GmshReferenceManualMsh2]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format-version-2-_0028Legacy_0029
[GmshReferenceManualMsh4]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format

<!-- EOF -->
