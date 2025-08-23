program check

    use, intrinsic :: iso_fortran_env , only: &!
        compiler_options , &!
        compiler_version

    use, non_intrinsic :: gmsh_msh_format_fortran



    implicit none



    print *, compiler_version()
    print *, compiler_options()

end program check
