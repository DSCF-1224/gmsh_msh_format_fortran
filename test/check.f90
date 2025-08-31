program check

    use, intrinsic :: iso_fortran_env , only: &!
        error_unit       , &!
        output_unit      , &!
        compiler_options , &!
        compiler_version

    use, non_intrinsic :: gmsh_msh_format_fortran



    implicit none



    print *, compiler_version()
    print *, compiler_options()

    call test_formatted( 'test/empty-ASCII.msh2' )
    call test_formatted( 'test/empty-ASCII.msh4' )

    call test_unformatted( 'test/empty-binary.msh2' )
    call test_unformatted( 'test/empty-binary.msh4' )



    contains



    subroutine test_formatted(file)

        character(*), intent(in) :: file



        integer :: unit, iostat

        character(256) :: iomsg

        type(gmsh_msh_format_type) :: msh_format



        open( &!
        newunit = unit        , &!
        file    = file        , &!
        form    = 'formatted' , &!
        iostat  = iostat      , &!
        iomsg   = iomsg         &!
        )

        call handle_io_error(file = file, iostat = iostat, iomsg = iomsg )



        read( &!
        unit   = unit   , &!
        fmt    = *      , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        ) &!
        msh_format

        call handle_io_error(file = file, iostat = iostat, iomsg = iomsg )



        close( &!
        unit   = unit   , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        )

        call handle_io_error(file = file, iostat = iostat, iomsg = iomsg )



        if ( is_binary(msh_format) ) then

            write( error_unit, '(A)' ) 'binary mode is detected in read(formatted)'

            error stop

        end if

    end subroutine test_formatted



    subroutine test_unformatted(file)

        character(*), intent(in) :: file



        integer :: unit, iostat

        character(256) :: iomsg

        type(gmsh_msh_format_type) :: msh_format



        open( &!
        newunit = unit          , &!
        file    = file          , &!
        access  = 'stream'      , &!
        form    = 'unformatted' , &!
        iostat  = iostat        , &!
        iomsg   = iomsg           &!
        )

        call handle_io_error(file = file, iostat = iostat, iomsg = iomsg )



        read( &!
        unit   = unit   , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        ) &!
        msh_format

        call handle_io_error(file = file, iostat = iostat, iomsg = iomsg )



        close( &!
        unit   = unit   , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        )

        call handle_io_error(file = file, iostat = iostat, iomsg = iomsg )



        if ( is_ascii(msh_format) ) then

            write( error_unit, '(A)' ) 'ASCII mode is detected in read(unformatted)'

            error stop

        end if

    end subroutine test_unformatted



    subroutine handle_io_error(file, iostat, iomsg)

        character(*), intent(in) :: file

        integer, intent(in) :: iostat

        character(*), intent(in) :: iomsg



        if ( iostat .ne. 0 ) then

            write( error_unit, '(A,1X,A)'  ) 'file   :', trim(file)
            write( error_unit, '(A,1X,I0)' ) 'iostat :', iostat
            write( error_unit, '(A,1X,A)'  ) 'iomsg  :', trim(iomsg)

            error stop

        end if

    end subroutine handle_io_error

end program check
