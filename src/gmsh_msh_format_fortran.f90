!> Version: experimental
!> Implementation of a derived type to hold `$MeshFormat` of Gmsh MSH file format.
module gmsh_msh_format_fortran

    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_new_line



    implicit none



    private



    public :: gmsh_msh_format_type

    public :: read(formatted)
    public :: read(unformatted)

    public :: export_data_size
    public :: export_file_type
    public :: export_version
    public :: is_ascii
    public :: is_binary



    !> Version: experimental
    !> Derived type to hold `$MeshFormat` of Gmsh MSH file format.
    type :: gmsh_msh_format_type

        private

        real(c_double) :: version

        integer(c_int) :: file_type

        integer(c_int) :: data_size

    end type gmsh_msh_format_type



    !> `iostat` value when an error occurs
    integer, parameter :: iostat_error = 1

    !> `iostat` value when an I/O statement executes successfully
    integer, parameter :: iostat_success = 0



    character(*), parameter :: header = '$MeshFormat'
    character(*), parameter :: footer = '$EndMeshFormat'



    !> Version: experimental
    !> Read an `$MshMeshFormat` from a connected formatted unit.
    interface read(formatted)
        module procedure :: read_formatted
    end interface read(formatted)



    !> Version: experimental
    !> Read an `$MshMeshFormat` from a connected unformatted unit.
    interface read(unformatted)
        module procedure :: read_unformatted
    end interface read(unformatted)



    !> Version: experimental
    !> Exports the `data-size` of the read Gmsh MSH file format as a `c_int` value.
    interface export_data_size
        module procedure :: export_data_size_from_format
    end interface export_data_size



    !> Version: experimental
    !> Exports the `file-type` of the read Gmsh MSH file format as a `c_int` value.
    interface export_file_type
        module procedure :: export_file_type_from_format
    end interface export_file_type



    !> Version: experimental
    !> Exports the version of the read Gmsh MSH file format as a `c_double` value.
    interface export_version
        module procedure :: export_version_from_format
    end interface export_version



    !> Version: experimental
    !> Checks if the read Gmsh MSH file is ASCII mode
    interface is_ascii
        module procedure :: is_ascii_gmsh_msh_format
    end interface is_ascii



    !> Version: experimental
    !> Checks if the read Gmsh MSH file is binary mode
    interface is_binary
        module procedure :: is_binary_gmsh_msh_format
    end interface is_binary



    contains



    !> Version: experimental
    !> Exports the `data-size` of the read Gmsh MSH file format as a `c_int` value.
    elemental function export_data_size_from_format(gmsh_msh_format) result(data_size)

        type(gmsh_msh_format_type), intent(in) :: gmsh_msh_format

        integer(c_int) :: data_size



        data_size = gmsh_msh_format%data_size

    end function export_data_size_from_format



    !> Version: experimental
    !> Exports the `file-type` of the read Gmsh MSH file format as a `c_int` value.
    elemental function export_file_type_from_format(gmsh_msh_format) result(file_type)

        type(gmsh_msh_format_type), intent(in) :: gmsh_msh_format

        integer(c_int) :: file_type



        file_type = gmsh_msh_format%file_type

    end function export_file_type_from_format



    !> Version: experimental
    !> Exports the version of the read Gmsh MSH file format as a `c_double` value.
    elemental function export_version_from_format(gmsh_msh_format) result(version)

        type(gmsh_msh_format_type), intent(in) :: gmsh_msh_format

        real(c_double) :: version



        version = gmsh_msh_format%version

    end function export_version_from_format



    !> Version: experimental
    !> Checks if the read Gmsh MSH file is ASCII mode
    elemental function is_ascii_gmsh_msh_format(gmsh_msh_format) result(res)

        type(gmsh_msh_format_type), intent(in) :: gmsh_msh_format

        logical :: res



        res = gmsh_msh_format%file_type .eq. 0_c_int

    end function is_ascii_gmsh_msh_format



    !> Version: experimental
    !> Checks if the read Gmsh MSH file is binary mode
    elemental function is_binary_gmsh_msh_format(gmsh_msh_format) result(res)

        type(gmsh_msh_format_type), intent(in) :: gmsh_msh_format

        logical :: res



        res = gmsh_msh_format%file_type .eq. 1_c_int

    end function is_binary_gmsh_msh_format



    !> Version: experimental
    !> Read an `$MshMeshFormat` from a connected formatted unit.
    subroutine read_formatted(gmsh_msh_format, unit, iotype, v_list, iostat, iomsg)

        class(gmsh_msh_format_type), intent(inout) :: gmsh_msh_format

        integer, intent(in) :: unit

        character(*), intent(in) :: iotype

        integer, intent(in) :: v_list(:)

        integer, intent(out) :: iostat

        character(*), intent(inout) :: iomsg



        iomsg = ''



        if ( size( v_list(:) ) .gt. 0 ) then

            iostat = iostat_error

            write( iomsg, '(A)' ) "gmsh_msh_format_type does NOT support `v_list` formatters."

            return

        end if



        select case(iotype)

            case("LISTDIRECTED")

                call read_formatted_kernel(gmsh_msh_format, unit, iostat, iomsg)

            case default

                iostat = iostat_error

                write( iomsg, '(A)' ) "gmsh_msh_format_type only supports the `LISTDIRECTED` iotype."

        end select

    end subroutine read_formatted



    !> Version: experimental
    !> Read an `$MshMeshFormat` from a connected formatted unit.
    subroutine read_formatted_kernel(gmsh_msh_format, unit, iostat, iomsg)

        class(gmsh_msh_format_type), intent(inout) :: gmsh_msh_format

        integer, intent(in) :: unit

        integer, intent(out) :: iostat

        character(*), intent(inout) :: iomsg



        !> Version: experimental
        !> A string for reading a line of text.<br>
        !> The length of this string is provisional.
        character(32) :: text_line



        read( &!
        unit   = unit   , &!
        fmt    = '(A)'  , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        ) &!
        text_line

        if ( is_iostat_eor(iostat) ) iostat = iostat_success
        if ( iostat .ne. iostat_success ) return

        call check_text_line( &!
        text_line = text_line , &!
        str       = header    , &!
        iostat    = iostat    , &!
        iomsg     = iomsg       &!
        )



        read( &!
        unit   = unit   , &!
        fmt    = *      , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        ) &!
        gmsh_msh_format%version   , &!
        gmsh_msh_format%file_type , &!
        gmsh_msh_format%data_size

        if ( iostat .ne. iostat_success ) return



        read( &!
        unit   = unit   , &!
        fmt    = '(A)'  , &!
        iostat = iostat , &!
        iomsg  = iomsg    &!
        ) &!
        text_line

        if ( is_iostat_eor(iostat) ) iostat = iostat_success
        if ( iostat .ne. iostat_success ) return

        call check_text_line( &!
        text_line = text_line , &!
        str       = footer    , &!
        iostat    = iostat    , &!
        iomsg     = iomsg       &!
        )

    end subroutine read_formatted_kernel



    !> Version: experimental
    !> Read an `$MshMeshFormat` from a connected unformatted unit.
    subroutine read_unformatted(gmsh_msh_format, unit, iostat, iomsg)

        class(gmsh_msh_format_type), intent(inout) :: gmsh_msh_format

        integer, intent(in) :: unit

        integer, intent(out) :: iostat

        character(*), intent(inout) :: iomsg



        integer :: itr

        !> Version: experimental
        !> A `character` variable to read a character.
        character(1) :: buffer

        !> Version: experimental
        !> A `character` variable for holding read string.<br>
        !> The length of this string is provisional.
        character(32) :: str



        read_header: &!
        block

            itr   = 1
            iomsg = ''
            str   = ''

            do

                read( &!
                unit   = unit   , &!
                iostat = iostat , &!
                iomsg  = iomsg    &!
                ) &!
                buffer

                if ( iostat .ne. iostat_success ) return

                if ( buffer(1:1) .eq. '$' ) then

                    str(itr:itr) = buffer(1:1)

                    itr = itr + 1

                    exit

                end if

            end do

            do

                read( &!
                unit   = unit   , &!
                iostat = iostat , &!
                iomsg  = iomsg    &!
                ) &!
                buffer

                if ( iostat .ne. iostat_success ) return

                if ( buffer(1:1) .eq. c_new_line ) then

                    call check_text_line( &!
                    text_line = str(1:itr) , &!
                    str       = header     , &!
                    iostat    = iostat     , &!
                    iomsg     = iomsg        &!
                    )

                    exit

                end if

                str(itr:itr) = buffer(1:1)

                itr = itr + 1

            end do

            if ( iostat .ne. iostat_success ) return

        end block &!
        read_header



        read_values: &!
        block

            itr = 1
            str = ''

            do

                read( &!
                unit   = unit   , &!
                iostat = iostat , &!
                iomsg  = iomsg    &!
                ) &!
                buffer

                if ( iostat .ne. iostat_success ) return

                if ( buffer(1:1) .eq. c_new_line ) exit

                str(itr:itr) = buffer(1:1)

                itr = itr + 1

            end do

            read( &!
            unit   = str(1:itr) , &!
            fmt    = *          , &!
            iostat = iostat     , &!
            iomsg  = iomsg        &!
            ) &!
            gmsh_msh_format%version   , &!
            gmsh_msh_format%file_type , &!
            gmsh_msh_format%data_size

            if ( iostat .ne. iostat_success ) return

        end block &!
        read_values



        read_footer: &!
        block

            itr   = 1
            iomsg = ''
            str   = ''

            do

                read( &!
                unit   = unit   , &!
                iostat = iostat , &!
                iomsg  = iomsg    &!
                ) &!
                buffer

                if ( iostat .ne. iostat_success ) return

                if ( buffer(1:1) .eq. '$' ) then

                    str(itr:itr) = buffer(1:1)

                    itr = itr + 1

                    exit

                end if

            end do

            do

                read( &!
                unit   = unit   , &!
                iostat = iostat , &!
                iomsg  = iomsg    &!
                ) &!
                buffer

                if ( iostat .ne. iostat_success ) return

                if ( buffer(1:1) .eq. c_new_line ) then

                    call check_text_line( &!
                    text_line = str(1:itr) , &!
                    str       = footer     , &!
                    iostat    = iostat     , &!
                    iomsg     = iomsg        &!
                    )

                    exit

                end if

                str(itr:itr) = buffer(1:1)

                itr = itr + 1

            end do

            if ( iostat .ne. iostat_success ) return

        end block &!
        read_footer

    end subroutine read_unformatted



    subroutine check_text_line(text_line, str, iostat, iomsg)

        character(*), intent(in) :: text_line, str

        integer, intent(inout) :: iostat

        character(*), intent(inout) :: iomsg



        if ( trim(text_line) .eq. str ) return



        iostat = iostat_error

        write( iomsg, '(A)' ) "Failed to read " // str

    end subroutine check_text_line

end module gmsh_msh_format_fortran
