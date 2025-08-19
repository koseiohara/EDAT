module EDAT_BinIO

    implicit none

    private
    public :: finfo, fopen, fclose, fread, fwrite, get_record, reset_record, endian_converter

    type finfo
        private
        integer        :: unit
        character(128) :: file
        character(16)  :: action
        integer        :: record
        integer        :: recl
        integer        :: recstep
    end type finfo


    interface fread
        module procedure &
            & fread_s, &
            & fread_1, &
            & fread_2, &
            & fread_3, &
            & fread_4, &
            & fread_5
    end interface fread

    interface fwrite
        module procedure &
            & fwrite_ss, &
            & fwrite_sd, &
            & fwrite_sq, &
            & fwrite_1s, &
            & fwrite_1d, &
            & fwrite_1q, &
            & fwrite_2s, &
            & fwrite_2d, &
            & fwrite_2q, &
            & fwrite_3s, &
            & fwrite_3d, &
            & fwrite_3q, &
            & fwrite_4s, &
            & fwrite_4d, &
            & fwrite_4q, &
            & fwrite_5s, &
            & fwrite_5d, &
            & fwrite_5q
    end interface fwrite


    contains


    subroutine fopen(ftype, unit, file, action, record, recl, recstep)
        type(finfo) , intent(out) :: ftype

        integer     , intent(in), optional :: unit

        character(*), intent(in) :: file
        character(*), intent(in) :: action
        integer     , intent(in) :: record
        integer     , intent(in) :: recl
        integer     , intent(in) :: recstep

        if (record <= 0) then
            write(0,'(A)')    'ERROR STOP'
            write(0,'(A,I0)') 'Invalid initial record : ', record
            write(0,'(A)')    'Argument "record" should be more than 0'
            ERROR STOP
        endif

        if (recl <= 0) then
            write(0,'(A)')    'ERROR STOP'
            write(0,'(A,I0)') 'Invalid record length : ', recl
            write(0,'(A)')    'Argument "recl" should be more than 0'
            ERROR STOP
        endif

        if (present(unit)) then
            open(unit                , &
               & FILE  =file         , &
               & ACTION=action       , &
               & FORM  ='UNFORMATTED', &
               & ACCESS='DIRECT'     , &
               & RECL  =recl           )
            
            ftype%unit    = unit
        else
            open(NEWUNIT=ftype%unit   , &
               & FILE   =file         , &
               & ACTION =action       , &
               & FORM   ='UNFORMATTED', &
               & ACCESS ='DIRECT'     , &
               & RECL   =recl           )
        endif

        ftype%file    = file
        ftype%action  = action
        ftype%record  = record
        ftype%recl    = recl
        ftype%recstep = recstep

    end subroutine fopen


    subroutine fclose(ftype)
        type(finfo), intent(inout) :: ftype
        logical :: open_status

        INQUIRE(ftype%unit      , &  !! IN
              & OPENED=open_status)  !! OUT

        if (open_status) then
            close(ftype%unit)
            
            ftype%unit    = 0
            ftype%file    = 'ERROR'
            ftype%action  = 'ERROR'
            ftype%record  = -999
            ftype%recl    = -999
            ftype%recstep = -999

            return
        else
            return
        endif

    end subroutine fclose


    subroutine fread_s(ftype, input_data)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: input_data

        read(ftype%unit,rec=ftype%record) input_data
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_s


    subroutine fread_1(ftype, input_data)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: input_data(:)

        read(ftype%unit,rec=ftype%record) input_data(:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_1


    subroutine fread_2(ftype, input_data)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: input_data(:,:)

        read(ftype%unit,rec=ftype%record) input_data(:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_2


    subroutine fread_3(ftype, input_data)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: input_data(:,:,:)

        read(ftype%unit,rec=ftype%record) input_data(:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_3


    subroutine fread_4(ftype, input_data)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: input_data(:,:,:,:)

        read(ftype%unit,rec=ftype%record) input_data(:,:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_4


    subroutine fread_5(ftype, input_data)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: input_data(:,:,:,:,:)

        read(ftype%unit,rec=ftype%record) input_data(:,:,:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_5


    subroutine fwrite_ss(ftype, output_data)
        integer, parameter :: kind=4
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data

        write(ftype%unit,rec=ftype%record) real(output_data, kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_ss


    subroutine fwrite_sd(ftype, output_data)
        integer, parameter :: kind=8
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data

        write(ftype%unit,rec=ftype%record) real(output_data, kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_sd


    subroutine fwrite_sq(ftype, output_data)
        integer, parameter :: kind=16
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data

        write(ftype%unit,rec=ftype%record) real(output_data, kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_sq


    subroutine fwrite_1s(ftype, output_data)
        integer, parameter :: kind=4
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:)

        write(ftype%unit,rec=ftype%record) real(output_data(:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_1s


    subroutine fwrite_1d(ftype, output_data)
        integer, parameter :: kind=8
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:)

        write(ftype%unit,rec=ftype%record) real(output_data(:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_1d


    subroutine fwrite_1q(ftype, output_data)
        integer, parameter :: kind=16
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:)

        write(ftype%unit,rec=ftype%record) real(output_data(:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_1q


    subroutine fwrite_2s(ftype, output_data)
        integer, parameter :: kind=4
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_2s


    subroutine fwrite_2d(ftype, output_data)
        integer, parameter :: kind=8
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_2d


    subroutine fwrite_2q(ftype, output_data)
        integer, parameter :: kind=16
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_2q


    subroutine fwrite_3s(ftype, output_data)
        integer, parameter :: kind=4
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_3s


    subroutine fwrite_3d(ftype, output_data)
        integer, parameter :: kind=8
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_3d


    subroutine fwrite_3q(ftype, output_data)
        integer, parameter :: kind=16
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_3q


    subroutine fwrite_4s(ftype, output_data)
        integer, parameter :: kind=4
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_4s


    subroutine fwrite_4d(ftype, output_data)
        integer, parameter :: kind=8
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_4d


    subroutine fwrite_4q(ftype, output_data)
        integer, parameter :: kind=16
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_4q


    subroutine fwrite_5s(ftype, output_data)
        integer, parameter :: kind=4
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_5s


    subroutine fwrite_5d(ftype, output_data)
        integer, parameter :: kind=8
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_5d


    subroutine fwrite_5q(ftype, output_data)
        integer, parameter :: kind=16
        type(finfo), intent(inout) :: ftype
        real(kind) , intent(in)    :: output_data(:,:,:,:,:)

        write(ftype%unit,rec=ftype%record) real(output_data(:,:,:,:,:), kind=4)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite_5q


    subroutine get_record(ftype, record)
        type(finfo), intent(in)  :: ftype
        integer    , intent(out) :: record

        record = ftype%record

    end subroutine get_record


    subroutine reset_record(ftype, increment, newrecord)
        type(finfo), intent(inout) :: ftype
        integer    , intent(in)   , optional :: increment
        integer    , intent(in)   , optional :: newrecord

        if (present(increment)) then
            ftype%record = ftype%record + increment
            return
        else if (present(newrecord)) then
            ftype%record = newrecord
            return
        else
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A)') 'Both "increment" and "newrecord" were not specified in the argument of reset_record()'
            ERROR STOP
        endif

    end subroutine reset_record


    pure elemental subroutine endian_converter(rawOre)
        use iso_fortran_env, only : int8, real32
        integer, parameter :: rk=real32
        real(rk), intent(inout) :: rawOre
        integer(int8) :: bits(rk)

        bits(1:rk) = transfer(rawOre, bits)
        rawOre     = transfer(bits(rk:1:-1), rawOre)

    end subroutine endian_converter


end module EDAT_BinIO

