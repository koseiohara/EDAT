module EDAT_BinIO

    implicit none

    private
    public :: finfo, endian_converter

    type finfo
        private
        integer        :: unit
        character(128) :: file
        character(16)  :: action
        integer        :: record
        integer        :: recl
        integer        :: recstep

        contains

        procedure, pass, public :: fclose
        procedure, pass, public :: get_record
        procedure, pass, public :: reset_record

        generic, public :: fread  => fread_s, fread_1, fread_2, fread_3, fread_4, fread_5
        generic, public :: fwrite => fwrite_ss, fwrite_sd, fwrite_sq, &
                                   & fwrite_1s, fwrite_1d, fwrite_1q, &
                                   & fwrite_2s, fwrite_2d, fwrite_2q, &
                                   & fwrite_3s, fwrite_3d, fwrite_3q, &
                                   & fwrite_4s, fwrite_4d, fwrite_4q, &
                                   & fwrite_5s, fwrite_5d, fwrite_5q

        procedure, pass, private :: fread_s
        procedure, pass, private :: fread_1
        procedure, pass, private :: fread_2
        procedure, pass, private :: fread_3
        procedure, pass, private :: fread_4
        procedure, pass, private :: fread_5
        procedure, pass, private :: fwrite_ss
        procedure, pass, private :: fwrite_sd
        procedure, pass, private :: fwrite_sq
        procedure, pass, private :: fwrite_1s
        procedure, pass, private :: fwrite_1d
        procedure, pass, private :: fwrite_1q
        procedure, pass, private :: fwrite_2s
        procedure, pass, private :: fwrite_2d
        procedure, pass, private :: fwrite_2q
        procedure, pass, private :: fwrite_3s
        procedure, pass, private :: fwrite_3d
        procedure, pass, private :: fwrite_3q
        procedure, pass, private :: fwrite_4s
        procedure, pass, private :: fwrite_4d
        procedure, pass, private :: fwrite_4q
        procedure, pass, private :: fwrite_5s
        procedure, pass, private :: fwrite_5d
        procedure, pass, private :: fwrite_5q
    end type finfo

    
    interface finfo
        module procedure fopen
    end interface finfo


    contains


    function fopen(unit, file, action, record, recl, recstep) result(self)
        type(finfo) :: self

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
            
            self%unit    = unit
        else
            open(NEWUNIT=self%unit    , &
               & FILE   =file         , &
               & ACTION =action       , &
               & FORM   ='UNFORMATTED', &
               & ACCESS ='DIRECT'     , &
               & RECL   =recl           )
        endif

        self%file    = file
        self%action  = action
        self%record  = record
        self%recl    = recl
        self%recstep = recstep

    end function fopen


    subroutine fclose(self)
        class(finfo), intent(inout) :: self
        logical :: open_status

        INQUIRE(self%unit      , &  !! IN
              & OPENED=open_status)  !! OUT

        if (open_status) then
            close(self%unit)
            
            self%unit    = 0
            self%file    = 'ERROR'
            self%action  = 'ERROR'
            self%record  = -999
            self%recl    = -999
            self%recstep = -999

            return
        else
            return
        endif

    end subroutine fclose


    subroutine fread_s(self, input_data)
        class(finfo), intent(inout) :: self
        real(4)     , intent(out)   :: input_data

        read(self%unit,rec=self%record) input_data
        self%record = self%record + self%recstep

    end subroutine fread_s


    subroutine fread_1(self, input_data)
        class(finfo), intent(inout) :: self
        real(4)     , intent(out)   :: input_data(:)

        read(self%unit,rec=self%record) input_data(:)
        self%record = self%record + self%recstep

    end subroutine fread_1


    subroutine fread_2(self, input_data)
        class(finfo), intent(inout) :: self
        real(4)     , intent(out)   :: input_data(:,:)

        read(self%unit,rec=self%record) input_data(:,:)
        self%record = self%record + self%recstep

    end subroutine fread_2


    subroutine fread_3(self, input_data)
        class(finfo), intent(inout) :: self
        real(4)     , intent(out)   :: input_data(:,:,:)

        read(self%unit,rec=self%record) input_data(:,:,:)
        self%record = self%record + self%recstep

    end subroutine fread_3


    subroutine fread_4(self, input_data)
        class(finfo), intent(inout) :: self
        real(4)     , intent(out)   :: input_data(:,:,:,:)

        read(self%unit,rec=self%record) input_data(:,:,:,:)
        self%record = self%record + self%recstep

    end subroutine fread_4


    subroutine fread_5(self, input_data)
        class(finfo), intent(inout) :: self
        real(4)     , intent(out)   :: input_data(:,:,:,:,:)

        read(self%unit,rec=self%record) input_data(:,:,:,:,:)
        self%record = self%record + self%recstep

    end subroutine fread_5


    subroutine fwrite_ss(self, output_data)
        integer, parameter :: kind=4
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data

        write(self%unit,rec=self%record) real(output_data, kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_ss


    subroutine fwrite_sd(self, output_data)
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data

        write(self%unit,rec=self%record) real(output_data, kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_sd


    subroutine fwrite_sq(self, output_data)
        integer, parameter :: kind=16
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data

        write(self%unit,rec=self%record) real(output_data, kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_sq


    subroutine fwrite_1s(self, output_data)
        integer, parameter :: kind=4
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:)

        write(self%unit,rec=self%record) real(output_data(:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_1s


    subroutine fwrite_1d(self, output_data)
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:)

        write(self%unit,rec=self%record) real(output_data(:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_1d


    subroutine fwrite_1q(self, output_data)
        integer, parameter :: kind=16
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:)

        write(self%unit,rec=self%record) real(output_data(:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_1q


    subroutine fwrite_2s(self, output_data)
        integer, parameter :: kind=4
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:)

        write(self%unit,rec=self%record) real(output_data(:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_2s


    subroutine fwrite_2d(self, output_data)
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:)

        write(self%unit,rec=self%record) real(output_data(:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_2d


    subroutine fwrite_2q(self, output_data)
        integer, parameter :: kind=16
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:)

        write(self%unit,rec=self%record) real(output_data(:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_2q


    subroutine fwrite_3s(self, output_data)
        integer, parameter :: kind=4
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_3s


    subroutine fwrite_3d(self, output_data)
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_3d


    subroutine fwrite_3q(self, output_data)
        integer, parameter :: kind=16
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_3q


    subroutine fwrite_4s(self, output_data)
        integer, parameter :: kind=4
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_4s


    subroutine fwrite_4d(self, output_data)
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_4d


    subroutine fwrite_4q(self, output_data)
        integer, parameter :: kind=16
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_4q


    subroutine fwrite_5s(self, output_data)
        integer, parameter :: kind=4
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_5s


    subroutine fwrite_5d(self, output_data)
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_5d


    subroutine fwrite_5q(self, output_data)
        integer, parameter :: kind=16
        class(finfo), intent(inout) :: self
        real(kind)  , intent(in)    :: output_data(:,:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:,:), kind=4)
        self%record = self%record + self%recstep

    end subroutine fwrite_5q


    subroutine get_record(self, record)
        class(finfo), intent(in)  :: self
        integer     , intent(out) :: record

        record = self%record

    end subroutine get_record


    subroutine reset_record(self, increment, newrecord)
        class(finfo), intent(inout) :: self
        integer     , intent(in)   , optional :: increment
        integer     , intent(in)   , optional :: newrecord

        if (present(increment)) then
            self%record = self%record + increment
            return
        else if (present(newrecord)) then
            self%record = newrecord
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

