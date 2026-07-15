module EDAT_BinIO
    use, intrinsic :: iso_fortran_env, only : i64=>int64

    implicit none

    private
    public :: finfo, endian_converter

    type finfo
        private
        integer        :: unit
        character(128) :: file
        character(16)  :: action
        integer(i64)   :: record
        integer        :: recl
        integer(i64)   :: recstep

        contains

        generic, public :: get_record => get_record32, &
                                       & get_record64

        generic, public :: fread => fread_ss, fread_sd, fread_sq, &
                                  & fread_1s, fread_1d, fread_1q, &
                                  & fread_2s, fread_2d, fread_2q, &
                                  & fread_3s, fread_3d, fread_3q, &
                                  & fread_4s, fread_4d, fread_4q, &
                                  & fread_5s, fread_5d, fread_5q

        generic, public :: fwrite => fwrite_ss, fwrite_sd, fwrite_sq, &
                                   & fwrite_1s, fwrite_1d, fwrite_1q, &
                                   & fwrite_2s, fwrite_2d, fwrite_2q, &
                                   & fwrite_3s, fwrite_3d, fwrite_3q, &
                                   & fwrite_4s, fwrite_4d, fwrite_4q, &
                                   & fwrite_5s, fwrite_5d, fwrite_5q

        procedure, pass, public  :: fclose

        procedure, pass, private :: get_record32
        procedure, pass, private :: get_record64

        procedure, pass, private :: fread_ss
        procedure, pass, private :: fread_sd
        procedure, pass, private :: fread_sq
        procedure, pass, private :: fread_1s
        procedure, pass, private :: fread_1d
        procedure, pass, private :: fread_1q
        procedure, pass, private :: fread_2s
        procedure, pass, private :: fread_2d
        procedure, pass, private :: fread_2q
        procedure, pass, private :: fread_3s
        procedure, pass, private :: fread_3d
        procedure, pass, private :: fread_3q
        procedure, pass, private :: fread_4s
        procedure, pass, private :: fread_4d
        procedure, pass, private :: fread_4q
        procedure, pass, private :: fread_5s
        procedure, pass, private :: fread_5d
        procedure, pass, private :: fread_5q

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
        use, intrinsic :: iso_fortran_env, only : err=>error_unit
        type(finfo) :: self

        integer     , intent(in), optional :: unit

        character(*), intent(in) :: file
        character(*), intent(in) :: action
        integer     , intent(in) :: record
        integer     , intent(in) :: recl
        integer     , intent(in) :: recstep

        if (record <= 0) then
            write(err,'(A)')    '<ERROR STOP>'
            write(err,'(A,I0)') 'Invalid initial record : ', record
            write(err,'(A)')    'Argument "record" should be more than 0'
            ERROR STOP
        endif

        if (recl <= 0) then
            write(err,'(A)')    '<ERROR STOP>'
            write(err,'(A,I0)') 'Invalid record length : ', recl
            write(err,'(A)')    'Argument "recl" should be more than 0'
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

        INQUIRE(self%unit         , &  !! IN
              & OPENED=open_status  )  !! OUT

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


    subroutine fread_ss(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data
        real(irk) :: work_input

        read(self%unit,rec=self%record) work_input
        input_data  = real(work_input, kind=rk)
        self%record = self%record + self%recstep

    end subroutine fread_ss


    subroutine fread_sd(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data
        real(irk) :: work_input

        read(self%unit,rec=self%record) work_input
        input_data  = real(work_input, kind=rk)
        self%record = self%record + self%recstep

    end subroutine fread_sd


    subroutine fread_sq(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data
        real(irk) :: work_input

        read(self%unit,rec=self%record) work_input
        input_data  = real(work_input, kind=rk)
        self%record = self%record + self%recstep

    end subroutine fread_sq


    subroutine fread_1s(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:)
        real(irk) :: work_input(size(input_data))

        read(self%unit,rec=self%record) work_input(:)
        input_data(:) = real(work_input(:), kind=rk)
        self%record   = self%record + self%recstep

    end subroutine fread_1s


    subroutine fread_1d(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:)
        real(irk) :: work_input(size(input_data))

        read(self%unit,rec=self%record) work_input(:)
        input_data(:) = real(work_input(:), kind=rk)
        self%record   = self%record + self%recstep

    end subroutine fread_1d


    subroutine fread_1q(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:)
        real(irk) :: work_input(size(input_data))

        read(self%unit,rec=self%record) work_input(:)
        input_data(:) = real(work_input(:), kind=rk)
        self%record   = self%record + self%recstep

    end subroutine fread_1q


    subroutine fread_2s(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2))

        read(self%unit,rec=self%record) work_input(:,:)
        input_data(:,:) = real(work_input(:,:), kind=rk)
        self%record     = self%record + self%recstep

    end subroutine fread_2s


    subroutine fread_2d(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2))

        read(self%unit,rec=self%record) work_input(:,:)
        input_data(:,:) = real(work_input(:,:), kind=rk)
        self%record     = self%record + self%recstep

    end subroutine fread_2d


    subroutine fread_2q(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2))

        read(self%unit,rec=self%record) work_input(:,:)
        input_data(:,:) = real(work_input(:,:), kind=rk)
        self%record     = self%record + self%recstep

    end subroutine fread_2q


    subroutine fread_3s(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3))

        read(self%unit,rec=self%record) work_input(:,:,:)
        input_data(:,:,:) = real(work_input(:,:,:), kind=rk)
        self%record       = self%record + self%recstep

    end subroutine fread_3s


    subroutine fread_3d(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3))

        read(self%unit,rec=self%record) work_input(:,:,:)
        input_data(:,:,:) = real(work_input(:,:,:), kind=rk)
        self%record       = self%record + self%recstep

    end subroutine fread_3d


    subroutine fread_3q(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3))

        read(self%unit,rec=self%record) work_input(:,:,:)
        input_data(:,:,:) = real(work_input(:,:,:), kind=rk)
        self%record       = self%record + self%recstep

    end subroutine fread_3q


    subroutine fread_4s(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3),size(input_data,4))

        read(self%unit,rec=self%record) work_input(:,:,:,:)
        input_data(:,:,:,:) = real(work_input(:,:,:,:), kind=rk)
        self%record         = self%record + self%recstep

    end subroutine fread_4s


    subroutine fread_4d(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3),size(input_data,4))

        read(self%unit,rec=self%record) work_input(:,:,:,:)
        input_data(:,:,:,:) = real(work_input(:,:,:,:), kind=rk)
        self%record         = self%record + self%recstep

    end subroutine fread_4d


    subroutine fread_4q(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3),size(input_data,4))

        read(self%unit,rec=self%record) work_input(:,:,:,:)
        input_data(:,:,:,:) = real(work_input(:,:,:,:), kind=rk)
        self%record         = self%record + self%recstep

    end subroutine fread_4q


    subroutine fread_5s(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3),size(input_data,4),size(input_data,5))

        read(self%unit,rec=self%record) work_input(:,:,:,:,:)
        input_data(:,:,:,:,:) = real(work_input(:,:,:,:,:), kind=rk)
        self%record           = self%record + self%recstep

    end subroutine fread_5s


    subroutine fread_5d(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3),size(input_data,4),size(input_data,5))

        read(self%unit,rec=self%record) work_input(:,:,:,:,:)
        input_data(:,:,:,:,:) = real(work_input(:,:,:,:,:), kind=rk)
        self%record           = self%record + self%recstep

    end subroutine fread_5d


    subroutine fread_5q(self, input_data)
        use, intrinsic :: iso_fortran_env, only : irk=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(out)   :: input_data(:,:,:,:,:)
        real(irk) :: work_input(size(input_data,1),size(input_data,2),size(input_data,3),size(input_data,4),size(input_data,5))

        read(self%unit,rec=self%record) work_input(:,:,:,:,:)
        input_data(:,:,:,:,:) = real(work_input(:,:,:,:,:), kind=rk)
        self%record           = self%record + self%recstep

    end subroutine fread_5q


    subroutine fwrite_ss(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)  , intent(in)    :: output_data

        write(self%unit,rec=self%record) real(output_data, kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_ss


    subroutine fwrite_sd(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data

        write(self%unit,rec=self%record) real(output_data, kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_sd


    subroutine fwrite_sq(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data

        write(self%unit,rec=self%record) real(output_data, kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_sq


    subroutine fwrite_1s(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:)

        write(self%unit,rec=self%record) real(output_data(:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_1s


    subroutine fwrite_1d(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real64
        integer, parameter :: kind=8
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:)

        write(self%unit,rec=self%record) real(output_data(:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_1d


    subroutine fwrite_1q(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:)

        write(self%unit,rec=self%record) real(output_data(:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_1q


    subroutine fwrite_2s(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:)

        write(self%unit,rec=self%record) real(output_data(:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_2s


    subroutine fwrite_2d(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:)

        write(self%unit,rec=self%record) real(output_data(:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_2d


    subroutine fwrite_2q(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:)

        write(self%unit,rec=self%record) real(output_data(:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_2q


    subroutine fwrite_3s(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_3s


    subroutine fwrite_3d(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_3d


    subroutine fwrite_3q(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_3q


    subroutine fwrite_4s(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_4s


    subroutine fwrite_4d(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_4d


    subroutine fwrite_4q(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_4q


    subroutine fwrite_5s(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real32
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_5s


    subroutine fwrite_5d(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real64
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_5d


    subroutine fwrite_5q(self, output_data)
        use, intrinsic :: iso_fortran_env, only : r32=>real32, rk=>real128
        class(finfo), intent(inout) :: self
        real(rk)    , intent(in)    :: output_data(:,:,:,:,:)

        write(self%unit,rec=self%record) real(output_data(:,:,:,:,:), kind=r32)
        self%record = self%record + self%recstep

    end subroutine fwrite_5q


    subroutine get_record32(self, record)
        use, intrinsic :: iso_fortran_env, only : lik=>int32
        class(finfo), intent(in)  :: self
        integer(lik), intent(out) :: record

        record = self%record

    end subroutine get_record32


    subroutine get_record64(self, record)
        use, intrinsic :: iso_fortran_env, only : lik=>int64
        class(finfo), intent(in)  :: self
        integer(lik), intent(out) :: record

        record = self%record

    end subroutine get_record64


    subroutine reset_record(self, increment, newrecord)
        use, intrinsic :: iso_fortran_env, only : int32, int64, err=>error_unit
        class(finfo), intent(inout) :: self
        class(*), intent(in), optional :: increment
        class(*), intent(in), optional :: newrecord

        if (present(increment)) then
            select type (increment)
            type is (integer(int32))
                self%record = self%record + int(increment, kind=int64)
            type is (integer(int64))
                self%record = self%record + int(increment, kind=int64)
            class default
                write(err,'(A)') '<ERROR STOP>'
                write(err,'(A)') '"increment" must be integer(int32) or integer(int64).'
                ERROR STOP
            end select
        else if (present(newrecord)) then
            select type (newrecord)
            type is (integer(int32))
                self%record = newrecord
            type is (integer(int64))
                self%record = newrecord
            class default
                write(err,'(A)') '<ERROR STOP>'
                write(err,'(A)') '"increment" must be integer(int32) or integer(int64).'
                ERROR STOP
            end select
        else
            write(*,'(A)') '<ERROR STOP>'
            write(*,'(A)') 'Both "increment" and "newrecord" were not specified in the argument of reset_record()'
            ERROR STOP
        endif

    end subroutine reset_record


    pure elemental subroutine endian_converter(rawOre)
        use, intrinsic :: iso_fortran_env, only : int8, rk=>real32
        real(rk), intent(inout) :: rawOre
        integer(int8) :: bits(rk)

        bits(1:rk) = transfer(rawOre, bits)
        rawOre     = transfer(bits(rk:1:-1), rawOre)

    end subroutine endian_converter


end module EDAT_BinIO

