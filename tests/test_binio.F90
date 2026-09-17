program test_binio
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
    use :: EDAT_BinIO, only : endian_converter, finfo
    use :: test_support, only : check, check_array_close, finish_tests

    implicit none

    character(*), parameter :: real32_file = 'test_binio_r4.tmp'
    character(*), parameter :: real64_file = 'test_binio_r8.tmp'

    call test_real32_record_io(real32_file)
    call test_real64_storage_conversion(real64_file)
    call test_endian_conversion
    call remove_test_file(real32_file)
    call remove_test_file(real64_file)

    call finish_tests('test_binio')

    contains

    subroutine test_real32_record_io(file_name)
        character(*), intent(in) :: file_name

        type(finfo)    :: file_info
        real(real32)   :: input(17)
        real(real32)   :: output(17)
        integer(int32) :: record_number
        integer        :: index

        do index = 1, size(input)
            input(index) = real(index, real32) / 7.0_real32
        enddo

        file_info = finfo( &
            file = file_name, &
            action = 'write', &
            record = 1_int32, &
            recl = int(size(input) * storage_size(input) / 8, int32), &
            recstep = 2_int32)

        call file_info % fwrite(input(1:size(input,1)))
        call file_info % get_record(record_number)
        call check(record_number == 3_int32                  , &  !! IN
                 & 'record advances by recstep after writing'  )  !! IN
        call file_info % fclose()

        output(1:size(output,1)) = 0.0_real32
        file_info = finfo( &
            file = file_name, &
            action = 'read', &
            record = 1_int32, &
            recl = int(size(output) * storage_size(output) / 8, int32), &
            recstep = 2_int32)

        call file_info % fread(output(1:size(output,1)))
        call check_array_close(real(output(1:size(output,1)), real64)                               , &  !! IN
                             & real(input(1:size(input,1)), real64)                                , &  !! IN
                             & 0.0_real64                                         , &  !! IN
                             & 0.0_real64                                         , &  !! IN
                             & 'real32 rank-one values survive a write/read cycle'  )  !! IN

        call file_info % reset_record(newrecord = 1_int32)
        call file_info % get_record(record_number)
        call check(record_number == 1_int32                       , &  !! IN
                 & 'reset_record sets the requested record number'  )  !! IN
        call file_info % fclose()
    end subroutine test_real32_record_io


    subroutine test_real64_storage_conversion(file_name)
        character(*), intent(in) :: file_name

        type(finfo)  :: file_info
        real(real64) :: input(3, 4)
        real(real64) :: output(3, 4)
        real(real64) :: expected(3, 4)
        integer      :: index

        input(1:size(input,1),1:size(input,2)) = reshape( &
            [(real(index, real64) / 11.0_real64, index = 1, size(input))], &
            shape(input))

        file_info = finfo( &
            file = file_name, &
            action = 'write', &
            record = 1_int64, &
            recl = int(size(input) * storage_size(0.0_real32) / 8, int64), &
            recstep = 1_int64)

        call file_info % fwrite(input(1:size(input,1),1:size(input,2)))
        call file_info % fclose()

        output(1:size(output,1),1:size(output,2)) = 0.0_real64
        file_info = finfo( &
            file = file_name, &
            action = 'read', &
            record = 1_int64, &
            recl = int(size(output) * storage_size(0.0_real32) / 8, int64), &
            recstep = 1_int64)

        call file_info % fread(output(1:size(output,1),1:size(output,2)))
        call file_info % fclose()

        expected(1:size(expected,1),1:size(expected,2)) = real(real(input(1:size(input,1),1:size(input,2)), real32), real64)
        call check_array_close(reshape(output(1:size(output,1),1:size(output,2)), &
            & [size(output)])                                , &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2)), &
                                 & [size(expected)])                            , &  !! IN
                             & 0.0_real64                                                     , &  !! IN
                             & 0.0_real64                                                     , &  !! IN
                             & 'real64 values are stored and restored through real32 payloads'  )  !! IN
    end subroutine test_real64_storage_conversion


    subroutine test_endian_conversion
        real(real32) :: original(17)
        real(real32) :: converted(17)

        original(1:size(original,1)) = 0.0_real32
        original(1:3) = [1.0_real32, 2.0_real32, 3.0_real32]

        converted(1:size(converted,1)) = original(1:size(original,1))
        call endian_converter(converted(1:size(converted,1)))
        call endian_converter(converted(1:size(converted,1)))

        call check_array_close(real(converted(1:size(converted,1)), real64)                              , &  !! IN
                             & real(original(1:size(original,1)), real64)                               , &  !! IN
                             & 0.0_real64                                           , &  !! IN
                             & 0.0_real64                                           , &  !! IN
                             & 'applying endian conversion twice restores the input'  )  !! IN
    end subroutine test_endian_conversion


    subroutine remove_test_file(file_name)
        character(*), intent(in) :: file_name

        integer :: unit_number

        open(newunit = unit_number, file = file_name, status = 'old')
        close(unit_number, status = 'delete')
    end subroutine remove_test_file

end program test_binio
