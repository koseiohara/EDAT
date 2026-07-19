program test_binio
  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  use EDAT_BinIO, only: endian_converter, finfo
  use test_support, only: check, check_array_close, finish_tests
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

    type(finfo) :: file_info
    real(real32) :: input(17)
    real(real32) :: output(17)
    integer(int32) :: record_number
    integer :: index

    do index = 1, size(input)
      input(index) = real(index, real32) / 7.0_real32
    end do

    file_info = finfo( &
      file=file_name, &
      action='write', &
      record=1_int32, &
      recl=int(size(input) * storage_size(input) / 8, int32), &
      recstep=2_int32)

    call file_info%fwrite(input)
    call file_info%get_record(record_number)
    call check(record_number == 3_int32, &
               'record advances by recstep after writing')
    call file_info%fclose()

    output = 0.0_real32
    file_info = finfo( &
      file=file_name, &
      action='read', &
      record=1_int32, &
      recl=int(size(output) * storage_size(output) / 8, int32), &
      recstep=2_int32)

    call file_info%fread(output)
    call check_array_close( &
      real(output, real64), &
      real(input, real64), &
      0.0_real64, &
      0.0_real64, &
      'real32 rank-one values survive a write/read cycle')

    call file_info%reset_record(newrecord=1_int32)
    call file_info%get_record(record_number)
    call check(record_number == 1_int32, &
               'reset_record sets the requested record number')
    call file_info%fclose()
  end subroutine test_real32_record_io


  subroutine test_real64_storage_conversion(file_name)
    character(*), intent(in) :: file_name

    type(finfo) :: file_info
    real(real64) :: input(3, 4)
    real(real64) :: output(3, 4)
    real(real64) :: expected(3, 4)
    integer :: index

    input = reshape( &
      [(real(index, real64) / 11.0_real64, index=1, size(input))], &
      shape(input))

    file_info = finfo( &
      file=file_name, &
      action='write', &
      record=1_int64, &
      recl=int(size(input) * storage_size(0.0_real32) / 8, int64), &
      recstep=1_int64)

    call file_info%fwrite(input)
    call file_info%fclose()

    output = 0.0_real64
    file_info = finfo( &
      file=file_name, &
      action='read', &
      record=1_int64, &
      recl=int(size(output) * storage_size(0.0_real32) / 8, int64), &
      recstep=1_int64)

    call file_info%fread(output)
    call file_info%fclose()

    expected = real(real(input, real32), real64)
    call check_array_close( &
      reshape(output, [size(output)]), &
      reshape(expected, [size(expected)]), &
      0.0_real64, &
      0.0_real64, &
      'real64 values are stored and restored through real32 payloads')
  end subroutine test_real64_storage_conversion


  subroutine test_endian_conversion
    real(real32) :: original(17)
    real(real32) :: converted(17)

    original = 0.0_real32
    original(1:3) = [1.0_real32, 2.0_real32, 3.0_real32]

    converted = original
    call endian_converter(converted)
    call endian_converter(converted)

    call check_array_close( &
      real(converted, real64), &
      real(original, real64), &
      0.0_real64, &
      0.0_real64, &
      'applying endian conversion twice restores the input')
  end subroutine test_endian_conversion


  subroutine remove_test_file(file_name)
    character(*), intent(in) :: file_name

    integer :: unit_number

    open(newunit=unit_number, file=file_name, status='old')
    close(unit_number, status='delete')
  end subroutine remove_test_file

end program test_binio
