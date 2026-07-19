program test_negative_binio
  use, intrinsic :: iso_fortran_env, only: int32
  use EDAT_BinIO, only: finfo
  implicit none
  type(finfo) :: f
  character(32) :: mode
  call get_command_argument(1,mode)
  select case(trim(mode))
  case('record')
    f=finfo(file='negative.tmp',action='write',record=0_int32,recl=4_int32,recstep=1_int32)
  case('recl')
    f=finfo(file='negative.tmp',action='write',record=1_int32,recl=0_int32,recstep=1_int32)
  case('missing')
    f=finfo(file='definitely_missing_edat_file.tmp',action='read',record=1_int32,recl=4_int32,recstep=1_int32)
  case default
    error stop 99
  end select
end program test_negative_binio
