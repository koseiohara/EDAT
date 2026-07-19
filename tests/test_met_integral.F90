program test_met_integral
  use, intrinsic :: iso_fortran_env, only: real64
  use EDAT_Met, only: meridionalIntegral, verticalIntegral
  use test_support, only: check, check_array_close, finish_tests
  implicit none

  call test_meridional_constant_fields
  call test_meridional_trigonometric_fields
  call test_vertical_full_columns
  call test_vertical_partial_surface_layer

  call finish_tests('test_met_integral')

contains

  subroutine test_meridional_constant_fields
    integer :: ny

    do ny = 2, 34, 4
      call check_meridional_constant_case(nx=3, ny=ny, nz=2)
    end do
  end subroutine test_meridional_constant_fields


  subroutine check_meridional_constant_case(nx, ny, nz)
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    real(real64), allocatable :: latitude(:)
    real(real64), allocatable :: field(:,:,:)
    real(real64), allocatable :: result(:,:)
    real(real64), allocatable :: expected(:,:)
    real(real64) :: value_at_north
    real(real64) :: value_at_south
    integer :: i
    integer :: j
    integer :: k
    integer :: status
    character(80) :: description

    allocate(latitude(ny), field(nx, ny, nz), result(nx, nz), expected(nx, nz))

    do j = 1, ny
      latitude(j) = -0.9_real64 &
        + 1.8_real64 * real(j - 1, real64) / real(ny - 1, real64)
    end do

    do k = 1, nz
      do i = 1, nx
        field(i, :, k) = real(i + k, real64)
      end do
    end do

    call meridionalIntegral( &
      latitude, &
      field, &
      latitude(1), &
      latitude(ny), &
      result, &
      status, &
      value_at_south, &
      value_at_north)

    expected = weighted_latitude_trapezoids(latitude, field, 1, ny)

    write(description, '(A,I0)') 'meridional integral status, ny=', ny
    call check(status == 1, description)

    write(description, '(A,I0)') 'meridional cos(latitude) weighting, ny=', ny
    call check_array_close( &
      reshape(result, [size(result)]), &
      reshape(expected, [size(expected)]), &
      2.0e-13_real64, &
      2.0e-13_real64, &
      description)

    deallocate(latitude, field, result, expected)
  end subroutine check_meridional_constant_case


  subroutine test_meridional_trigonometric_fields
    integer, parameter :: nx = 2
    integer, parameter :: ny = 41
    integer, parameter :: nz = 3

    real(real64) :: latitude(ny)
    real(real64) :: field(nx, ny, nz)
    real(real64) :: result(nx, nz)
    real(real64) :: expected(nx, nz)
    integer :: i
    integer :: j
    integer :: k
    integer :: status

    do j = 1, ny
      latitude(j) = -1.0_real64 &
        + 2.0_real64 * (real(j - 1, real64) / real(ny - 1, real64))**1.1_real64
    end do

    do k = 1, nz
      do i = 1, nx
        field(i, :, k) = real(i, real64) * sin(latitude) &
                       + real(k, real64) * cos(latitude)
      end do
    end do

    call meridionalIntegral( &
      latitude, field, latitude(3), latitude(ny - 2), result, status)

    expected = weighted_latitude_trapezoids(latitude, field, 3, ny - 2)

    call check(status == 1, 'meridional trigonometric integral status')
    call check_array_close( &
      reshape(result, [size(result)]), &
      reshape(expected, [size(expected)]), &
      3.0e-13_real64, &
      3.0e-13_real64, &
      'weighted trapezoidal integral of sine and cosine fields')
  end subroutine test_meridional_trigonometric_fields


  function weighted_latitude_trapezoids(latitude, field, first_index, last_index) &
      result(integral)
    real(real64), intent(in) :: latitude(:)
    real(real64), intent(in) :: field(:,:,:)
    integer, intent(in) :: first_index
    integer, intent(in) :: last_index

    real(real64) :: integral(size(field, 1), size(field, 3))
    integer :: i
    integer :: j
    integer :: k

    integral = 0.0_real64

    do k = 1, size(field, 3)
      do i = 1, size(field, 1)
        do j = first_index, last_index - 1
          integral(i, k) = integral(i, k) &
            + 0.5_real64 &
            * (field(i, j, k) * cos(latitude(j)) &
               + field(i, j + 1, k) * cos(latitude(j + 1))) &
            * (latitude(j + 1) - latitude(j))
        end do
      end do
    end do
  end function weighted_latitude_trapezoids


  subroutine test_vertical_full_columns
    integer :: nz

    do nz = 2, 12, 2
      call check_vertical_full_column_case(nx=3, ny=2, nz=nz)
    end do
  end subroutine test_vertical_full_columns


  subroutine check_vertical_full_column_case(nx, ny, nz)
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    real(real64), allocatable :: pressure(:)
    real(real64), allocatable :: field(:,:,:)
    real(real64), allocatable :: result(:,:)
    real(real64), allocatable :: expected(:,:)
    real(real64), allocatable :: surface_pressure(:,:)
    integer :: i
    integer :: j
    integer :: k
    integer :: status
    character(80) :: description

    allocate(pressure(nz), field(nx, ny, nz), result(nx, ny), &
             expected(nx, ny), surface_pressure(nx, ny))

    do k = 1, nz
      pressure(k) = 10000.0_real64 &
        + 80000.0_real64 * real(k - 1, real64) / real(nz - 1, real64)
    end do

    do j = 1, ny
      do i = 1, nx
        field(i, j, :) = real(i + j, real64) + pressure / 100000.0_real64
      end do
    end do

    surface_pressure = pressure(nz)
    expected = full_vertical_column_integral(pressure, field)

    call verticalIntegral(pressure, field, surface_pressure, result, status)

    write(description, '(A,I0)') 'vertical integral status, nz=', nz
    call check(status == 1, description)

    write(description, '(A,I0)') 'vertical TOA triangle and trapezoids, nz=', nz
    call check_array_close( &
      reshape(result, [size(result)]), &
      reshape(expected, [size(expected)]), &
      3.0e-13_real64, &
      3.0e-10_real64, &
      description)

    deallocate(pressure, field, result, expected, surface_pressure)
  end subroutine check_vertical_full_column_case


  function full_vertical_column_integral(pressure, field) result(integral)
    real(real64), intent(in) :: pressure(:)
    real(real64), intent(in) :: field(:,:,:)

    real(real64) :: integral(size(field, 1), size(field, 2))
    integer :: level

    integral = 0.5_real64 * pressure(1) * field(:, :, 1)

    do level = 1, size(pressure) - 1
      integral = integral &
        + 0.5_real64 * (pressure(level + 1) - pressure(level)) &
        * (field(:, :, level) + field(:, :, level + 1))
    end do
  end function full_vertical_column_integral


  subroutine test_vertical_partial_surface_layer
    integer, parameter :: nx = 2
    integer, parameter :: ny = 2
    integer, parameter :: nz = 4

    real(real64) :: pressure(nz)
    real(real64) :: field(nx, ny, nz)
    real(real64) :: result(nx, ny)
    real(real64) :: expected(nx, ny)
    real(real64) :: surface_pressure(nx, ny)
    integer :: i
    integer :: j
    integer :: status

    pressure = [ &
      10000.0_real64, &
      30000.0_real64, &
      60000.0_real64, &
      90000.0_real64]

    do j = 1, ny
      do i = 1, nx
        field(i, j, :) = real(i + j, real64) + pressure / 100000.0_real64
      end do
    end do

    surface_pressure = 75000.0_real64

    expected = &
        0.5_real64 * pressure(1) * field(:, :, 1) &
      + 0.5_real64 * (pressure(2) - pressure(1)) &
        * (field(:, :, 1) + field(:, :, 2)) &
      + 0.5_real64 * (pressure(3) - pressure(2)) &
        * (field(:, :, 2) + field(:, :, 3)) &
      + (surface_pressure - pressure(3)) * field(:, :, 3)

    call verticalIntegral(pressure, field, surface_pressure, result, status)

    call check(status == 1, 'partial surface vertical integral status')
    call check_array_close( &
      reshape(result, [size(result)]), &
      reshape(expected, [size(expected)]), &
      3.0e-13_real64, &
      3.0e-10_real64, &
      'partial surface layer uses the current upper-level value rule')
  end subroutine test_vertical_partial_surface_layer

end program test_met_integral
