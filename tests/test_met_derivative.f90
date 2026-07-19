program test_met_derivative
  use, intrinsic :: iso_fortran_env, only: real64
  use EDAT_Math, only: M_PI
  use EDAT_Met, only: meridionalDerivative, verticalDerivative, zonalDerivative
  use test_support, only: check, check_array_close, finish_tests
  implicit none

  real(real64), parameter :: pi = real(M_PI, real64)

  call test_zonal_linear_fields
  call test_zonal_periodic_trigonometric_fields
  call test_meridional_linear_fields
  call test_meridional_trigonometric_field
  call test_repeated_latitude_rejection
  call test_vertical_linear_fields

  call finish_tests('test_met_derivative')

contains

  subroutine test_zonal_linear_fields
    integer :: nx

    do nx = 2, 17, 5
      call check_zonal_linear_case(nx, ny=3, nz=2)
    end do
  end subroutine test_zonal_linear_fields


  subroutine check_zonal_linear_case(nx, ny, nz)
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    real(real64), allocatable :: longitude(:)
    real(real64), allocatable :: field(:,:,:)
    real(real64), allocatable :: derivative(:,:,:)
    real(real64), allocatable :: expected(:,:,:)
    integer :: i
    integer :: j
    integer :: k
    integer :: status
    character(80) :: description

    allocate(longitude(nx), field(nx, ny, nz), derivative(nx, ny, nz), &
             expected(nx, ny, nz))

    do i = 1, nx
      longitude(i) = -1.0_real64 &
        + 2.0_real64 * real(i - 1, real64) / real(nx - 1, real64)
    end do

    do k = 1, nz
      do j = 1, ny
        field(:, j, k) = (2.0_real64 + real(j, real64)) * longitude &
                       + real(k, real64)
        expected(:, j, k) = 2.0_real64 + real(j, real64)
      end do
    end do

    call zonalDerivative( &
      longitude, field, derivative, periodic=.false., status=status)

    write(description, '(A,I0)') 'zonal linear status, nx=', nx
    call check(status == 1, description)

    write(description, '(A,I0)') 'zonal derivative of a linear field, nx=', nx
    call check_array_close( &
      reshape(derivative, [size(derivative)]), &
      reshape(expected, [size(expected)]), &
      1.0e-13_real64, &
      1.0e-13_real64, &
      description)

    deallocate(longitude, field, derivative, expected)
  end subroutine check_zonal_linear_case


  subroutine test_zonal_periodic_trigonometric_fields
    integer :: nx

    do nx = 8, 64, 8
      call check_zonal_trigonometric_case(nx, ny=2, nz=2)
    end do
  end subroutine test_zonal_periodic_trigonometric_fields


  subroutine check_zonal_trigonometric_case(nx, ny, nz)
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    real(real64), allocatable :: longitude(:)
    real(real64), allocatable :: field(:,:,:)
    real(real64), allocatable :: derivative(:,:,:)
    real(real64), allocatable :: expected(:,:,:)
    real(real64) :: grid_spacing
    real(real64) :: maximum_error
    integer :: i
    integer :: j
    integer :: k
    integer :: status
    character(80) :: description

    allocate(longitude(nx), field(nx, ny, nz), derivative(nx, ny, nz), &
             expected(nx, ny, nz))

    grid_spacing = 2.0_real64 * pi / real(nx, real64)
    do i = 1, nx
      longitude(i) = grid_spacing * real(i - 1, real64)
    end do

    do k = 1, nz
      do j = 1, ny
        field(:, j, k) = sin(longitude) &
                       + 0.3_real64 * cos(2.0_real64 * longitude)
        expected(:, j, k) = cos(longitude) &
                          - 0.6_real64 * sin(2.0_real64 * longitude)
      end do
    end do

    call zonalDerivative(longitude, field, derivative, status=status)
    maximum_error = maxval(abs(derivative - expected))

    write(description, '(A,I0)') 'periodic zonal status, nx=', nx
    call check(status == 1, description)

    write(description, '(A,I0)') 'second-order periodic zonal accuracy, nx=', nx
    call check(maximum_error < 5.0_real64 * grid_spacing**2, description)

    deallocate(longitude, field, derivative, expected)
  end subroutine check_zonal_trigonometric_case


  subroutine test_meridional_linear_fields
    integer :: ny

    do ny = 2, 19, 4
      call check_meridional_linear_case(nx=3, ny=ny, nz=2)
    end do
  end subroutine test_meridional_linear_fields


  subroutine check_meridional_linear_case(nx, ny, nz)
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    real(real64), allocatable :: latitude(:)
    real(real64), allocatable :: field(:,:,:)
    real(real64), allocatable :: derivative(:,:,:)
    real(real64), allocatable :: expected(:,:,:)
    integer :: i
    integer :: j
    integer :: k
    integer :: status
    character(80) :: description

    allocate(latitude(ny), field(nx, ny, nz), derivative(nx, ny, nz), &
             expected(nx, ny, nz))

    do j = 1, ny
      latitude(j) = -0.8_real64 &
        + 1.6_real64 * (real(j - 1, real64) / real(ny - 1, real64))**1.2_real64
    end do

    do k = 1, nz
      do i = 1, nx
        field(i, :, k) = (1.0_real64 + real(i, real64)) * latitude &
                       + real(k, real64)
        expected(i, :, k) = 1.0_real64 + real(i, real64)
      end do
    end do

    call meridionalDerivative(latitude, field, derivative, status)

    write(description, '(A,I0)') 'meridional linear status, ny=', ny
    call check(status == 1, description)

    write(description, '(A,I0)') 'meridional derivative of a linear field, ny=', ny
    call check_array_close( &
      reshape(derivative, [size(derivative)]), &
      reshape(expected, [size(expected)]), &
      2.0e-13_real64, &
      2.0e-13_real64, &
      description)

    deallocate(latitude, field, derivative, expected)
  end subroutine check_meridional_linear_case


  subroutine test_meridional_trigonometric_field
    integer, parameter :: nx = 2
    integer, parameter :: ny = 65
    integer, parameter :: nz = 2

    real(real64) :: latitude(ny)
    real(real64) :: field(nx, ny, nz)
    real(real64) :: derivative(nx, ny, nz)
    real(real64) :: expected(nx, ny, nz)
    integer :: i
    integer :: j
    integer :: k
    integer :: status

    do j = 1, ny
      latitude(j) = -pi / 3.0_real64 &
        + (2.0_real64 * pi / 3.0_real64) &
        * real(j - 1, real64) / real(ny - 1, real64)
    end do

    do k = 1, nz
      do i = 1, nx
        field(i, :, k) = sin(latitude) &
                       + 0.2_real64 * cos(2.0_real64 * latitude)
        expected(i, :, k) = cos(latitude) &
                          - 0.4_real64 * sin(2.0_real64 * latitude)
      end do
    end do

    call meridionalDerivative(latitude, field, derivative, status)

    call check(status == 1, 'meridional trigonometric status')
    call check( &
      maxval(abs(derivative(:, 2:ny - 1, :) &
                 - expected(:, 2:ny - 1, :))) < 5.0e-4_real64, &
      'meridional trigonometric derivative on interior points')
  end subroutine test_meridional_trigonometric_field


  subroutine test_repeated_latitude_rejection
    integer, parameter :: nx = 2
    integer, parameter :: ny = 5
    integer, parameter :: nz = 2

    real(real64) :: latitude(ny)
    real(real64) :: field(nx, ny, nz)
    real(real64) :: derivative(nx, ny, nz)
    integer :: i
    integer :: k
    integer :: status

    latitude = [ &
      -0.6_real64, &
      -0.2_real64, &
      -0.2_real64, &
       0.3_real64, &
       0.8_real64]

    do k = 1, nz
      do i = 1, nx
        field(i, :, k) = sin(latitude) + real(i + k, real64)
      end do
    end do

    derivative = huge(0.0_real64)
    call meridionalDerivative(latitude, field, derivative, status)

    call check(status == -2, &
               'meridionalDerivative rejects repeated latitude coordinates')
  end subroutine test_repeated_latitude_rejection


  subroutine test_vertical_linear_fields
    integer :: nz

    do nz = 2, 12, 2
      call check_vertical_linear_case(nx=3, ny=2, nz=nz)
    end do
  end subroutine test_vertical_linear_fields


  subroutine check_vertical_linear_case(nx, ny, nz)
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    real(real64), allocatable :: pressure(:)
    real(real64), allocatable :: field(:,:,:)
    real(real64), allocatable :: derivative(:,:,:)
    real(real64), allocatable :: expected(:,:,:)
    real(real64), allocatable :: reversed_pressure(:)
    real(real64), allocatable :: reversed_field(:,:,:)
    real(real64), allocatable :: reversed_expected(:,:,:)
    real(real64), allocatable :: surface_pressure(:,:)
    integer :: i
    integer :: j
    integer :: k
    integer :: status
    character(80) :: description

    allocate(pressure(nz), field(nx, ny, nz), derivative(nx, ny, nz), &
             expected(nx, ny, nz), reversed_pressure(nz), &
             reversed_field(nx, ny, nz), reversed_expected(nx, ny, nz), &
             surface_pressure(nx, ny))

    do k = 1, nz
      pressure(k) = 10000.0_real64 &
        + 80000.0_real64 &
        * (real(k - 1, real64) / real(nz - 1, real64))**1.15_real64
    end do

    surface_pressure = 110000.0_real64

    do j = 1, ny
      do i = 1, nx
        field(i, j, :) = real(i + j, real64) * pressure + 7.0_real64
        expected(i, j, :) = real(i + j, real64)
      end do
    end do

    call verticalDerivative( &
      pressure, field, surface_pressure, derivative, status=status)

    write(description, '(A,I0)') 'vertical linear status, nz=', nz
    call check(status == 1, description)

    write(description, '(A,I0)') 'vertical derivative of a linear field, nz=', nz
    call check_array_close( &
      reshape(derivative, [size(derivative)]), &
      reshape(expected, [size(expected)]), &
      2.0e-13_real64, &
      2.0e-13_real64, &
      description)

    reversed_pressure = pressure(nz:1:-1)
    reversed_field = field(:, :, nz:1:-1)
    reversed_expected = expected(:, :, nz:1:-1)

    call verticalDerivative( &
      reversed_pressure, &
      reversed_field, &
      surface_pressure, &
      derivative, &
      status=status)

    write(description, '(A,I0)') 'vertical derivative on reversed levels, nz=', nz
    call check_array_close( &
      reshape(derivative, [size(derivative)]), &
      reshape(reversed_expected, [size(reversed_expected)]), &
      2.0e-13_real64, &
      2.0e-13_real64, &
      description)

    deallocate(pressure, field, derivative, expected, reversed_pressure, &
               reversed_field, reversed_expected, surface_pressure)
  end subroutine check_vertical_linear_case

end program test_met_derivative
