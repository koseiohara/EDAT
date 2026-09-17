program test_met_derivative
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Math, only : M_PI
    use :: EDAT_Met, only : meridionalDerivative, verticalDerivative, zonalDerivative
    use :: test_support, only : check, check_array_close, finish_tests

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
            call check_zonal_linear_case(nx  , &  !! IN
                                       & ny = 3, &  !! IN
                                       & nz = 2  )  !! IN
        enddo
    end subroutine test_zonal_linear_fields


    subroutine check_zonal_linear_case(nx, ny, nz)
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        integer, intent(in) :: nz

        real(real64), allocatable :: longitude(:)
        real(real64), allocatable :: field(:,:,:)
        real(real64), allocatable :: derivative(:,:,:)
        real(real64), allocatable :: expected(:,:,:)
        integer                   :: i
        integer                   :: j
        integer                   :: k
        integer                   :: status
        character(80)             :: description

        allocate(longitude(nx), field(nx, ny, nz), derivative(nx, ny, nz), &
                          expected(nx, ny, nz))

        do i = 1, nx
            longitude(i) = -1.0_real64 &
                + 2.0_real64 * real(i - 1, real64) / real(nx - 1, real64)
        enddo

        do k = 1, nz
            do j = 1, ny
                field(:, j, k) = (2.0_real64 + real(j, real64)) * longitude(1:size(longitude,1)) &
                                              + real(k, real64)
                expected(:, j, k) = 2.0_real64 + real(j, real64)
            enddo
        enddo

        call zonalDerivative(longitude(1:size(longitude,1))       , &  !! IN
                           & field(1:size(field,1),1:size(field,2),1:size(field,3))           , &  !! IN
                           & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3))      , &  !! OUT
                           & periodic = .FALSE., &  !! IN
                           & status = status     )  !! OUT

        write(description, '(A,I0)') 'zonal linear status, nx=', nx
        call check(status == 1, &  !! IN
                 & description  )  !! IN

        write(description, '(A,I0)') 'zonal derivative of a linear field, nx=', nx
        call check_array_close(reshape(derivative(1:size(derivative,1),1:size(derivative,2), &
            & 1:size(derivative,3)), [size(derivative)]), &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2), &
                                 & 1:size(expected,3)), [size(expected)])    , &  !! IN
                             & 1.0e-13_real64                         , &  !! IN
                             & 1.0e-13_real64                         , &  !! IN
                             & description                              )  !! IN

        deallocate(longitude, field, derivative, expected)
    end subroutine check_zonal_linear_case


    subroutine test_zonal_periodic_trigonometric_fields
        integer :: nx

        do nx = 8, 64, 8
            call check_zonal_trigonometric_case(nx  , &  !! IN
                                              & ny = 2, &  !! IN
                                              & nz = 2  )  !! IN
        enddo
    end subroutine test_zonal_periodic_trigonometric_fields


    subroutine check_zonal_trigonometric_case(nx, ny, nz)
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        integer, intent(in) :: nz

        real(real64), allocatable :: longitude(:)
        real(real64), allocatable :: field(:,:,:)
        real(real64), allocatable :: derivative(:,:,:)
        real(real64), allocatable :: expected(:,:,:)
        real(real64)              :: grid_spacing
        real(real64)              :: maximum_error
        integer                   :: i
        integer                   :: j
        integer                   :: k
        integer                   :: status
        character(80)             :: description

        allocate(longitude(nx), field(nx, ny, nz), derivative(nx, ny, nz), &
                          expected(nx, ny, nz))

        grid_spacing = 2.0_real64 * pi / real(nx, real64)
        do i = 1, nx
            longitude(i) = grid_spacing * real(i - 1, real64)
        enddo

        do k = 1, nz
            do j = 1, ny
                field(:, j, k) = sin(longitude(1:size(longitude,1))) &
                                              + 0.3_real64 * cos(2.0_real64 * longitude(1:size(longitude,1)))
                expected(:, j, k) = cos(longitude(1:size(longitude,1))) &
                                                    - 0.6_real64 * sin(2.0_real64 * longitude(1:size(longitude,1)))
            enddo
        enddo

        call zonalDerivative(longitude(1:size(longitude,1))    , &  !! IN
                           & field(1:size(field,1),1:size(field,2),1:size(field,3))        , &  !! IN
                           & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3))   , &  !! OUT
                           & status = status  )  !! OUT
        maximum_error = maxval(abs(derivative(1:size(derivative,1),1:size(derivative,2), &
            & 1:size(derivative,3)) - expected(1:size(expected,1),1:size(expected,2),1:size(expected,3))))

        write(description, '(A,I0)') 'periodic zonal status, nx=', nx
        call check(status == 1, &  !! IN
                 & description  )  !! IN

        write(description, '(A,I0)') 'second-order periodic zonal accuracy, nx=', nx
        call check(maximum_error < 5.0_real64 * grid_spacing**2, &  !! IN
                 & description                                   )  !! IN

        deallocate(longitude, field, derivative, expected)
    end subroutine check_zonal_trigonometric_case


    subroutine test_meridional_linear_fields
        integer :: ny

        do ny = 2, 19, 4
            call check_meridional_linear_case(nx = 3 , &  !! IN
                                            & ny = ny, &  !! IN
                                            & nz = 2   )  !! IN
        enddo
    end subroutine test_meridional_linear_fields


    subroutine check_meridional_linear_case(nx, ny, nz)
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        integer, intent(in) :: nz

        real(real64), allocatable :: latitude(:)
        real(real64), allocatable :: field(:,:,:)
        real(real64), allocatable :: derivative(:,:,:)
        real(real64), allocatable :: expected(:,:,:)
        integer                   :: i
        integer                   :: j
        integer                   :: k
        integer                   :: status
        character(80)             :: description

        allocate(latitude(ny), field(nx, ny, nz), derivative(nx, ny, nz), &
                          expected(nx, ny, nz))

        do j = 1, ny
            latitude(j) = -0.8_real64 &
                + 1.6_real64 * (real(j - 1, real64) / real(ny - 1, real64))**1.2_real64
        enddo

        do k = 1, nz
            do i = 1, nx
                field(i, :, k) = (1.0_real64 + real(i, real64)) * latitude(1:size(latitude,1)) &
                                              + real(k, real64)
                expected(i, :, k) = 1.0_real64 + real(i, real64)
            enddo
        enddo

        call meridionalDerivative(latitude(1:size(latitude,1))  , &  !! IN
                                & field(1:size(field,1),1:size(field,2),1:size(field,3))     , &  !! IN
                                & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3)), &  !! OUT
                                & status      )  !! OUT

        write(description, '(A,I0)') 'meridional linear status, ny=', ny
        call check(status == 1, &  !! IN
                 & description  )  !! IN

        write(description, '(A,I0)') 'meridional derivative of a linear field, ny=', ny
        call check_array_close(reshape(derivative(1:size(derivative,1),1:size(derivative,2), &
            & 1:size(derivative,3)), [size(derivative)]), &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2), &
                                 & 1:size(expected,3)), [size(expected)])    , &  !! IN
                             & 2.0e-13_real64                         , &  !! IN
                             & 2.0e-13_real64                         , &  !! IN
                             & description                              )  !! IN

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
        integer      :: i
        integer      :: j
        integer      :: k
        integer      :: status

        do j = 1, ny
            latitude(j) = -pi / 3.0_real64 &
                + (2.0_real64 * pi / 3.0_real64) &
                * real(j - 1, real64) / real(ny - 1, real64)
        enddo

        do k = 1, nz
            do i = 1, nx
                field(i, :, k) = sin(latitude(1:size(latitude,1))) &
                                              + 0.2_real64 * cos(2.0_real64 * latitude(1:size(latitude,1)))
                expected(i, :, k) = cos(latitude(1:size(latitude,1))) &
                                                    - 0.4_real64 * sin(2.0_real64 * latitude(1:size(latitude,1)))
            enddo
        enddo

        call meridionalDerivative(latitude(1:size(latitude,1))  , &  !! IN
                                & field(1:size(field,1),1:size(field,2),1:size(field,3))     , &  !! IN
                                & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3)), &  !! OUT
                                & status      )  !! OUT

        call check(status == 1                      , &  !! IN
                 & 'meridional trigonometric status'  )  !! IN
        call check(maxval(abs(derivative(:, 2:ny - 1, :) - expected(:, 2:ny - 1, :))) < 5.0e-4_real64, &  !! IN
                 & 'meridional trigonometric derivative on interior points'                            )  !! IN
    end subroutine test_meridional_trigonometric_field


    subroutine test_repeated_latitude_rejection
        integer, parameter :: nx = 2
        integer, parameter :: ny = 5
        integer, parameter :: nz = 2

        real(real64) :: latitude(ny)
        real(real64) :: field(nx, ny, nz)
        real(real64) :: derivative(nx, ny, nz)
        integer      :: i
        integer      :: k
        integer      :: status

        latitude(1:size(latitude,1)) = [ &
            -0.6_real64, &
            -0.2_real64, &
            -0.2_real64, &
              0.3_real64, &
              0.8_real64]

        do k = 1, nz
            do i = 1, nx
                field(i, :, k) = sin(latitude(1:size(latitude,1))) + real(i + k, real64)
            enddo
        enddo

        derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3)) = huge(0.0_real64)
        call meridionalDerivative(latitude(1:size(latitude,1))  , &  !! IN
                                & field(1:size(field,1),1:size(field,2),1:size(field,3))     , &  !! IN
                                & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3)), &  !! OUT
                                & status      )  !! OUT

        call check(status == -2                                                , &  !! IN
                 & 'meridionalDerivative rejects repeated latitude coordinates'  )  !! IN
    end subroutine test_repeated_latitude_rejection


    subroutine test_vertical_linear_fields
        integer :: nz

        do nz = 2, 12, 2
            call check_vertical_linear_case(nx = 3 , &  !! IN
                                          & ny = 2 , &  !! IN
                                          & nz = nz  )  !! IN
        enddo
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
        integer                   :: i
        integer                   :: j
        integer                   :: k
        integer                   :: status
        character(80)             :: description

        allocate(pressure(nz), field(nx, ny, nz), derivative(nx, ny, nz), &
                          expected(nx, ny, nz), reversed_pressure(nz), &
                          reversed_field(nx, ny, nz), reversed_expected(nx, ny, nz), &
                          surface_pressure(nx, ny))

        do k = 1, nz
            pressure(k) = 10000.0_real64 &
                + 80000.0_real64 &
                * (real(k - 1, real64) / real(nz - 1, real64))**1.15_real64
        enddo

        surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)) = 110000.0_real64

        do j = 1, ny
            do i = 1, nx
                field(i, j, :) = real(i + j, real64) * pressure(1:size(pressure,1)) + 7.0_real64
                expected(i, j, :) = real(i + j, real64)
            enddo
        enddo

        call verticalDerivative(pressure(1:size(pressure,1))        , &  !! IN
                              & field(1:size(field,1),1:size(field,2),1:size(field,3))           , &  !! IN
                              & surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)), &  !! IN
                              & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3))      , &  !! OUT
                              & status = status     )  !! OUT

        write(description, '(A,I0)') 'vertical linear status, nz=', nz
        call check(status == 1, &  !! IN
                 & description  )  !! IN

        write(description, '(A,I0)') 'vertical derivative of a linear field, nz=', nz
        call check_array_close(reshape(derivative(1:size(derivative,1),1:size(derivative,2), &
            & 1:size(derivative,3)), [size(derivative)]), &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2), &
                                 & 1:size(expected,3)), [size(expected)])    , &  !! IN
                             & 2.0e-13_real64                         , &  !! IN
                             & 2.0e-13_real64                         , &  !! IN
                             & description                              )  !! IN

        reversed_pressure(1:size(reversed_pressure,1)) = pressure(nz:1:-1)
        reversed_field(1:size(reversed_field,1),1:size(reversed_field,2),1:size(reversed_field,3)) = field(:, :, nz:1:-1)
        reversed_expected(1:size(reversed_expected,1),1:size(reversed_expected,2), &
            & 1:size(reversed_expected,3)) = expected(:, :, nz:1:-1)

        call verticalDerivative(reversed_pressure(1:size(reversed_pressure,1)), &  !! IN
                              & reversed_field(1:size(reversed_field,1),1:size(reversed_field, &
                                  & 2),1:size(reversed_field,3))   , &  !! IN
                              & surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)) , &  !! IN
                              & derivative(1:size(derivative,1),1:size(derivative,2),1:size(derivative,3))       , &  !! OUT
                              & status = status      )  !! OUT

        write(description, '(A,I0)') 'vertical derivative on reversed levels, nz=', nz
        call check_array_close(reshape(derivative(1:size(derivative,1),1:size(derivative,2), &
            & 1:size(derivative,3)), [size(derivative)])              , &  !! IN
                             & reshape(reversed_expected(1:size(reversed_expected,1), &
                                 & 1:size(reversed_expected,2),1:size(reversed_expected,3)), &
                                     & [size(reversed_expected)]), &  !! IN
                             & 2.0e-13_real64                                       , &  !! IN
                             & 2.0e-13_real64                                       , &  !! IN
                             & description                                            )  !! IN

        deallocate(pressure, field, derivative, expected, reversed_pressure, &
                   reversed_field, reversed_expected, surface_pressure)
    end subroutine check_vertical_linear_case

end program test_met_derivative
