program test_met_integral
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Met, only : meridionalIntegral, verticalIntegral
    use :: test_support, only : check, check_array_close, finish_tests

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
            call check_meridional_constant_case(nx = 3 , &  !! IN
                                              & ny = ny, &  !! IN
                                              & nz = 2   )  !! IN
        enddo
    end subroutine test_meridional_constant_fields


    subroutine check_meridional_constant_case(nx, ny, nz)
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        integer, intent(in) :: nz

        real(real64), allocatable :: latitude(:)
        real(real64), allocatable :: field(:,:,:)
        real(real64), allocatable :: result(:,:)
        real(real64), allocatable :: expected(:,:)
        real(real64)              :: value_at_north
        real(real64)              :: value_at_south
        integer                   :: i
        integer                   :: j
        integer                   :: k
        integer                   :: status
        character(80)             :: description

        allocate(latitude(ny), field(nx, ny, nz), result(nx, nz), expected(nx, nz))

        do j = 1, ny
            latitude(j) = -0.9_real64 &
                + 1.8_real64 * real(j - 1, real64) / real(ny - 1, real64)
        enddo

        do k = 1, nz
            do i = 1, nx
                field(i, :, k) = real(i + k, real64)
            enddo
        enddo

        call meridionalIntegral(latitude(1:size(latitude,1))      , &  !! IN
                              & field(1:size(field,1),1:size(field,2),1:size(field,3))         , &  !! IN
                              & latitude(1)   , &  !! IN
                              & latitude(ny)  , &  !! IN
                              & result(1:size(result,1),1:size(result,2))        , &  !! OUT
                              & status        , &  !! OUT
                              & value_at_south, &  !! OUT
                              & value_at_north  )  !! OUT

        expected(1:size(expected,1),1:size(expected, &
            & 2)) = weighted_latitude_trapezoids(latitude(1:size(latitude,1)), &
                & field(1:size(field,1),1:size(field,2),1:size(field,3)), 1, ny)

        write(description, '(A,I0)') 'meridional integral status, ny=', ny
        call check(status == 1, &  !! IN
                 & description  )  !! IN

        write(description, '(A,I0)') 'meridional cos(latitude) weighting, ny=', ny
        call check_array_close(reshape(result(1:size(result,1),1:size(result,2)), [size(result)])    , &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2)), [size(expected)]), &  !! IN
                             & 2.0e-13_real64                     , &  !! IN
                             & 2.0e-13_real64                     , &  !! IN
                             & description                          )  !! IN

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
        integer      :: i
        integer      :: j
        integer      :: k
        integer      :: status

        do j = 1, ny
            latitude(j) = -1.0_real64 &
                + 2.0_real64 * (real(j - 1, real64) / real(ny - 1, real64))**1.1_real64
        enddo

        do k = 1, nz
            do i = 1, nx
                field(i, :, k) = real(i, real64) * sin(latitude(1:size(latitude,1))) &
                                              + real(k, real64) * cos(latitude(1:size(latitude,1)))
            enddo
        enddo

        call meridionalIntegral(latitude(1:size(latitude,1))        , &  !! IN
                              & field(1:size(field,1),1:size(field,2),1:size(field,3))           , &  !! IN
                              & latitude(3)     , &  !! IN
                              & latitude(ny - 2), &  !! IN
                              & result(1:size(result,1),1:size(result,2))          , &  !! OUT
                              & status            )  !! OUT

        expected(1:size(expected,1),1:size(expected, &
            & 2)) = weighted_latitude_trapezoids(latitude(1:size(latitude,1)), &
                & field(1:size(field,1),1:size(field,2),1:size(field,3)), 3, ny - 2)

        call check(status == 1                               , &  !! IN
                 & 'meridional trigonometric integral status'  )  !! IN
        call check_array_close(reshape(result(1:size(result,1),1:size(result,2)), &
            & [size(result)])                          , &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2)), &
                                 & [size(expected)])                      , &  !! IN
                             & 3.0e-13_real64                                           , &  !! IN
                             & 3.0e-13_real64                                           , &  !! IN
                             & 'weighted trapezoidal integral of sine and cosine fields'  )  !! IN
    end subroutine test_meridional_trigonometric_fields


    function weighted_latitude_trapezoids(latitude, field, first_index, last_index) &
            result(integral)
        real(real64), intent(in) :: latitude(:)
        real(real64), intent(in) :: field(:,:,:)
        integer, intent(in)      :: first_index
        integer, intent(in)      :: last_index

        real(real64) :: integral(size(field, 1), size(field, 3))
        integer      :: i
        integer      :: j
        integer      :: k

        integral(1:size(integral,1),1:size(integral,2)) = 0.0_real64

        do k = 1, size(field, 3)
            do i = 1, size(field, 1)
                do j = first_index, last_index - 1
                    integral(i, k) = integral(i, k) &
                        + 0.5_real64 &
                        * (field(i, j, k) * cos(latitude(j)) &
                              + field(i, j + 1, k) * cos(latitude(j + 1))) &
                        * (latitude(j + 1) - latitude(j))
                enddo
            enddo
        enddo
    end function weighted_latitude_trapezoids


    subroutine test_vertical_full_columns
        integer :: nz

        do nz = 2, 12, 2
            call check_vertical_full_column_case(nx = 3 , &  !! IN
                                               & ny = 2 , &  !! IN
                                               & nz = nz  )  !! IN
        enddo
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
        integer                   :: i
        integer                   :: j
        integer                   :: k
        integer                   :: status
        character(80)             :: description

        allocate(pressure(nz), field(nx, ny, nz), result(nx, ny), &
                          expected(nx, ny), surface_pressure(nx, ny))

        do k = 1, nz
            pressure(k) = 10000.0_real64 &
                + 80000.0_real64 * real(k - 1, real64) / real(nz - 1, real64)
        enddo

        do j = 1, ny
            do i = 1, nx
                field(i, j, :) = real(i + j, real64) + pressure(1:size(pressure,1)) / 100000.0_real64
            enddo
        enddo

        surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)) = pressure(nz)
        expected(1:size(expected,1),1:size(expected, &
            & 2)) = full_vertical_column_integral(pressure(1:size(pressure,1)), &
                & field(1:size(field,1),1:size(field,2),1:size(field,3)))

        call verticalIntegral(pressure(1:size(pressure,1))        , &  !! IN
                            & field(1:size(field,1),1:size(field,2),1:size(field,3))           , &  !! IN
                            & surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)), &  !! IN
                            & result(1:size(result,1),1:size(result,2))          , &  !! OUT
                            & status            )  !! OUT

        write(description, '(A,I0)') 'vertical integral status, nz=', nz
        call check(status == 1, &  !! IN
                 & description  )  !! IN

        write(description, '(A,I0)') 'vertical TOA triangle and trapezoids, nz=', nz
        call check_array_close(reshape(result(1:size(result,1),1:size(result,2)), [size(result)])    , &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2)), [size(expected)]), &  !! IN
                             & 3.0e-13_real64                     , &  !! IN
                             & 3.0e-10_real64                     , &  !! IN
                             & description                          )  !! IN

        deallocate(pressure, field, result, expected, surface_pressure)
    end subroutine check_vertical_full_column_case


    function full_vertical_column_integral(pressure, field) result(integral)
        real(real64), intent(in) :: pressure(:)
        real(real64), intent(in) :: field(:,:,:)

        real(real64) :: integral(size(field, 1), size(field, 2))
        integer      :: level

        integral(1:size(integral,1),1:size(integral,2)) = 0.5_real64 * pressure(1) * field(:, :, 1)

        do level = 1, size(pressure) - 1
            integral(1:size(integral,1),1:size(integral,2)) = integral(1:size(integral,1),1:size(integral,2)) &
                + 0.5_real64 * (pressure(level + 1) - pressure(level)) &
                * (field(:, :, level) + field(:, :, level + 1))
        enddo
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
        integer      :: i
        integer      :: j
        integer      :: status

        pressure(1:size(pressure,1)) = [ &
            10000.0_real64, &
            30000.0_real64, &
            60000.0_real64, &
            90000.0_real64]

        do j = 1, ny
            do i = 1, nx
                field(i, j, :) = real(i + j, real64) + pressure(1:size(pressure,1)) / 100000.0_real64
            enddo
        enddo

        surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)) = 75000.0_real64

        expected(1:size(expected,1),1:size(expected,2)) = &
                0.5_real64 * pressure(1) * field(:, :, 1) &
            + 0.5_real64 * (pressure(2) - pressure(1)) &
                * (field(:, :, 1) + field(:, :, 2)) &
            + 0.5_real64 * (pressure(3) - pressure(2)) &
                * (field(:, :, 2) + field(:, :, 3)) &
            + (surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)) - pressure(3)) * field(:, :, 3)

        call verticalIntegral(pressure(1:size(pressure,1))        , &  !! IN
                            & field(1:size(field,1),1:size(field,2),1:size(field,3))           , &  !! IN
                            & surface_pressure(1:size(surface_pressure,1),1:size(surface_pressure,2)), &  !! IN
                            & result(1:size(result,1),1:size(result,2))          , &  !! OUT
                            & status            )  !! OUT

        call check(status == 1                               , &  !! IN
                 & 'partial surface vertical integral status'  )  !! IN
        call check_array_close(reshape(result(1:size(result,1),1:size(result,2)), &
            & [size(result)])                                , &  !! IN
                             & reshape(expected(1:size(expected,1),1:size(expected,2)), &
                                 & [size(expected)])                            , &  !! IN
                             & 3.0e-13_real64                                                 , &  !! IN
                             & 3.0e-10_real64                                                 , &  !! IN
                             & 'partial surface layer uses the current upper-level value rule'  )  !! IN
    end subroutine test_vertical_partial_surface_layer

end program test_met_integral
