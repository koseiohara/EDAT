
program test_edat
    use, intrinsic :: iso_fortran_env, only : real64, int32

    use EDAT_Math, only : mean, variance, covariance, corrcoef, &
                          sum_hp, M_PI
    use EDAT_Float, only : isclose
    use EDAT_String, only : to_upper, to_lower
    use EDAT_Sort, only : quick_sort
    use EDAT_Met, only : potential_temperature, &
                         meridionalIntegral, verticalIntegral, &
                         zonalDerivative, meridionalDerivative, &
                         verticalDerivative

    implicit none

    integer :: failures

    failures = 0

    call test_math_small(failures)
    call test_math_large(failures)
    call test_float(failures)
    call test_string(failures)
    call test_sort(failures)
    call test_met(failures)

    if (failures /= 0) then
        write(*, '(a,i0)') 'FAILED tests: ', failures
        error stop 1
    endif

    write(*, '(a)') 'All EDAT tests passed.'

contains

    subroutine assert_true(condition, name, failures)
        logical, intent(in) :: condition
        character(*), intent(in) :: name
        integer, intent(inout) :: failures

        if (.not. condition) then
            failures = failures + 1
            write(*, '(a)') 'FAIL: '//trim(name)
        endif
    end subroutine assert_true


    subroutine assert_close(actual, expected, abs_tolerance, &
                            rel_tolerance, name, failures)
        real(real64), intent(in) :: actual
        real(real64), intent(in) :: expected
        real(real64), intent(in) :: abs_tolerance
        real(real64), intent(in) :: rel_tolerance
        character(*), intent(in) :: name
        integer, intent(inout) :: failures

        real(real64) :: allowed_error
        logical :: passed

        allowed_error = max( &
            abs_tolerance, &
            rel_tolerance * abs(expected) &
        )

        passed = abs(actual - expected) <= allowed_error

        call assert_true(passed, name, failures)

        if (.not. passed) then
            write(*, '(a,es24.16)') '  actual   = ', actual
            write(*, '(a,es24.16)') '  expected = ', expected
            write(*, '(a,es24.16)') '  error    = ', &
                abs(actual - expected)
            write(*, '(a,es24.16)') '  tolerance= ', &
                allowed_error
        endif
    end subroutine assert_close


    subroutine test_math_small(failures)
        integer, intent(inout) :: failures

        real(real64), allocatable :: empty(:)
        real(real64) :: x(2)
        real(real64) :: y(2)

        allocate(empty(0))

        x = [1.0_real64, 3.0_real64]
        y = [2.0_real64, 6.0_real64]

        call assert_close( &
            mean(empty), &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'mean(empty) == 0', &
            failures &
        )

        call assert_close( &
            variance(empty), &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'variance(empty) == 0', &
            failures &
        )

        call assert_close( &
            variance(empty, sample=.true.), &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'sample variance(empty) == 0', &
            failures &
        )

        call assert_close( &
            covariance(empty, empty), &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'covariance(empty,empty) == 0', &
            failures &
        )

        call assert_close( &
            covariance(empty, empty, sample=.true.), &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'sample covariance(empty,empty) == 0', &
            failures &
        )

        call assert_close( &
            corrcoef(empty, empty), &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'corrcoef(empty,empty) == 0', &
            failures &
        )

        call assert_close( &
            mean(x), &
            2.0_real64, &
            1.0e-14_real64, &
            1.0e-14_real64, &
            'mean small array', &
            failures &
        )

        call assert_close( &
            variance(x), &
            1.0_real64, &
            1.0e-14_real64, &
            1.0e-14_real64, &
            'population variance small array', &
            failures &
        )

        call assert_close( &
            variance(x, sample=.true.), &
            2.0_real64, &
            1.0e-14_real64, &
            1.0e-14_real64, &
            'sample variance small array', &
            failures &
        )

        call assert_close( &
            covariance(x, y), &
            2.0_real64, &
            1.0e-14_real64, &
            1.0e-14_real64, &
            'population covariance small array', &
            failures &
        )

        call assert_close( &
            covariance(x, y, sample=.true.), &
            4.0_real64, &
            1.0e-14_real64, &
            1.0e-14_real64, &
            'sample covariance small array', &
            failures &
        )

        call assert_close( &
            corrcoef(x, y), &
            1.0_real64, &
            1.0e-14_real64, &
            1.0e-14_real64, &
            'correlation small array', &
            failures &
        )

        call assert_close( &
            sum_hp([ &
                1.0_real64, &
                2.0_real64, &
                3.0_real64, &
                4.0_real64 &
            ]), &
            10.0_real64, &
            0.0_real64, &
            0.0_real64, &
            'sum_hp small array', &
            failures &
        )

        deallocate(empty)
    end subroutine test_math_small


    subroutine test_math_large(failures)
        integer, intent(inout) :: failures

        integer, parameter :: n = 10000

        integer :: i

        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y_positive(:)
        real(real64), allocatable :: y_negative(:)
        real(real64), allocatable :: harmonic(:)
        real(real64), allocatable :: cancellation(:)

        real(real64) :: n_real
        real(real64) :: expected_mean
        real(real64) :: expected_population_variance
        real(real64) :: expected_sample_variance
        real(real64) :: expected_positive_covariance
        real(real64) :: expected_negative_covariance
        real(real64) :: expected_harmonic
        real(real64) :: actual_harmonic

        allocate(x(n))
        allocate(y_positive(n))
        allocate(y_negative(n))
        allocate(harmonic(n))
        allocate(cancellation(n + 2))

        n_real = real(n, real64)

        do i = 1, n
            x(i) = real(i, real64)
        enddo

        y_positive = 2.0_real64 * x + 5.0_real64
        y_negative = -3.0_real64 * x + 7.0_real64

        expected_mean = (n_real + 1.0_real64) / 2.0_real64

        expected_population_variance = &
            (n_real * n_real - 1.0_real64) / 12.0_real64

        expected_sample_variance = &
            n_real * (n_real + 1.0_real64) / 12.0_real64

        expected_positive_covariance = &
            2.0_real64 * expected_population_variance

        expected_negative_covariance = &
            -3.0_real64 * expected_population_variance

        call assert_close( &
            mean(x), &
            expected_mean, &
            1.0e-10_real64, &
            1.0e-13_real64, &
            'mean 10000 elements', &
            failures &
        )

        call assert_close( &
            variance(x), &
            expected_population_variance, &
            1.0e-7_real64, &
            1.0e-13_real64, &
            'population variance 10000 elements', &
            failures &
        )

        call assert_close( &
            variance(x, sample=.true.), &
            expected_sample_variance, &
            1.0e-7_real64, &
            1.0e-13_real64, &
            'sample variance 10000 elements', &
            failures &
        )

        call assert_close( &
            covariance(x, y_positive), &
            expected_positive_covariance, &
            1.0e-7_real64, &
            1.0e-13_real64, &
            'positive covariance 10000 elements', &
            failures &
        )

        call assert_close( &
            covariance(x, y_positive, sample=.true.), &
            2.0_real64 * expected_sample_variance, &
            1.0e-7_real64, &
            1.0e-13_real64, &
            'positive sample covariance 10000 elements', &
            failures &
        )

        call assert_close( &
            covariance(x, y_negative), &
            expected_negative_covariance, &
            1.0e-7_real64, &
            1.0e-13_real64, &
            'negative covariance 10000 elements', &
            failures &
        )

        call assert_close( &
            covariance(x, y_negative, sample=.true.), &
            -3.0_real64 * expected_sample_variance, &
            1.0e-7_real64, &
            1.0e-13_real64, &
            'negative sample covariance 10000 elements', &
            failures &
        )

        call assert_close( &
            corrcoef(x, y_positive), &
            1.0_real64, &
            1.0e-12_real64, &
            1.0e-12_real64, &
            'positive correlation 10000 elements', &
            failures &
        )

        call assert_close( &
            corrcoef(x, y_negative), &
            -1.0_real64, &
            1.0e-12_real64, &
            1.0e-12_real64, &
            'negative correlation 10000 elements', &
            failures &
        )

        do i = 1, n
            harmonic(i) = 1.0_real64 / real(i, real64)
        enddo

        expected_harmonic = 0.0_real64

        do i = n, 1, -1
            expected_harmonic = expected_harmonic + harmonic(i)
        enddo

        actual_harmonic = sum_hp(harmonic)

        call assert_close( &
            actual_harmonic, &
            expected_harmonic, &
            1.0e-13_real64, &
            1.0e-14_real64, &
            'sum_hp harmonic series 10000 elements', &
            failures &
        )

        cancellation = 1.0_real64
        cancellation(1) = 1.0e16_real64
        cancellation(2) = -1.0e16_real64

        call assert_close( &
            sum_hp(cancellation), &
            n_real, &
            1.0e-10_real64, &
            0.0_real64, &
            'sum_hp cancellation 10002 elements', &
            failures &
        )

        deallocate(x)
        deallocate(y_positive)
        deallocate(y_negative)
        deallocate(harmonic)
        deallocate(cancellation)
    end subroutine test_math_large


    subroutine test_float(failures)
        integer, intent(inout) :: failures

        call assert_true( &
            isclose(1.0_real64, 1.0_real64 + 1.0e-14_real64), &
            'isclose relative tolerance', &
            failures &
        )

        call assert_true( &
            .not. isclose(0.0_real64, 1.0e-8_real64), &
            'isclose zero without absolute tolerance', &
            failures &
        )

        call assert_true( &
            isclose( &
                0.0_real64, &
                1.0e-8_real64, &
                abs_tol=1.0e-7_real64 &
            ), &
            'isclose absolute tolerance', &
            failures &
        )
    end subroutine test_float


    subroutine test_string(failures)
        integer, intent(inout) :: failures

        call assert_true( &
            to_upper('Abc xyz 123') == 'ABC XYZ 123', &
            'to_upper', &
            failures &
        )

        call assert_true( &
            to_lower('AbC XYZ 123') == 'abc xyz 123', &
            'to_lower', &
            failures &
        )
    end subroutine test_string


    subroutine test_sort(failures)
        integer, intent(inout) :: failures

        integer(int32) :: a(5)
        real(real64) :: b(4)

        a = [ &
            3_int32, &
            -1_int32, &
            2_int32, &
            2_int32, &
            0_int32 &
        ]

        b = [ &
            3.0_real64, &
            -1.0_real64, &
            2.0_real64, &
            0.0_real64 &
        ]

        call quick_sort(size(a), a)
        call quick_sort(size(b), b)

        call assert_true( &
            all(a == [ &
                -1_int32, &
                0_int32, &
                2_int32, &
                2_int32, &
                3_int32 &
            ]), &
            'integer quick_sort', &
            failures &
        )

        call assert_true( &
            all(b == [ &
                -1.0_real64, &
                0.0_real64, &
                2.0_real64, &
                3.0_real64 &
            ]), &
            'real quick_sort', &
            failures &
        )
    end subroutine test_sort


    subroutine test_met(failures)
        integer, intent(inout) :: failures

        real(real64) :: theta

        real(real64) :: lat1(1)
        real(real64) :: field_lat1(1, 1, 1)
        real(real64) :: out_lat1(1, 1)

        real(real64) :: lev1(1)
        real(real64) :: field_lev1(1, 1, 1)
        real(real64) :: psfc(1, 1)
        real(real64) :: out_lev1(1, 1)

        real(real64) :: lev_dup(3)
        real(real64) :: field_dup(1, 1, 3)
        real(real64) :: out_dup(1, 1)

        real(real64) :: lon(4)
        real(real64) :: f_lon(4, 1, 1)
        real(real64) :: d_lon(4, 1, 1)

        real(real64) :: lat(3)
        real(real64) :: f_lat(1, 3, 1)
        real(real64) :: d_lat(1, 3, 1)

        real(real64) :: lev(3)
        real(real64) :: f_lev(1, 1, 3)
        real(real64) :: d_lev(1, 1, 3)

        integer :: status

        theta = potential_temperature( &
            300.0_real64, &
            100000.0_real64 &
        )

        call assert_close( &
            theta, &
            300.0_real64, &
            1.0e-12_real64, &
            1.0e-14_real64, &
            'potential temperature at reference pressure', &
            failures &
        )

        lat1 = 0.0_real64
        field_lat1 = 1.0_real64

        call meridionalIntegral( &
            lat1, &
            field_lat1, &
            0.0_real64, &
            0.0_real64, &
            out_lat1, &
            status &
        )

        call assert_true( &
            status == -3, &
            'meridional integral short axis status', &
            failures &
        )

        lev1 = 1000.0_real64
        field_lev1 = 1.0_real64
        psfc = 1000.0_real64

        call verticalIntegral( &
            lev1, &
            field_lev1, &
            psfc, &
            out_lev1, &
            status &
        )

        call assert_true( &
            status == -3, &
            'vertical integral short axis status', &
            failures &
        )

        lev_dup = [ &
            100.0_real64, &
            200.0_real64, &
            200.0_real64 &
        ]

        field_dup = 1.0_real64
        psfc = 300.0_real64

        call verticalIntegral( &
            lev_dup, &
            field_dup, &
            psfc, &
            out_dup, &
            status &
        )

        call assert_true( &
            status == -2, &
            'vertical integral duplicate axis status', &
            failures &
        )

        lon = [ &
            0.0_real64, &
            0.5_real64 * real(M_PI, real64), &
            real(M_PI, real64), &
            1.5_real64 * real(M_PI, real64) &
        ]

        f_lon(:, 1, 1) = sin(lon)

        call zonalDerivative( &
            lon, &
            f_lon, &
            d_lon, &
            periodic=.true., &
            status=status &
        )

        call assert_true( &
            status > 0, &
            'zonal derivative status', &
            failures &
        )

        call assert_true( &
            maxval(abs(d_lon(:, 1, 1) - cos(lon))) &
                < 0.37_real64, &
            'zonal derivative sine', &
            failures &
        )

        lat = [ &
            -0.5_real64, &
            0.0_real64, &
            0.5_real64 &
        ]

        f_lat(1, :, 1) = lat

        call meridionalDerivative( &
            lat, &
            f_lat, &
            d_lat, &
            status &
        )

        call assert_true( &
            status > 0, &
            'meridional derivative status', &
            failures &
        )

        call assert_true( &
            maxval(abs(d_lat(1, :, 1) - 1.0_real64)) &
                < 1.0e-12_real64, &
            'meridional derivative linear', &
            failures &
        )

        lev = [ &
            100.0_real64, &
            200.0_real64, &
            300.0_real64 &
        ]

        f_lev(1, 1, :) = lev
        psfc = 350.0_real64

        call verticalDerivative( &
            lev, &
            f_lev, &
            psfc, &
            d_lev, &
            -9999.0_real64, &
            status &
        )

        call assert_true( &
            status > 0, &
            'vertical derivative status', &
            failures &
        )

        call assert_true( &
            maxval(abs(d_lev(1, 1, :) - 1.0_real64)) &
                < 1.0e-12_real64, &
            'vertical derivative linear', &
            failures &
        )
    end subroutine test_met

end program test_edat



