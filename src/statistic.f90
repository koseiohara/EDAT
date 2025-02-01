module statistic

    implicit none

    private
    public :: corrcoef, covariance, variance, mean


    interface corrcoef
        module procedure &
            & corrcoef_sp, &
            & corrcoef_dp
    end interface corrcoef

    interface covariance
        module procedure &
            & covariance_sp, &
            & covariance_dp
    end interface covariance

    interface variance
        module procedure &
            & variance_sp, &
            & variance_dp
    end interface variance

    interface mean
        module procedure &
            & mean_sp, &
            & mean_dp
    end interface mean

    contains


    pure function corrcoef_sp(n, array1, array2) result(output)
        integer, parameter :: rk = 4
        integer , intent(in) :: n
        real(rk), intent(in) :: array1(n)
        real(rk), intent(in) :: array2(n)

        real(rk) :: output
        real(rk) :: variance1
        real(rk) :: variance2
        real(rk) :: covar

        variance1 = variance(n, array1(1:n))
        variance2 = variance(n, array2(1:n))
        covar = covariance(n, array1(1:n), array2(1:n))

        output = covar / sqrt(variance1*variance2)

    end function corrcoef_sp


    pure function corrcoef_dp(n, array1, array2) result(output)
        integer, parameter :: rk = 8
        integer , intent(in) :: n
        real(rk), intent(in) :: array1(n)
        real(rk), intent(in) :: array2(n)

        real(rk) :: output
        real(rk) :: variance1
        real(rk) :: variance2
        real(rk) :: covar

        variance1 = variance(n, array1(1:n))
        variance2 = variance(n, array2(1:n))
        covar = covariance(n, array1(1:n), array2(1:n))

        output = covar / sqrt(variance1*variance2)

    end function corrcoef_dp


    pure function covariance_sp(n, array1, array2, sample) result(output)
        integer, parameter :: rk = 4
        integer , intent(in) :: n
        real(rk), intent(in) :: array1(n)
        real(rk), intent(in) :: array2(n)
        logical , intent(in), optional :: sample

        real(rk) :: output
        real(rk) :: mean1
        real(rk) :: mean2
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        mean1 = mean(n, array1(1:n))
        mean2 = mean(n, array2(1:n))

        output = dot_product(array1(1:n)-mean1, array2(1:n)-mean2) / real(sample_num, kind=rk)

    end function covariance_sp


    pure function covariance_dp(n, array1, array2, sample) result(output)
        integer, parameter :: rk = 8
        integer , intent(in) :: n
        real(rk), intent(in) :: array1(n)
        real(rk), intent(in) :: array2(n)
        logical , intent(in), optional :: sample

        real(rk) :: output
        real(rk) :: mean1
        real(rk) :: mean2
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        mean1 = mean(n, array1(1:n))
        mean2 = mean(n, array2(1:n))

        output = dot_product(array1(1:n)-mean1, array2(1:n)-mean2) / real(sample_num, kind=rk)

    end function covariance_dp


    pure function variance_sp(n, array, sample) result(output)
        integer, parameter :: rk = 4
        integer , intent(in) :: n
        real(rk), intent(in) :: array(n)
        logical , intent(in), optional :: sample

        real(rk) :: output
        real(rk) :: array_mean
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        array_mean = mean(n, array)
        output = sum((array(1:n) - array_mean)**2) / real(sample_num, kind=rk)

    end function variance_sp


    pure function variance_dp(n, array, sample) result(output)
        integer, parameter :: rk = 8
        integer , intent(in) :: n
        real(rk), intent(in) :: array(n)
        logical , intent(in), optional :: sample

        real(rk) :: output
        real(rk) :: array_mean
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        array_mean = mean(n, array)
        output = sum((array(1:n) - array_mean)**2) / real(sample_num, kind=rk)

    end function variance_dp


    pure function mean_sp(n, array) result(output)
        integer, parameter :: rk = 4
        integer , intent(in) :: n
        real(rk), intent(in) :: array(n)

        real(rk) :: output

        output = sum(array(1:n)) / real(n, kind=rk)

    end function mean_sp


    pure function mean_dp(n, array) result(output)
        integer, parameter :: rk = 8
        integer , intent(in) :: n
        real(rk), intent(in) :: array(n)

        real(rk) :: output

        output = sum(array(1:n)) / real(n, kind=rk)

    end function mean_dp


end module statistic

