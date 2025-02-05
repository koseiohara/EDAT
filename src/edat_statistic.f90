module EDAT_Statistic

    implicit none

    private :: rk
    public :: qnorm, corrcoef, covariance, variance, mean, sum_hp

    integer, parameter :: rk = 8
    real(rk), parameter :: qnorm(77)=[0.0125334695_rk, &  !! 01
                                    & 0.0250689082_rk, &  !! 02
                                    & 0.0376082876_rk, &  !! 03
                                    & 0.0501535834_rk, &  !! 04
                                    & 0.0627067779_rk, &  !! 05
                                    & 0.0752698620_rk, &  !! 06
                                    & 0.0878448378_rk, &  !! 07
                                    & 0.1004337205_rk, &  !! 08
                                    & 0.1130385406_rk, &  !! 09
                                    & 0.1256613468_rk, &  !! 10
                                    & 0.1383042079_rk, &  !! 11
                                    & 0.1509692154_rk, &  !! 12
                                    & 0.1636584862_rk, &  !! 13
                                    & 0.1763741647_rk, &  !! 14
                                    & 0.1891184262_rk, &  !! 15
                                    & 0.2018934791_rk, &  !! 16
                                    & 0.2147015680_rk, &  !! 17
                                    & 0.2275449766_rk, &  !! 18
                                    & 0.2404260311_rk, &  !! 19
                                    & 0.2533471031_rk, &  !! 20
                                    & 0.2663106132_rk, &  !! 21
                                    & 0.2793190344_rk, &  !! 22
                                    & 0.2923748962_rk, &  !! 23
                                    & 0.3054807880_rk, &  !! 24
                                    & 0.3186393639_rk, &  !! 25
                                    & 0.3318533464_rk, &  !! 26
                                    & 0.3451255314_rk, &  !! 27
                                    & 0.3584587932_rk, &  !! 28
                                    & 0.3718560893_rk, &  !! 29
                                    & 0.3853204664_rk, &  !! 30
                                    & 0.3988550656_rk, &  !! 31
                                    & 0.4124631294_rk, &  !! 32
                                    & 0.4261480078_rk, &  !! 33
                                    & 0.4399131656_rk, &  !! 34
                                    & 0.4537621901_rk, &  !! 35
                                    & 0.4676987991_rk, &  !! 36
                                    & 0.4817268495_rk, &  !! 37
                                    & 0.4958503473_rk, &  !! 38
                                    & 0.5100734569_rk, &  !! 39
                                    & 0.5244005127_rk, &  !! 40
                                    & 0.5388360302_rk, &  !! 41
                                    & 0.5533847195_rk, &  !! 42
                                    & 0.5680514983_rk, &  !! 43
                                    & 0.5828415072_rk, &  !! 44
                                    & 0.5977601260_rk, &  !! 45
                                    & 0.6128129910_rk, &  !! 46
                                    & 0.6280060144_rk, &  !! 47
                                    & 0.6433454053_rk, &  !! 48
                                    & 0.6588376927_rk, &  !! 49
                                    & 0.6744897501_rk, &  !! 50
                                    & 0.6903088239_rk, &  !! 51
                                    & 0.7063025628_rk, &  !! 52
                                    & 0.7224790519_rk, &  !! 53
                                    & 0.7388468491_rk, &  !! 54
                                    & 0.7554150263_rk, &  !! 55
                                    & 0.7721932141_rk, &  !! 56
                                    & 0.7891916526_rk, &  !! 57
                                    & 0.8064212470_rk, &  !! 58
                                    & 0.8238936303_rk, &  !! 59
                                    & 0.8416212335_rk, &  !! 60
                                    & 0.8596173642_rk, &  !! 61
                                    & 0.8778962950_rk, &  !! 62
                                    & 0.8964733640_rk, &  !! 63
                                    & 0.9153650878_rk, &  !! 64
                                    & 0.9345892910_rk, &  !! 65
                                    & 0.9541652531_rk, &  !! 66
                                    & 0.9741138770_rk, &  !! 67
                                    & 0.9944578832_rk, &  !! 68
                                    & 1.0152220332_rk, &  !! 69
                                    & 1.0364333894_rk, &  !! 70
                                    & 1.0581216176_rk, &  !! 71
                                    & 1.0803193408_rk, &  !! 72
                                    & 1.1030625561_rk, &  !! 73
                                    & 1.1263911290_rk, &  !! 74
                                    & 1.1503493803_rk, &  !! 75
                                    & 1.1749867920_rk, &  !! 76
                                    & 1.2003588580_rk &  !! 77
                                    & ]

    interface corrcoef
        module procedure &
            & corrcoef_sp, &
            & corrcoef_dp, &
            & corrcoef_qp
    end interface corrcoef

    interface covariance
        module procedure &
            & covariance_sp, &
            & covariance_dp, &
            & covariance_qp
    end interface covariance

    interface variance
        module procedure &
            & variance_sp, &
            & variance_dp, &
            & variance_qp
    end interface variance

    interface mean
        module procedure &
            & mean_sp, &
            & mean_dp, &
            & mean_qp
    end interface mean

    interface sum_hp
        module procedure &
            & sum_hp_sp, &
            & sum_hp_dp, &
            & sum_hp_qp
    end interface sum_hp

    contains


    pure function corrcoef_sp(n, array1, array2) result(output)
        integer, parameter :: rkloc = 4
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)

        real(rkloc) :: output
        real(rkloc) :: variance1
        real(rkloc) :: variance2
        real(rkloc) :: covar

        variance1 = variance(n, array1(1:n))
        variance2 = variance(n, array2(1:n))
        covar = covariance(n, array1(1:n), array2(1:n))

        output = covar / sqrt(variance1*variance2)

    end function corrcoef_sp


    pure function corrcoef_dp(n, array1, array2) result(output)
        integer, parameter :: rkloc = 8
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)

        real(rkloc) :: output
        real(rkloc) :: variance1
        real(rkloc) :: variance2
        real(rkloc) :: covar

        variance1 = variance(n, array1(1:n))
        variance2 = variance(n, array2(1:n))
        covar = covariance(n, array1(1:n), array2(1:n))

        output = covar / sqrt(variance1*variance2)

    end function corrcoef_dp


    pure function corrcoef_qp(n, array1, array2) result(output)
        integer, parameter :: rkloc = 16
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)

        real(rkloc) :: output
        real(rkloc) :: variance1
        real(rkloc) :: variance2
        real(rkloc) :: covar

        variance1 = variance(n, array1(1:n))
        variance2 = variance(n, array2(1:n))
        covar = covariance(n, array1(1:n), array2(1:n))

        output = covar / sqrt(variance1*variance2)

    end function corrcoef_qp


    pure function covariance_sp(n, array1, array2, sample) result(output)
        integer, parameter :: rkloc = 4
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)
        logical , intent(in), optional :: sample

        real(rkloc) :: output
        real(rkloc) :: mean1
        real(rkloc) :: mean2
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        mean1 = mean(n, array1(1:n))
        mean2 = mean(n, array2(1:n))

        output = dot_product(array1(1:n)-mean1, array2(1:n)-mean2) / real(sample_num, kind=rkloc)

    end function covariance_sp


    pure function covariance_dp(n, array1, array2, sample) result(output)
        integer, parameter :: rkloc = 8
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)
        logical , intent(in), optional :: sample

        real(rkloc) :: output
        real(rkloc) :: mean1
        real(rkloc) :: mean2
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        mean1 = mean(n, array1(1:n))
        mean2 = mean(n, array2(1:n))

        output = dot_product(array1(1:n)-mean1, array2(1:n)-mean2) / real(sample_num, kind=rkloc)

    end function covariance_dp


    pure function covariance_qp(n, array1, array2, sample) result(output)
        integer, parameter :: rkloc = 16
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)
        logical , intent(in), optional :: sample

        real(rkloc) :: output
        real(rkloc) :: mean1
        real(rkloc) :: mean2
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        mean1 = mean(n, array1(1:n))
        mean2 = mean(n, array2(1:n))

        output = dot_product(array1(1:n)-mean1, array2(1:n)-mean2) / real(sample_num, kind=rkloc)

    end function covariance_qp


    pure function variance_sp(n, array, sample) result(output)
        integer, parameter :: rkloc = 4
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array(n)
        logical , intent(in), optional :: sample

        real(rkloc) :: output
        real(rkloc) :: array_mean
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        array_mean = mean(n, array)
        output = sum_hp(n, (array(1:n) - array_mean)**2) / real(sample_num, kind=rkloc)

    end function variance_sp


    pure function variance_dp(n, array, sample) result(output)
        integer, parameter :: rkloc = 8
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array(n)
        logical , intent(in), optional :: sample

        real(rkloc) :: output
        real(rkloc) :: array_mean
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        array_mean = mean(n, array)
        output = sum_hp(n, (array(1:n) - array_mean)**2) / real(sample_num, kind=rkloc)

    end function variance_dp


    pure function variance_qp(n, array, sample) result(output)
        integer, parameter :: rkloc = 16
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array(n)
        logical , intent(in), optional :: sample

        real(rkloc) :: output
        real(rkloc) :: array_mean
        integer :: sample_num

        if (present(sample) .AND. sample) then
            sample_num = n-1
        else
            sample_num = n
        endif

        array_mean = mean(n, array)
        output = sum_hp(n, (array(1:n) - array_mean)**2) / real(sample_num, kind=rkloc)

    end function variance_qp


    pure function mean_sp(n, array) result(output)
        integer, parameter :: rkloc = 4
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        real(rkloc) :: output

        output = sum_hp(n, array(1:n)) / real(n, kind=rkloc)

    end function mean_sp


    pure function mean_dp(n, array) result(output)
        integer, parameter :: rkloc = 8
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        real(rkloc) :: output

        output = sum_hp(n, array(1:n)) / real(n, kind=rkloc)

    end function mean_dp


    pure function mean_qp(n, array) result(output)
        integer, parameter :: rkloc = 16
        integer , intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        real(rkloc) :: output

        output = sum_hp(n, array(1:n)) / real(n, kind=rkloc)

    end function mean_qp


    pure function sum_hp_sp(n, array) result(output)
        integer, parameter :: rkloc = 4
        integer    , intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        integer, parameter :: part_size = 2
        real(rkloc) :: workspace(n)
        real(rkloc) :: output
        integer :: i
        integer :: left
        integer :: right
        integer :: total_size

        workspace(1:n) = array(1:n)
        total_size = n
        do while (total_size /= 1)
            i = 1
            do 
                left = (i-1)*part_size + 1
                right = min(i*part_size, total_size)
                workspace(i) = sum(workspace(left:right))
                if (right==total_size)then
                    total_size = i
                    exit
                endif
                i = i + 1
            enddo
        enddo

        output = workspace(1)

    end function sum_hp_sp


    pure function sum_hp_dp(n, array) result(output)
        integer, parameter :: rkloc = 8
        integer    , intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        integer, parameter :: part_size = 2
        real(rkloc) :: workspace(n)
        real(rkloc) :: output
        integer :: i
        integer :: left
        integer :: right
        integer :: total_size

        workspace(1:n) = array(1:n)
        total_size = n
        do while (total_size /= 1)
            i = 1
            do 
                left = (i-1)*part_size + 1
                right = min(i*part_size, total_size)
                workspace(i) = sum(workspace(left:right))
                if (right==total_size)then
                    total_size = i
                    exit
                endif
                i = i + 1
            enddo
        enddo

        output = workspace(1)

    end function sum_hp_dp


    pure function sum_hp_qp(n, array) result(output)
        integer, parameter :: rkloc = 16
        integer    , intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        integer, parameter :: part_size = 2
        real(rkloc) :: workspace(n)
        real(rkloc) :: output
        integer :: i
        integer :: left
        integer :: right
        integer :: total_size

        workspace(1:n) = array(1:n)
        total_size = n
        do while (total_size /= 1)
            i = 1
            do 
                left = (i-1)*part_size + 1
                right = min(i*part_size, total_size)
                workspace(i) = sum(workspace(left:right))
                if (right==total_size)then
                    total_size = i
                    exit
                endif
                i = i + 1
            enddo
        enddo

        output = workspace(1)

    end function sum_hp_qp


end module EDAT_Statistic

