module EDAT_Math

    implicit none

    private :: rk
    public :: M_E, M_LOG2E, M_LOG10E, M_LN2, M_LN10, M_PI, M_PI_2, M_PI_4, M_1_PI, M_2_PI, M_2_SQRTPI, M_SQRT2, M_SQRT1_2, &
            & qnorm, corrcoef, covariance, variance, mean, sum_hp

    integer , parameter :: rk = 16

    real(rk), parameter :: M_E        = 2.718281828459045235360287471352662498_rk       ! e
    real(rk), parameter :: M_LOG2E    = 1.442695040888963407359924681001892137_rk       ! log_2 e
    real(rk), parameter :: M_LOG10E   = 0.434294481903251827651128918916605082_rk       ! log_10 e
    real(rk), parameter :: M_LN2      = 0.693147180559945309417232121458176568_rk       ! log_e 2
    real(rk), parameter :: M_LN10     = 2.302585092994045684017991454684364208_rk       ! log_e 10
    real(rk), parameter :: M_PI       = 3.141592653589793238462643383279502884_rk       ! pi
    real(rk), parameter :: M_PI_2     = 1.570796326794896619231321691639751442_rk       ! pi/2
    real(rk), parameter :: M_PI_4     = 0.785398163397448309615660845819875721_rk       ! pi/4
    real(rk), parameter :: M_1_PI     = 0.318309886183790671537767526745028724_rk       ! 1/pi
    real(rk), parameter :: M_2_PI     = 0.636619772367581343075535053490057448_rk       ! 2/pi
    real(rk), parameter :: M_2_SQRTPI = 1.128379167095512573896158903121545172_rk       ! 2/sqrt(pi)
    real(rk), parameter :: M_SQRT2    = 1.414213562373095048801688724209698079_rk       ! sqrt(2)
    real(rk), parameter :: M_SQRT1_2  = 0.707106781186547524400844362104849039_rk       ! 1/sqrt(2)

    real(rk), parameter :: qnorm(99) = [0.01253346951_rk, &  !! 01
                                      & 0.02506890826_rk, &  !! 02
                                      & 0.03760828766_rk, &  !! 03
                                      & 0.05015358346_rk, &  !! 04
                                      & 0.06270677794_rk, &  !! 05
                                      & 0.07526986210_rk, &  !! 06
                                      & 0.08784483790_rk, &  !! 07
                                      & 0.10043372051_rk, &  !! 08
                                      & 0.11303854064_rk, &  !! 09
                                      & 0.12566134686_rk, &  !! 10
                                      & 0.13830420796_rk, &  !! 10
                                      & 0.15096921550_rk, &  !! 11
                                      & 0.16365848623_rk, &  !! 12
                                      & 0.17637416478_rk, &  !! 13
                                      & 0.18911842627_rk, &  !! 15
                                      & 0.20189347914_rk, &  !! 16
                                      & 0.21470156800_rk, &  !! 17
                                      & 0.22754497664_rk, &  !! 18
                                      & 0.24042603114_rk, &  !! 19
                                      & 0.25334710314_rk, &  !! 20
                                      & 0.26631061320_rk, &  !! 21
                                      & 0.27931903445_rk, &  !! 22
                                      & 0.29237489623_rk, &  !! 23
                                      & 0.30548078810_rk, &  !! 24
                                      & 0.31863936396_rk, &  !! 25
                                      & 0.33185334644_rk, &  !! 26
                                      & 0.34512553147_rk, &  !! 27
                                      & 0.35845879325_rk, &  !! 28
                                      & 0.37185608939_rk, &  !! 29
                                      & 0.38532046641_rk, &  !! 30
                                      & 0.39885506564_rk, &  !! 31
                                      & 0.41246312944_rk, &  !! 32
                                      & 0.42614800784_rk, &  !! 33
                                      & 0.43991316567_rk, &  !! 34
                                      & 0.45376219017_rk, &  !! 35
                                      & 0.46769879911_rk, &  !! 36
                                      & 0.48172684958_rk, &  !! 37
                                      & 0.49585034735_rk, &  !! 38
                                      & 0.51007345697_rk, &  !! 39
                                      & 0.52440051271_rk, &  !! 40
                                      & 0.53883603028_rk, &  !! 41
                                      & 0.55338471956_rk, &  !! 42
                                      & 0.56805149834_rk, &  !! 43
                                      & 0.58284150727_rk, &  !! 44
                                      & 0.59776012604_rk, &  !! 45
                                      & 0.61281299102_rk, &  !! 46
                                      & 0.62800601444_rk, &  !! 47
                                      & 0.64334540539_rk, &  !! 48
                                      & 0.65883769274_rk, &  !! 49
                                      & 0.67448975020_rk, &  !! 50
                                      & 0.69030882393_rk, &  !! 51
                                      & 0.70630256284_rk, &  !! 52
                                      & 0.72247905193_rk, &  !! 53
                                      & 0.73884684919_rk, &  !! 54
                                      & 0.75541502636_rk, &  !! 55
                                      & 0.77219321419_rk, &  !! 56
                                      & 0.78919165266_rk, &  !! 57
                                      & 0.80642124702_rk, &  !! 58
                                      & 0.82389363034_rk, &  !! 59
                                      & 0.84162123357_rk, &  !! 60
                                      & 0.85961736424_rk, &  !! 61
                                      & 0.87789629505_rk, &  !! 62
                                      & 0.89647336400_rk, &  !! 63
                                      & 0.91536508784_rk, &  !! 64
                                      & 0.93458929107_rk, &  !! 65
                                      & 0.95416525315_rk, &  !! 66
                                      & 0.97411387706_rk, &  !! 67
                                      & 0.99445788321_rk, &  !! 68
                                      & 1.01522203322_rk, &  !! 69
                                      & 1.03643338949_rk, &  !! 70
                                      & 1.05812161768_rk, &  !! 71
                                      & 1.08031934081_rk, &  !! 72
                                      & 1.10306255620_rk, &  !! 73
                                      & 1.12639112904_rk, &  !! 74
                                      & 1.15034938038_rk, &  !! 75
                                      & 1.17498679207_rk, &  !! 76
                                      & 1.20035885803_rk, &  !! 77
                                      & 1.22652812004_rk, &  !! 78
                                      & 1.25356543847_rk, &  !! 79
                                      & 1.28155156554_rk, &  !! 80
                                      & 1.31057911217_rk, &  !! 81
                                      & 1.34075503369_rk, &  !! 82
                                      & 1.37220380900_rk, &  !! 83
                                      & 1.40507156031_rk, &  !! 84
                                      & 1.43953147094_rk, &  !! 85
                                      & 1.47579102818_rk, &  !! 86
                                      & 1.51410188762_rk, &  !! 87
                                      & 1.55477359460_rk, &  !! 88
                                      & 1.59819313992_rk, &  !! 89
                                      & 1.64485362695_rk, &  !! 90
                                      & 1.69539771027_rk, &  !! 91
                                      & 1.75068607125_rk, &  !! 92
                                      & 1.81191067295_rk, &  !! 93
                                      & 1.88079360815_rk, &  !! 94
                                      & 1.95996398454_rk, &  !! 95
                                      & 2.05374891063_rk, &  !! 96
                                      & 2.17009037758_rk, &  !! 97
                                      & 2.32634787404_rk, &  !! 98
                                      & 2.57582930355_rk  ]  !! 99

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
        integer, intent(in) :: n
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
        integer, intent(in) :: n
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
        integer, intent(in) :: n
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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)
        logical, intent(in), optional :: sample

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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)
        logical, intent(in), optional :: sample

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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array1(n)
        real(rkloc), intent(in) :: array2(n)
        logical, intent(in), optional :: sample

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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array(n)
        logical, intent(in), optional :: sample

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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array(n)
        logical, intent(in), optional :: sample

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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array(n)
        logical, intent(in), optional :: sample

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
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        real(rkloc) :: output

        output = sum_hp(n, array(1:n)) / real(n, kind=rkloc)

    end function mean_sp


    pure function mean_dp(n, array) result(output)
        integer, parameter :: rkloc = 8
        integer, intent(in) :: n
        real(rkloc), intent(in) :: array(n)

        real(rkloc) :: output

        output = sum_hp(n, array(1:n)) / real(n, kind=rkloc)

    end function mean_dp


    pure function mean_qp(n, array) result(output)
        integer, parameter :: rkloc = 16
        integer, intent(in) :: n
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
            left = 1
            right = min(part_size, total_size)
            do 
                workspace(i) = sum(workspace(left:right))
                if (right==total_size)then
                    total_size = i
                    exit
                endif
                i = i + 1
                left = right + 1
                right = min(left+part_size-1, total_size)
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
            left = 1
            right = min(part_size, total_size)
            do 
                workspace(i) = sum(workspace(left:right))
                if (right==total_size)then
                    total_size = i
                    exit
                endif
                i = i + 1
                left = right + 1
                right = min(left+part_size-1, total_size)
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
            left = 1
            right = min(part_size, total_size)
            do 
                workspace(i) = sum(workspace(left:right))
                if (right==total_size)then
                    total_size = i
                    exit
                endif
                i = i + 1
                left = right + 1
                right = min(left+part_size-1, total_size)
            enddo
        enddo

        output = workspace(1)

    end function sum_hp_qp


end module EDAT_Math

