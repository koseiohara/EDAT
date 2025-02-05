module EDAT_Physics

    implicit none

    private :: rk

    integer , parameter :: rk = 16
    real(rk), parameter :: P_E        = 2.718281828459045235360287471352662498_rk       ! e
    real(rk), parameter :: P_LOG2E    = 1.442695040888963407359924681001892137_rk       ! log_2 e
    real(rk), parameter :: P_LOG10E   = 0.434294481903251827651128918916605082_rk       ! log_10 e
    real(rk), parameter :: P_LN2      = 0.693147180559945309417232121458176568_rk       ! log_e 2
    real(rk), parameter :: P_LN10     = 2.302585092994045684017991454684364208_rk       ! log_e 10
    real(rk), parameter :: P_PI       = 3.141592653589793238462643383279502884_rk       ! pi
    real(rk), parameter :: P_PI_2     = 1.570796326794896619231321691639751442_rk       ! pi/2
    real(rk), parameter :: P_PI_4     = 0.785398163397448309615660845819875721_rk       ! pi/4
    real(rk), parameter :: P_1_PI     = 0.318309886183790671537767526745028724_rk       ! 1/pi
    real(rk), parameter :: P_2_PI     = 0.636619772367581343075535053490057448_rk       ! 2/pi
    real(rk), parameter :: P_2_SQRTPI = 1.128379167095512573896158903121545172_rk       ! 2/sqrt(pi)
    real(rk), parameter :: P_SQRT2    = 1.414213562373095048801688724209698079_rk       ! sqrt(2)
    real(rk), parameter :: P_SQRT1_2  = 0.707106781186547524400844362104849039_rk       ! 1/sqrt(2)

    real(rk), parameter :: GRAV        = 9.80665_rk         ! Gravitational Acceleration [m/s^2]
    real(rk), parameter :: EarthRadius = 6.3710E+6_rk       ! Radius of the Earth [m]

    real(rk), parameter :: GasConstant = 287.04_rk          ! Gas Constant for Dry Air [J/K/kg] #used for p=rhoRT
    real(rk), parameter :: Cp          = 1004._rk           ! Specific Heat for Dry Air at Constant Pressure [J/K/kg]
    real(rk), parameter :: Cv          = GasConstant-Cp     ! Specific Heat for Dry Air at Constant Volume [J/K/kg]
    real(rk), parameter :: Lq          = 2.507E+6_rk        ! Latent Heat of vaporication [J/kg]


    interface potential_temperature
        module procedure &
            & potential_temperature_sp, &
            & potential_temperature_dp, &
            & potential_temperature_qp
    end interface potential_temperature
    
    contains


    pure elemental function potential_temperature_sp(T, P) result(output)
        integer, parameter :: rkloc = 4
        real(rkloc), intent(in) :: T
        real(rkloc), intent(in) :: P

        real(rkloc), parameter :: P0=1.E+5_rkloc
        real(rkloc) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_sp


    pure elemental function potential_temperature_dp(T, P) result(output)
        integer, parameter :: rkloc = 8
        real(rkloc), intent(in) :: T
        real(rkloc), intent(in) :: P

        real(rkloc), parameter :: P0=1.E+5_rkloc
        real(rkloc) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_dp


    pure elemental function potential_temperature_qp(T, P) result(output)
        integer, parameter :: rkloc = 16
        real(rkloc), intent(in) :: T
        real(rkloc), intent(in) :: P

        real(rkloc), parameter :: P0=1.E+5_rkloc
        real(rkloc) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_qp


end module EDAT_Physics


