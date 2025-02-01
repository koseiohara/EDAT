module physics

    implicit none

    real(8), parameter :: P_E        = 2.718281828459045235360287471352662498_8     ! e
    real(8), parameter :: P_LOG2E    = 1.442695040888963407359924681001892137_8     ! log_2 e
    real(8), parameter :: P_LOG10E   = 0.434294481903251827651128918916605082_8     ! log_10 e
    real(8), parameter :: P_LN2      = 0.693147180559945309417232121458176568_8     ! log_e 2
    real(8), parameter :: P_LN10     = 2.302585092994045684017991454684364208_8     ! log_e 10
    real(8), parameter :: P_PI       = 3.141592653589793238462643383279502884_8     ! pi
    real(8), parameter :: P_PI_2     = 1.570796326794896619231321691639751442_8     ! pi/2
    real(8), parameter :: P_PI_4     = 0.785398163397448309615660845819875721_8     ! pi/4
    real(8), parameter :: P_1_PI     = 0.318309886183790671537767526745028724_8     ! 1/pi
    real(8), parameter :: P_2_PI     = 0.636619772367581343075535053490057448_8     ! 2/pi
    real(8), parameter :: P_2_SQRTPI = 1.128379167095512573896158903121545172_8     ! 2/sqrt(pi)
    real(8), parameter :: P_SQRT2    = 1.414213562373095048801688724209698079_8     ! sqrt(2)
    real(8), parameter :: P_SQRT1_2  = 0.707106781186547524400844362104849039_8     ! 1/sqrt(2)

    real(8), parameter :: GRAV        = 9.80665_8       ! Gravitational Acceleration [m/s^2]
    real(8), parameter :: EarthRadius = 6.3710E+6_8     ! Radius of the Earth [m]

    real(8), parameter :: GasConstant = 287.04_8        ! Gas Constant for Dry Air [J/K/kg] #used for p=rhoRT
    real(8), parameter :: Cp          = 1004._8         ! Specific Heat for Dry Air at Constant Pressure [J/K/kg]
    real(8), parameter :: Cv          = GasConstant-Cp  ! Specific Heat for Dry Air at Constant Volume [J/K/kg]
    real(8), parameter :: Lq          = 2.507E+6_8      ! Latent Heat of vaporication [J/kg]


    interface potential_temperature
        module procedure &
            & potential_temperature_sp, &
            & potential_temperature_dp
    end interface potential_temperature
    
    contains


    pure elemental function potential_temperature_sp(T, P) result(output)
        integer, parameter :: rk = 4
        real(rk), intent(in) :: T
        real(rk), intent(in) :: P

        real(rk), parameter :: P0=1.E+5_rk
        real(rk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_sp


    pure elemental function potential_temperature_dp(T, P) result(output)
        integer, parameter :: rk = 8
        real(rk), intent(in) :: T
        real(rk), intent(in) :: P

        real(rk), parameter :: P0=1.E+5_rk
        real(rk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_dp


end module physics


