module EDAT_Met

    use EDAT_Math    , only : sum_hp
    use integral_sp  , only : meridionalIntegral_sp, verticalIntegral_sp
    use integral_dp  , only : meridionalIntegral_dp, verticalIntegral_dp
    use derivative_sp, only : zonalDerivative_sp, meridionalDerivative_sp, verticalDerivative_sp
    use derivative_dp, only : zonalDerivative_dp, meridionalDerivative_dp, verticalDerivative_dp

    implicit none

    private
    public :: GRAV, EarthRadius, GasConstant, Cp, Cv, Lq               , &
            & potential_temperature                                    , &
            & meridionalIntegral, verticalIntegral                     , &
            & zonalDerivative, meridionalDerivative, verticalDerivative

    integer , parameter :: rk = 16

    real(rk), parameter :: GRAV        = 9.80665_rk         ! Gravitational Acceleration [m/s^2]
    real(rk), parameter :: EarthRadius = 6.3710E+6_rk       ! Radius of the Earth [m]

    real(rk), parameter :: GasConstant = 287.04_rk          ! Gas Constant for Dry Air [J/K/kg] #used for p=rhoRT
    real(rk), parameter :: Cp          = 1004._rk           ! Specific Heat for Dry Air at Constant Pressure [J/K/kg]
    real(rk), parameter :: Cv          = Cp-GasConstant     ! Specific Heat for Dry Air at Constant Volume [J/K/kg]
    real(rk), parameter :: Lq          = 2.507E+6_rk        ! Latent Heat of vaporication [J/kg]


    interface potential_temperature
        module procedure &
            & potential_temperature_sp, &
            & potential_temperature_dp, &
            & potential_temperature_qp
    end interface potential_temperature

    interface meridionalIntegral
        module procedure meridionalIntegral_sp, &
                       & meridionalIntegral_dp
    end interface meridionalIntegral

    interface verticalIntegral
        module procedure verticalIntegral_sp, &
                       & verticalIntegral_dp
    end interface verticalIntegral

    interface zonalDerivative
        module procedure zonalDerivative_sp, &
                       & zonalDerivative_dp
    end interface zonalDerivative

    interface meridionalDerivative
        module procedure meridionalDerivative_sp, &
                       & meridionalDerivative_dp
    end interface meridionalDerivative

    interface verticalDerivative
        module procedure verticalDerivative_sp, &
                       & verticalDerivative_dp
    end interface verticalDerivative

    contains


    pure elemental function potential_temperature_sp(T, P) result(output)
        integer, parameter :: lrk = 4
        real(lrk), intent(in) :: T
        real(lrk), intent(in) :: P

        real(lrk), parameter :: P0=1.E+5_lrk
        real(lrk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_sp


    pure elemental function potential_temperature_dp(T, P) result(output)
        integer, parameter :: lrk = 8
        real(lrk), intent(in) :: T
        real(lrk), intent(in) :: P

        real(lrk), parameter :: P0=1.E+5_lrk
        real(lrk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_dp


    pure elemental function potential_temperature_qp(T, P) result(output)
        integer, parameter :: lrk = 16
        real(lrk), intent(in) :: T
        real(lrk), intent(in) :: P

        real(lrk), parameter :: P0=1.E+5_lrk
        real(lrk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_qp

end module EDAT_Met

