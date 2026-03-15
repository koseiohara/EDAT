module EDAT_Met

    use, intrinsic :: iso_fortran_env, only : rk=>real128

    use EDAT_Math    , only : sum_hp
    use integral_sp  , only : meridionalIntegral_sp, verticalIntegral_sp
    use integral_dp  , only : meridionalIntegral_dp, verticalIntegral_dp
    use integral_qp  , only : meridionalIntegral_qp, verticalIntegral_qp
    use derivative_sp, only : zonalDerivative_sp, meridionalDerivative_sp, verticalDerivative_sp
    use derivative_dp, only : zonalDerivative_dp, meridionalDerivative_dp, verticalDerivative_dp
    use derivative_qp, only : zonalDerivative_qp, meridionalDerivative_qp, verticalDerivative_qp

    implicit none

    private
    public :: GRAV, EarthRadius, GasConstant, Cp, Cv, Lq               , &
            & potential_temperature                                    , &
            & meridionalIntegral, verticalIntegral                     , &
            & zonalDerivative, meridionalDerivative, verticalDerivative

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
                       & meridionalIntegral_dp, &
                       & meridionalIntegral_qp
    end interface meridionalIntegral

    interface verticalIntegral
        module procedure verticalIntegral_sp, &
                       & verticalIntegral_dp, &
                       & verticalIntegral_qp
    end interface verticalIntegral

    interface zonalDerivative
        module procedure zonalDerivative_sp, &
                       & zonalDerivative_dp, &
                       & zonalDerivative_qp
    end interface zonalDerivative

    interface meridionalDerivative
        module procedure meridionalDerivative_sp, &
                       & meridionalDerivative_dp, &
                       & meridionalDerivative_qp
    end interface meridionalDerivative

    interface verticalDerivative
        module procedure verticalDerivative_sp, &
                       & verticalDerivative_dp, &
                       & verticalDerivative_qp
    end interface verticalDerivative

    contains


    pure elemental function potential_temperature_sp(T, P) result(output)
        use, intrinsic :: iso_fortran_env, only : lrk=>real32
        real(lrk), intent(in) :: T
        real(lrk), intent(in) :: P

        real(lrk), parameter :: P0=1.E+5_lrk
        real(lrk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_sp


    pure elemental function potential_temperature_dp(T, P) result(output)
        use, intrinsic :: iso_fortran_env, only : lrk=>real64
        real(lrk), intent(in) :: T
        real(lrk), intent(in) :: P

        real(lrk), parameter :: P0=1.E+5_lrk
        real(lrk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_dp


    pure elemental function potential_temperature_qp(T, P) result(output)
        use, intrinsic :: iso_fortran_env, only : lrk=>real128
        real(lrk), intent(in) :: T
        real(lrk), intent(in) :: P

        real(lrk), parameter :: P0=1.E+5_lrk
        real(lrk) :: output

        output = T*(P0/P)**(GasConstant/Cp)

    end function potential_temperature_qp

end module EDAT_Met

