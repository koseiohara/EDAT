module EDAT_Met

    implicit none

    private :: rk

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


end module EDAT_Met


