module EDAT_Met

    use EDAT_Math, only : sum_hp

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

    interface meridionalIntegral
        module procedure &
            & meridionalIntegral_sp, &
            & meridionalIntegral_dp, &
            & meridionalIntegral_qp
    end interface meridionalIntegral
    
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


    pure subroutine meridionalIntegral_sp(nx, ny, nz, lat, field, south, north, output, valid_south, valid_north)
        integer  , parameter   :: lrk = 4
        integer  , intent(in)  :: nx
        integer  , intent(in)  :: ny
        integer  , intent(in)  :: nz
        real(lrk), intent(in)  :: lat(ny)
        real(lrk), intent(in)  :: field(nx,ny,nz)
        real(lrk), intent(in)  :: south
        real(lrk), intent(in)  :: north
        real(lrk), intent(out) :: output(nx,nz)
        real(lrk), intent(out), optional :: valid_south
        real(lrk), intent(out), optional :: valid_north

        real(lrk), allocatable :: work_field(:,:)

        real(lrk) :: costbl(ny)
        real(lrk) :: dlat(ny)
        real(lrk) :: dl
        real(lrk) :: c0
        real(lrk) :: c1
        integer :: south_idx
        integer :: north_idx
        integer :: jl
        integer :: ju
        integer :: i
        integer :: j
        integer :: k

        south_idx = minloc(abs(lat(1:ny) - south), dim=1)
        north_idx = minloc(abs(lat(1:ny) - north), dim=1)

        if (present(valid_south)) then
            valid_south = lat(south_idx)
        endif
        if (present(valid_north)) then
            valid_north = lat(north_idx)
        endif

        if (south_idx < north_idx) then
            jl = south_idx
            ju = north_idx
        else if (south_idx > north_idx) then
            jl = north_idx
            ju = south_idx
        else
            output(1:nx,1:nz) = 0._lrk
            return
        endif

        costbl(jl:ju) = cos(lat(jl:ju)) * 0.5_lrk

        if (lat(2) < lat(1)) then
            costbl(jl:ju) = - costbl(jl:ju)
        endif

        allocate(work_field(jl:ju-1,nx))

        dlat(jl:ju-1) = lat(jl+1:ju) - lat(jl:ju-1)
        do k = 1, nz
            do j = jl, ju-1
                dl = dlat(j)
                c0 = costbl(j)
                c1 = costbl(j+1)
                do i = 1, nx
                    work_field(j,i) = (field(i,j+1,k)*c1 + field(i,j,k)*c0) * dl
                enddo
            enddo
            do i = 1, nx
                output(i,k) = sum_hp(ju-jl, work_field(jl:ju-1,i))
            enddo
        enddo

        deallocate(work_field)

    end subroutine meridionalIntegral_sp


    pure subroutine meridionalIntegral_dp(nx, ny, nz, lat, field, south, north, output, valid_south, valid_north)
        integer  , parameter   :: lrk = 8
        integer  , intent(in)  :: nx
        integer  , intent(in)  :: ny
        integer  , intent(in)  :: nz
        real(lrk), intent(in)  :: lat(ny)
        real(lrk), intent(in)  :: field(nx,ny,nz)
        real(lrk), intent(in)  :: south
        real(lrk), intent(in)  :: north
        real(lrk), intent(out) :: output(nx,nz)
        real(lrk), intent(out), optional :: valid_south
        real(lrk), intent(out), optional :: valid_north

        real(lrk), allocatable :: work_field(:,:)

        real(lrk) :: costbl(ny)
        real(lrk) :: dlat(ny)
        real(lrk) :: dl
        real(lrk) :: c0
        real(lrk) :: c1
        integer :: south_idx
        integer :: north_idx
        integer :: jl
        integer :: ju
        integer :: i
        integer :: j
        integer :: k

        south_idx = minloc(abs(lat(1:ny) - south), dim=1)
        north_idx = minloc(abs(lat(1:ny) - north), dim=1)

        if (present(valid_south)) then
            valid_south = lat(south_idx)
        endif
        if (present(valid_north)) then
            valid_north = lat(north_idx)
        endif

        if (south_idx < north_idx) then
            jl = south_idx
            ju = north_idx
        else if (south_idx > north_idx) then
            jl = north_idx
            ju = south_idx
        else
            output(1:nx,1:nz) = 0._lrk
            return
        endif

        costbl(jl:ju) = cos(lat(jl:ju)) * 0.5_lrk

        if (lat(2) < lat(1)) then
            costbl(jl:ju) = - costbl(jl:ju)
        endif

        allocate(work_field(jl:ju-1,nx))

        dlat(jl:ju-1) = lat(jl+1:ju) - lat(jl:ju-1)
        do k = 1, nz
            do j = jl, ju-1
                dl = dlat(j)
                c0 = costbl(j)
                c1 = costbl(j+1)
                do i = 1, nx
                    work_field(j,i) = (field(i,j+1,k)*c1 + field(i,j,k)*c0) * dl
                enddo
            enddo
            do i = 1, nx
                output(i,k) = sum_hp(ju-jl, work_field(jl:ju-1,i))
            enddo
        enddo

        deallocate(work_field)

    end subroutine meridionalIntegral_dp


    pure subroutine meridionalIntegral_qp(nx, ny, nz, lat, field, south, north, output, valid_south, valid_north)
        integer  , parameter   :: lrk = 16
        integer  , intent(in)  :: nx
        integer  , intent(in)  :: ny
        integer  , intent(in)  :: nz
        real(lrk), intent(in)  :: lat(ny)
        real(lrk), intent(in)  :: field(nx,ny,nz)
        real(lrk), intent(in)  :: south
        real(lrk), intent(in)  :: north
        real(lrk), intent(out) :: output(nx,nz)
        real(lrk), intent(out), optional :: valid_south
        real(lrk), intent(out), optional :: valid_north

        real(lrk), allocatable :: work_field(:,:)

        real(lrk) :: costbl(ny)
        real(lrk) :: dlat(ny)
        real(lrk) :: dl
        real(lrk) :: c0
        real(lrk) :: c1
        integer :: south_idx
        integer :: north_idx
        integer :: jl
        integer :: ju
        integer :: i
        integer :: j
        integer :: k

        south_idx = minloc(abs(lat(1:ny) - south), dim=1)
        north_idx = minloc(abs(lat(1:ny) - north), dim=1)

        if (present(valid_south)) then
            valid_south = lat(south_idx)
        endif
        if (present(valid_north)) then
            valid_north = lat(north_idx)
        endif

        if (south_idx < north_idx) then
            jl = south_idx
            ju = north_idx
        else if (south_idx > north_idx) then
            jl = north_idx
            ju = south_idx
        else
            output(1:nx,1:nz) = 0._lrk
            return
        endif

        costbl(jl:ju) = cos(lat(jl:ju)) * 0.5_lrk

        if (lat(2) < lat(1)) then
            costbl(jl:ju) = - costbl(jl:ju)
        endif

        allocate(work_field(jl:ju-1,nx))

        dlat(jl:ju-1) = lat(jl+1:ju) - lat(jl:ju-1)
        do k = 1, nz
            do j = jl, ju-1
                dl = dlat(j)
                c0 = costbl(j)
                c1 = costbl(j+1)
                do i = 1, nx
                    work_field(j,i) = (field(i,j+1,k)*c1 + field(i,j,k)*c0) * dl
                enddo
            enddo
            do i = 1, nx
                output(i,k) = sum_hp(ju-jl, work_field(jl:ju-1,i))
            enddo
        enddo

        deallocate(work_field)

    end subroutine meridionalIntegral_qp

end module EDAT_Met

