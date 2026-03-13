


module derivative_qp

    use, intrinsic :: iso_fortran_env, only : rk=>real128

    use EDAT_Math, only : M_PI

    implicit none

    private
    public :: zonalDerivative_qp, meridionalDerivative_qp, verticalDerivative_qp

    contains


    !! status:
    !!   -1 ... inconsistency of array size
    !!   -2 ... derivative was not computed because derivative axis is too small
    !!   -3 ... derivative axis is not a monotone sequence
    !!   any positive ... computed successfully
    pure subroutine zonalDerivative_qp(lon, input, output, periodic, status)
        real(rk), intent(in)  :: lon(:)
        real(rk), intent(in)  :: input(:,:,:)
        real(rk), intent(out) :: output(:,:,:)
        logical , intent(in) , optional :: periodic
        integer , intent(out), optional :: status

        real(rk) :: dlon(size(lon))
        real(rk) :: work_dl(size(lon))
        real(rk) :: work_du(size(lon))
        real(rk) :: coef(size(lon))
        integer :: i
        integer :: j
        integer :: k
        integer :: nx
        integer :: ny
        integer :: nz
        integer :: work_status
        logical :: dim_consist
        logical :: work_periodic

        nx = size(input, 1)
        ny = size(input, 2)
        nz = size(input, 3)

        if (present(periodic)) then
            work_periodic = periodic
        else
            work_periodic = .TRUE.
        endif

        if (nx <= 2) then
            if (work_periodic) then
                if (present(status)) then
                    status = -2
                endif
                return
            else
                if (nx <= 1) then
                    if (present(status)) then
                        status = -2
                    endif
                    return
                endif
            endif
        endif

        dim_consist = (size(lon   , 1) == nx) .AND. &
                    & (size(output, 1) == nx) .AND. &
                    & (size(output, 2) == ny) .AND. &
                    & (size(output, 3) == nz)

        if (.NOT. dim_consist) then
            if (present(status)) then
                status = -1
            endif
            return
        endif
        work_status = 1

        dlon(2:nx) = lon(2:nx) - lon(1:nx-1)

        if (all(dlon(2:nx) > 0)) then
            dlon(1) = lon(1) - lon(nx) + M_PI*2._rk
        else if (all(dlon(2:nx) < 0)) then
            dlon(1) = lon(1) - lon(nx) - M_PI*2._rk
        else
            if (present(status)) then
                status = -3
            endif
            return
        endif

        if (work_periodic) then
            work_du(1) = dlon(2) * dlon(2)
            work_dl(1) = dlon(1) * dlon(1)
            coef(1)    = 1._rk / (dlon(1)*dlon(2) * (dlon(1) + dlon(2)))
            do i = 2, nx-1
                work_du(i) = dlon(i+1) * dlon(i+1)
                work_dl(i) = dlon(i  ) * dlon(i  )
                coef(i)    = 1._rk / (dlon(i)*dlon(i+1) * (dlon(i) + dlon(i+1)))
            enddo
            work_du(nx) = dlon(1 ) * dlon(1 )
            work_dl(nx) = dlon(nx) * dlon(nx)
            coef(nx)    = 1._rk / (dlon(nx)*dlon(1) * (dlon(nx) + dlon(1)))

            do k = 1, nz
                do j = 1, ny
                    output(1,j,k) = ( work_du(1) * (input(1,j,k) - input(nx,j,k))   &
                                &   + work_dl(1) * (input(2,j,k) - input(1 ,j,k)) ) &
                                &   * coef(1)
                    do i = 2, nx-1
                        output(i,j,k) = ( work_du(i) * (input(i  ,j,k) - input(i-1,j,k))   &
                                    &   + work_dl(i) * (input(i+1,j,k) - input(i  ,j,k)) ) &
                                    &   * coef(i)
                    enddo
                    output(nx,j,k) = ( work_du(nx) * (input(nx,j,k) - input(nx-1,j,k))   &
                                  &  + work_dl(nx) * (input(1 ,j,k) - input(nx  ,j,k)) ) &
                                  &  * coef(nx)
                enddo
            enddo
        else
            work_du(1) = 1._rk / dlon(2)
            do i = 2, nx-1
                work_du(i) = dlon(i+1) * dlon(i+1)
                work_dl(i) = dlon(i  ) * dlon(i  )
                coef(i)    = 1._rk / (dlon(i)*dlon(i+1) * (dlon(i) + dlon(i+1)))
            enddo
            work_dl(nx) = 1._rk / dlon(nx)

            do k = 1, nz
                do j = 1, ny
                    output(1,j,k) = (input(2,j,k) - input(1,j,k)) * work_du(1)
                    do i = 2, nx-1
                        output(i,j,k) = ( work_du(i) * (input(i  ,j,k) - input(i-1,j,k))   &
                                    &   + work_dl(i) * (input(i+1,j,k) - input(i  ,j,k)) ) &
                                    &   * coef(i)
                    enddo
                    output(nx,j,k) = (input(nx,j,k) - input(nx-1,j,k)) * work_dl(nx)
                enddo
            enddo
        endif

        if (present(status)) then
            status = work_status
        endif

    end subroutine zonalDerivative_qp


    pure subroutine meridionalDerivative_qp(lat, input, output, status)
        real(rk), intent(in)  :: lat(:)
        real(rk), intent(in)  :: input(:,:,:)
        real(rk), intent(out) :: output(:,:,:)
        integer , intent(out), optional :: status

        real(rk) :: dlat(size(lat))
        real(rk) :: work_dl(size(lat))
        real(rk) :: work_du(size(lat))
        real(rk) :: coef(size(lat))
        real(rk) :: dl
        real(rk) :: du
        real(rk) :: c
        integer :: i
        integer :: j
        integer :: k
        integer :: nx
        integer :: ny
        integer :: nz
        integer :: work_status
        logical :: dim_consist

        nx = size(input, 1)
        ny = size(input, 2)
        nz = size(input, 3)

        if (ny <= 1) then
            if (present(status)) then
                status = -2
            endif
            return
        endif

        dim_consist = (size(lat   , 1) == ny) .AND. &
                    & (size(output, 1) == nx) .AND. &
                    & (size(output, 2) == ny) .AND. &
                    & (size(output, 3) == nz)

        if (.NOT. dim_consist) then
            if (present(status)) then
                status = -1
            endif
            return
        endif
        work_status = 1

        dlat(1:ny-1) = lat(2:ny) - lat(1:ny-1)

        if (any(dlat(1:ny-1) < 0) .AND. any(dlat(1:ny-1) > 0)) then
            if (present(status)) then
                status = -3
            endif
            return
        endif

        work_du(1) = 1._rk / dlat(1)
        do j = 2, ny-1
            work_du(j) = dlat(j  ) * dlat(j  )
            work_dl(j) = dlat(j-1) * dlat(j-1)
            coef(j)    = 1._rk / (dlat(j-1)*dlat(j) * (dlat(j-1) + dlat(j)))
        enddo
        work_dl(ny) = 1._rk / dlat(ny-1)

        do k = 1, nz
            du = work_du(1)
            do i = 1, nx
                output(i,1,k) = (input(i,2,k) - input(i,1,k)) * du
            enddo
            do j = 2, ny-1
                du = work_du(j)
                dl = work_dl(j)
                c  = coef(j)
                do i = 1, nx
                    output(i,j,k) = (du * (input(i,j  ,k) - input(i,j-1,k))   &
                                 & + dl * (input(i,j+1,k) - input(i,j  ,k)) ) &
                                 & * c
                enddo
            enddo
            dl = work_dl(ny)
            do i = 1, nx
                output(i,ny,k) = (input(i,ny,k) - input(i,ny-1,k)) * dl
            enddo
        enddo

        if (present(status)) then
            status = work_status
        endif

    end subroutine meridionalDerivative_qp


    pure subroutine verticalDerivative_qp(lev, input, psfc, output, undef, status)
        real(rk), intent(in)  :: lev(:)
        real(rk), intent(in)  :: input(:,:,:)
        real(rk), intent(in)  :: psfc(:,:)
        real(rk), intent(out) :: output(:,:,:)
        real(rk), intent(in) , optional :: undef       !! Default: -999.E+30_rk
        integer , intent(out), optional :: status

        real(rk) :: dlev(size(lev))
        real(rk) :: work_undef
        real(rk) :: toa_dlev_inv
        real(rk) :: bottom_dlev_inv
        real(rk) :: sfc_dlev_inv
        real(rk) :: dl
        real(rk) :: du
        real(rk) :: coef
        integer :: i
        integer :: j
        integer :: k
        integer :: nx
        integer :: ny
        integer :: nz
        integer :: top
        integer :: bottom
        integer :: dir
        integer :: work_status
        logical :: isAtmos(size(input,1),size(input,2))
        logical :: dim_consist

        nx = size(input, 1)
        ny = size(input, 2)
        nz = size(input, 3)

        if (nz <= 1) then
            if (present(status)) then
                status = -2
            endif
            return
        endif

        dim_consist = (size(lev   , 1) == nz) .AND. &
                    & (size(psfc  , 1) == nx) .AND. &
                    & (size(psfc  , 2) == ny) .AND. &
                    & (size(output, 1) == nx) .AND. &
                    & (size(output, 2) == ny) .AND. &
                    & (size(output, 3) == nz)

        if (.NOT. dim_consist) then
            if (present(status)) then
                status = -1
            endif
            return
        endif
        work_status = 1

        if (present(undef)) then
            work_undef = undef
        else
            work_undef = -999.E+30_rk
        endif

        dlev(1:nz-1) = lev(2:nz) - lev(1:nz-1)

        ! Vertical derivative should be executed in top->bottom direction
        if (all(dlev(1:nz-1) < 0)) then
            ! If the lev(1) is the bottom and lev(nz) is the top, loop direction is nz->1
            top      = nz
            bottom   =  1
            dir      = -1
            ! toa_dlev_inv    = 1._rk / dlev(nz-1)
            ! bottom_dlev_inv = 1._rk / dlev(1)
        else if (all(dlev(1:nz-1) > 0)) then
            ! If the lev(1) is the top and lev(nz) is the bottom, loop direction is 1->nz
            top      =  1
            bottom   = nz
            dir      =  1
            ! toa_dlev_inv    = 1._rk / dlev(1)
            ! bottom_dlev_inv = 1._rk / dlev(nz-1)
        else
            if (present(status)) then
                status = -3
            endif
            return
        endif

        toa_dlev_inv    = 1._rk / (lev(top+dir) - lev(top))
        bottom_dlev_inv = 1._rk / (lev(bottom) - lev(bottom-dir))

        !! Derivative for TOA
        do j = 1, ny
            do i = 1, nx
                ! if (isAtmos(i,j)) then
                if (lev(top) <= psfc(i,j)) then
                    isAtmos(i,j) = (lev(top+dir) <= psfc(i,j))
                    if (isAtmos(i,j)) then
                        output(i,j,top) = (input(i,j,top+dir) - input(i,j,top)) * toa_dlev_inv
                    else
                        ! work_dlev = psfc(i,j) - lev(top)
                        output(i,j,top) = work_undef
                    endif
                else
                    isAtmos(i,j) = .FALSE.
                    output(i,j,top) = work_undef
                endif
            enddo
        enddo
        !! Derivative between lev(2) and lev(nz-1)
        do k = top+dir, bottom-dir, dir
            dl   = dlev(k-1) * dlev(k-1)
            du   = dlev(k  ) * dlev(k  )
            coef = 1._rk / (dlev(k-1)*dlev(k) * (dlev(k-1) + dlev(k)))
            sfc_dlev_inv = 1._rk / (lev(k) - lev(k-dir))
            do j = 1, ny
                do i = 1, nx
                    if (isAtmos(i,j)) then
                        isAtmos(i,j) = (lev(k+dir) <= psfc(i,j))
                        if (isAtmos(i,j)) then
                            output(i,j,k) = (du * (input(i,j,k  ) - input(i,j,k-1))   &
                                         & + dl * (input(i,j,k+1) - input(i,j,k  )) ) &
                                         & * coef
                        else
                            output(i,j,k) = (input(i,j,k) - input(i,j,k-dir)) * sfc_dlev_inv
                        endif
                    else
                        output(i,j,k) = work_undef
                    endif
                enddo
            enddo
        enddo
        !! Derivative for bottom
        do j = 1, ny
            do i = 1, nx
                if (isAtmos(i,j)) then
                    output(i,j,bottom) = (input(i,j,bottom) - input(i,j,bottom-dir)) * bottom_dlev_inv
                else
                    output(i,j,bottom) = work_undef
                endif
            enddo
        enddo

        if (present(status)) then
            status = work_status
        endif

    end subroutine verticalDerivative_qp

end module derivative_qp


