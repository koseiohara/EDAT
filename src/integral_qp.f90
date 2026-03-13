


module integral_qp

    use, intrinsic :: iso_fortran_env, only : rk=>real128
    use EDAT_Math, only : sum_hp

    implicit none

    private
    public :: meridionalIntegral_qp, verticalIntegral_qp

    contains


    pure subroutine meridionalIntegral_qp(lat, field, south, north, output, status, valid_south, valid_north)
        real(rk), intent(in)  :: lat(:)
        real(rk), intent(in)  :: field(:,:,:)
        real(rk), intent(in)  :: south
        real(rk), intent(in)  :: north
        real(rk), intent(out) :: output(:,:)
        integer , intent(out) :: status
        real(rk), intent(out), optional :: valid_south
        real(rk), intent(out), optional :: valid_north

        real(rk), allocatable :: work_field(:,:)

        real(rk) :: costbl(size(lat))
        real(rk) :: dlat(size(lat))
        real(rk) :: dl
        real(rk) :: c0
        real(rk) :: c1
        integer :: nx
        integer :: ny
        integer :: nz
        integer :: work_nx
        integer :: work_ny
        integer :: work_nz
        integer :: south_idx
        integer :: north_idx
        integer :: jl
        integer :: ju
        integer :: i
        integer :: j
        integer :: k
        logical :: dim_consist

        nx = size(field, 1)
        ny = size(field, 2)
        nz = size(field, 3)

        dim_consist = .TRUE.

        work_ny = size(lat)
        dim_consist = dim_consist .AND. (work_ny == ny)

        work_nx = size(output, 1)
        work_nz = size(output, 2)
        dim_consist = dim_consist .AND. (work_nx == nx .AND. work_nz == nz)

        if (.NOT. dim_consist) then
            status = -1
            return
        endif
        status = 1

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
            output(1:nx,1:nz) = 0._rk
            return
        endif

        costbl(jl:ju) = cos(lat(jl:ju)) * 0.5_rk

        if (lat(2) < lat(1)) then
            costbl(jl:ju) = - costbl(jl:ju)
        endif

        allocate(work_field(nx,jl:ju-1))

        dlat(jl:ju-1) = lat(jl+1:ju) - lat(jl:ju-1)
        do k = 1, nz
            do j = jl, ju-1
                dl = dlat(j)
                c0 = costbl(j)
                c1 = costbl(j+1)
                do i = 1, nx
                    work_field(i,j) = (field(i,j+1,k)*c1 + field(i,j,k)*c0) * dl
                enddo
            enddo
            do i = 1, nx
                output(i,k) = sum_hp(ju-jl, work_field(i,jl:ju-1))
            enddo
        enddo

        deallocate(work_field)

    end subroutine meridionalIntegral_qp


    subroutine verticalIntegral_qp(lev, field, psfc, output, status)
        real(rk), intent(in), contiguous :: lev(:)
        real(rk), intent(in), contiguous :: field(:,:,:)
        real(rk), intent(in), contiguous :: psfc(:,:)
        real(rk), intent(out) :: output(:,:)
        integer , intent(out) :: status

        logical  :: isAtmos(size(field,1), size(field,2))
        real(rk) :: work_field(size(field,1), size(field,2), size(field,3))
        real(rk) :: work_lev
        real(rk) :: work_lev_upper
        real(rk) :: work_dlev

        integer :: sfc_k(size(field,1), size(field,2))
        integer :: nx
        integer :: ny
        integer :: nz
        integer :: work_nx
        integer :: work_ny
        integer :: i
        integer :: j
        integer :: k
        integer :: kupper
        integer :: zbeg
        integer :: zend
        integer :: zdir
        logical :: dim_consistent

        nx = size(field, 1)
        ny = size(field, 2)
        nz = size(field, 3)

        dim_consistent = .True.
        dim_consistent = dim_consistent .AND. (size(lev) == nz)

        work_nx = size(psfc, 1)
        work_ny = size(psfc, 2)
        dim_consistent = dim_consistent .AND. (work_nx == nx .AND. work_ny == ny)

        work_nx = size(output, 1)
        work_ny = size(output, 2)
        dim_consistent = dim_consistent .AND. (work_nx == nx .AND. work_ny == ny)

        if (.NOT. dim_consistent) then
            status = -1
            return
        endif

        status = 1

        ! zrev check
        ! zbeg : top
        ! zend : bottom
        if (lev(1) > lev(2)) then
            zbeg = nz
            zend = 1
            zdir = -1
        else
            zbeg = 1
            zend = nz
            zdir = 1
        endif

        work_field(1:nx,1:ny,1:nz) = -1.E+30_rk

        k = zbeg
        do j = 1, ny
            do i = 1, nx
                ! Init for the integral
                isAtmos(i,j) = .True.
                sfc_k(i,j)   = zend

                ! Triangle Integral for the TOA
                work_field(i,j,k) = lev(k) * field(i,j,k) * 0.5_rk
            enddo
        enddo

        kupper = zbeg
        ! Top -> Bottom
        do k = zbeg+zdir, zend, zdir
            ! kupper = k-zdir
            work_lev       = lev(k)
            work_lev_upper = lev(kupper)
            work_dlev      = (work_lev - work_lev_upper) * 0.5_rk
            ! Input the area of trapezoids to work_field
            do j = 1, ny
                do i = 1, nx
                    if (isAtmos(i,j)) then
                        if (work_lev < psfc(i,j)) then
                            work_field(i,j,k) = work_dlev * (field(i,j,k) + field(i,j,kupper))
                            cycle
                        endif

                        sfc_k(i,j)   = k
                        isAtmos(i,j) = .False.
                        if (work_lev > psfc(i,j)) then
                            ! Integral from work_lev_upper to Earth-Surface: if work_lev is under the surface
                            work_field(i,j,k) = (psfc(i,j) - work_lev_upper) * field(i,j,kupper)
                            cycle
                        else
                            ! Integral from work_lev_upper to Earth-Surface: if work_lev is at the surface
                            work_field(i,j,k) = work_dlev * (field(i,j,k) + field(i,j,kupper))
                        endif
                    endif
                enddo
            enddo
            kupper = k
        enddo

        ! Integral
        do j = 1, ny
            do i = 1, nx
                if (lev(zbeg) < psfc(i,j)) then
                    k = (sfc_k(i,j) - zbeg) * zdir + 1
                    output(i,j) = sum_hp(k, work_field(i,j,zbeg:sfc_k(i,j):zdir))
                    cycle
                endif
                output(i,j) = 0._rk
            enddo
        enddo

    end subroutine verticalIntegral_qp

end module integral_qp


