

program benchmark
    use, intrinsic :: iso_fortran_env, only : ik=>int64, ounit=>output_unit, rk=>real64, compiler_options, &
                                           & compiler_version
    use :: pairwise_sum, only : sum_hp

    implicit none

#ifdef DEBUG
#error benchmark.F90 must be compiled without DEBUG
#endif

    integer(ik), parameter :: target_elements = 4194304_ik
    integer(ik), parameter :: timed_elements_per_sample = 16777216_ik
    integer, parameter :: sample_count = 5

    integer(ik), parameter :: power_n_values(6) = [4_ik, 8_ik, 64_ik, 512_ik, 4096_ik, 32768_ik]
    integer(ik), parameter :: stride_values(6) = [1_ik, 3_ik, 8_ik, 31_ik, 64_ik, 256_ik]
    integer(ik), parameter :: boundary_n_values(6) = [7_ik, 9_ik, 63_ik, 65_ik, 4095_ik, 4097_ik]
    integer(ik), parameter :: boundary_stride_values(3) = [1_ik, 31_ik, 64_ik]

    integer :: i
    integer :: j

    write(ounit,'(A)') 'pairwise_sum public-API SIMD benchmark'
#ifdef BENCHMARK_OMP_SIMD
    write(ounit,'(A)') 'OpenMP SIMD: enabled'
#else
    write(ounit,'(A)') 'OpenMP SIMD: disabled'
#endif
    write(ounit,'(A,A)') 'Compiler: ', compiler_version()
    write(ounit,'(A,A)') 'Options:  ', compiler_options()
    write(ounit,'(A,I0)') 'Target elements/case: ', target_elements
    write(ounit,'(A,I0)') 'Timed elements/sample: ', timed_elements_per_sample
    write(ounit,'(A,I0)') 'Samples/case: ', sample_count
    write(ounit,'(A)') ''
    write(ounit,'(A)') 'suite               n  stride  howmany    elements repeats   median_ms    ns/element' // &
                      & '  spread_pct                checksum'

    do j = 1, size(stride_values)
        do i = 1, size(power_n_values)
            call run_case('power2_grid ', power_n_values(i), stride_values(j))
        enddo
    enddo

    do j = 1, size(boundary_stride_values)
        do i = 1, size(boundary_n_values)
            call run_case('boundary_grid', boundary_n_values(i), boundary_stride_values(j))
        enddo
    enddo

    contains


    subroutine run_case(suite, n, stride)
        character(*), intent(in) :: suite
        integer(ik)  , intent(in) :: n
        integer(ik)  , intent(in) :: stride

        real(rk), allocatable :: arr(:,:,:)
        real(rk), allocatable, volatile :: output(:,:)
        real(rk) :: sample_seconds(sample_count)
        real(rk) :: checksum
        real(rk) :: elapsed_seconds
        real(rk) :: median
        real(rk) :: nanoseconds_per_element
        real(rk) :: spread_percent
        integer(ik) :: clock_finish
        integer(ik) :: clock_rate
        integer(ik) :: clock_start
        integer(ik) :: element_count
        integer(ik) :: howmany
        integer(ik) :: repetitions
        integer(ik) :: repeat
        integer :: sample

        howmany       = max(1_ik, target_elements / (n*stride))
        element_count = n * stride * howmany
        repetitions   = max(1_ik, timed_elements_per_sample / element_count)

        allocate(arr(stride,n,howmany))
        allocate(output(stride,howmany))

        call init_array(arr(:,:,:))

        arr(1,1,1) = -arr(1,1,1)
        output(1:stride,1:howmany) = sum_hp(arr, dim=2)
        checksum = output(1,1)

        do sample = 1, sample_count
            call system_clock(count_rate=clock_rate)
            call system_clock(count=clock_start)
            do repeat = 1_ik, repetitions
                arr(1,1,1) = -arr(1,1,1)
                output(1:stride,1:howmany) = sum_hp(arr, dim=2)
                checksum = checksum + output(1,1)
            enddo
            call system_clock(count=clock_finish)

            elapsed_seconds = real(clock_finish-clock_start, rk) / real(clock_rate, rk)
            sample_seconds(sample) = elapsed_seconds / real(repetitions, rk)
        enddo

        median = median_value(sample_seconds(:))
        nanoseconds_per_element = 1.0e9_rk*median / real(element_count, rk)
        spread_percent = 100.0_rk*(maxval(sample_seconds)-minval(sample_seconds)) / median
        checksum = checksum + sum_hp(reshape(output, shape=[size(output)]))

        write(ounit,'(A13,3(1X,I8),1X,I11,1X,I7,2(1X,F12.6),1X,F11.3,1X,ES24.16)') suite, n, stride, &
                                                                                     & howmany, element_count, &
                                                                                     & repetitions, &
                                                                                     & 1000.0_rk*median, &
                                                                                     & nanoseconds_per_element, &
                                                                                     & spread_percent, checksum

        deallocate(arr)
        deallocate(output)

    end subroutine run_case


    subroutine init_array(arr)
        real(rk), intent(out), contiguous :: arr(:,:,:)

        integer(ik) :: i
        integer(ik) :: j
        integer(ik) :: k

        do k = 1_ik, size(arr, 3, kind=ik)
            do j = 1_ik, size(arr, 2, kind=ik)
                do i = 1_ik, size(arr, 1, kind=ik)
                    arr(i,j,k) = real(modulo(17_ik*i + 29_ik*j + 43_ik*k, 4096_ik) - 2048_ik, rk) / 2048.0_rk
                enddo
            enddo
        enddo

    end subroutine init_array


    pure function median_value(values) result(output)
        real(rk), intent(in) :: values(:)

        real(rk) :: work(size(values))
        real(rk) :: output
        real(rk) :: value
        integer :: i
        integer :: j

        work(1:size(values)) = values(1:size(values))
        do i = 2, size(work)
            value = work(i)
            j = i - 1
            do while (j >= 1)
                if (work(j) <= value) then
                    exit
                endif
                work(j+1) = work(j)
                j = j - 1
            enddo
            work(j+1) = value
        enddo

        output = work((size(work)+1) / 2)

    end function median_value

end program benchmark


