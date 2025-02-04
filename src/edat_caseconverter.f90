module EDAT_CaseConverter

    implicit none

    private
    public :: to_upper, to_lower

    contains


    subroutine to_upper(input, output)
        character(*), intent(in)  :: input
        character(*), intent(out) :: output

        integer, parameter :: code_min = iachar('A')
        integer, parameter :: code_max = iachar('Z')
        integer, parameter :: offset = iachar('a') - iachar('A')

        call convert_core(input   , &  !! IN
                        & output  , &  !! OUT
                        & offset  , &  !! IN
                        & code_min, &  !! IN
                        & code_max  )  !! IN
       
    end subroutine to_upper


    subroutine to_lower(input, output)
        character(*), intent(in)  :: input
        character(*), intent(out) :: output

        integer, parameter :: code_min = iachar('a')
        integer, parameter :: code_max = iachar('z')
        integer, parameter :: offset = iachar('A') - iachar('a')

        call convert_core(input   , &  !! IN
                        & output  , &  !! OUT
                        & offset  , &  !! IN
                        & code_min, &  !! IN
                        & code_max  )  !! IN
       
    end subroutine to_lower


    subroutine convert_core(input, output, offset, code_min, code_max)
        character(*), intent(in)  :: input
        character(*), intent(out) :: output
        integer     , intent(in)  :: offset
        integer     , intent(in)  :: code_min
        integer     , intent(in)  :: code_max

        integer :: input_len
        integer :: i

        input_len = len(trim(input))
        if (input_len > len(output)) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Input string is longer than the output'
            ERROR STOP
        endif

        output = ''
        do i = 1, input_len
            if (iachar(input(i:i)) >= code_min .AND. iachar(input(i:i)) <= code_max) then
                output(i:i) = achar(iachar(input(i:i)) + offset)
            else
                output(i:i) = input(i:i)
            endif
        enddo

    end subroutine convert_core


end module EDAT_CaseConverter

