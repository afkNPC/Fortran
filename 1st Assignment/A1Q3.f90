program A1Q3
    implicit none

    integer::n
    read(*,*)n

    call table(n)

end program

subroutine table(num)
    implicit none

    integer::num,i

    write(*,*)"        Number      Square Root            Square       Cube"
    write(*,*)"        -------      ----------            -------      -----"

    do i=1,num

        write(*,*)i,"      ",sqrt(real(i)),i**2,i**3

    end do

end subroutine
