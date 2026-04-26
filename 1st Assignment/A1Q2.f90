program Celsius_Fahrenheit
    implicit none

    integer::i
    real::C,F

    write(*,*)'   Celsius         Fahrenheit'
    write(*,*)'   -------------------------'

    do C=-20,50,10
       call sum1(C,F)
       write(*,*)C,F
    end do

end program

subroutine sum1(p,q)
    implicit none

    real::p,q
    q=((9*p)/5)+32

end subroutine
