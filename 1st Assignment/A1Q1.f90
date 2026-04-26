program A1Q1
    implicit none

    real::m,AC,TC
    read(*,*)m

    call sum1(m,AC,TC)
    !write(*,*)"Total cost=",TC
    !write(*,*)"Average cost=",AC
end program

subroutine sum1(p,q,r)
    implicit none

    real::p,q,r

    if (p<=100) then
        r=0.5*p
    else if (p<=300) then

        r=(0.5*100)+0.3*(p-100)

    else

        r=(0.5*100)+(0.3*200)+0.2*(p-300)

    end if

    q=r/p

    write(*,*)"Total cost=",r
    write(*,*)"Average cost=",q

end subroutine
