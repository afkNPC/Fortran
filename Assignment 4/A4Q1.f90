program Bisection
    implicit none

    integer::iter,max_iter
    real::f,a,b,c,tol,rel_error,r

    open (unit=11, file="A4Q1_out.txt")

    a=1.0
    b=2.0
    tol=1.0e-4
    max_iter=1000

    write(11,*)"         iter      a               b                c"
    write(11,*)"        ----------------------------------------------------"

    do iter=1,max_iter

        c=a+(b-a)/2
        rel_error=abs((c-r)/c)

        write(11,*)iter,a,b,c,rel_error

        if (rel_error<tol) then
            write(11,*)"root found at x=",c
            exit
        end if

        if (f(a)*f(c)<0.0) then
            b=c
            r=c
        else
            a=c
            r=c
        end if

    end do


    end program

   real function f(x)
    implicit none
    real::x
    f=x**3+4*x**2-10

    end function
