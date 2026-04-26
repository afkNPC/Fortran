program A4Q4
    implicit none

    real(8) :: a, b, c, fa, fb, fc, tol
    integer :: n

    a = 1.0d0
    b = 2.0d0
    tol = 1.0d-5

    print '(A4, A12, A12, A12, A15, A15, A12)', &
          "n", "a", "b", "c", "f(a)", "f(b)", "f(c)"
    print '(A85)', "-------------------------------------------------------------------------------------"

    n = 1
    do
        fa = f(a)
        fb = f(b)

        c = (a * fb - b * fa) / (fb - fa)
        fc = f(c)

        print '(I4, F12.6, F12.6, F12.6, F15.6, F15.6, F12.6)', &
              n, a, b, c, fa, fb, fc

        if (abs(fc) < tol) then
            print '(A85)', "-------------------------------------------------------------------------------------"
            print '(A25, F10.6)', "Root found at x = ", c
            exit
        end if

        if (fa * fc < 0.0d0) then
            b = c
        else
            a = c
        end if

        n = n + 1
        if (n > 100) exit
    end do

contains

    function f(x)
        real(8) :: f, x
        f = exp(x) + (2.0d0**(-x)) + 2.0d0*cos(x) - 6.0d0
    end function f

end program
