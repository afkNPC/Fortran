program A5Q3
    implicit none

    real, dimension(0:8) :: x, f
    real, dimension(4, 4) :: R
    real :: h
    integer :: i, j, k, step_size

    R = 0.0

    open(10, file='A5Q3_in.txt', status='old')
    read(10, *) x
    read(10, *) f
    close(10)

    !Romberg
    h = x(8) - x(0)
    R(1,1) = (h / 2.0) * (f(0) + f(8))

    do i = 2, 4
        h = h / 2.0
        step_size = 9 / (2**(i-1))

        R(i,1) = 0.5 * R(i-1,1)
        do k = step_size, 9 - step_size, 2 * step_size
            R(i,1) = R(i,1) + h * f(k)
        end do
    end do

    do j = 2, 4
        do i = j, 4
            R(i,j) = R(i,j-1) + (R(i,j-1) - R(i-1,j-1)) / (4.0d0**(j-1) - 1.0d0)
        end do
    end do

    print *, "Romberg Table:"
    print *, "--------------------------------------------"
    do i = 1, 4
        write(*,*), (R(i, j), j = 1, i)
    end do
    print *, "--------------------------------------------"
    write(*,*) "Estimated Integral: ", R(4, 4)

end program
