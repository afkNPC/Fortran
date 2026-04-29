program A6Q2
    implicit none
    integer, parameter :: n = 6
    integer :: i, j
    real(8) :: t(n), x(n), diff(n, n), h, velocity

    open(10, file='A6Q1_in.txt', status='old')
    open(20, file='A6Q2_out.txt', status='replace')

    read(10, *) (t(i), i=1,n)
    read(10, *) (x(i), i=1,n)

    h = t(2) - t(1)

    do i = 1, n
        diff(i, 1) = x(i)
    end do

    do j = 2, n
        do i = j, n
            diff(i, j) = diff(i, j-1) - diff(i-1, j-1)
        end do
    end do

    write(*, '(A10, A10, A12, A12, A12, A12, A12)') "t", "x", "N1", "N2", "N3", "N4", "N5"
    write(20, '(A10, A10, A12, A12, A12, A12, A12)') "t", "x", "N1", "N2", "N3", "N4", "N5"

    do i = 1, n
        write(*, '(F10.2, F10.4, 5F12.4)') t(i), (diff(i, j), j=1, i)
        write(20, '(F10.2, F10.4, 5F12.4)') t(i), (diff(i, j), j=1, i)
    end do

    velocity = (1.0d0 / h) * (diff(n, 2) + (diff(n, 3)/2.0d0) + &
               (diff(n, 4)/3.0d0) + (diff(n, 5)/4.0d0) + (diff(n, 6)/5.0d0))

    write(*, *) "--------------------------------------------------------"
    write(*, *) "Velocity at t=11 using Newton Backward: ", velocity
    write(20, *) "--------------------------------------------------------"
    write(20, *) "Velocity at t=11 using Newton Backward: ", velocity

    close(10)
    close(20)
end program A6Q2
