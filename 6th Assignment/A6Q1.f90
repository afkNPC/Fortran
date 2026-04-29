program A6Q1
    implicit none
    integer, parameter :: n = 6
    integer :: i, j
    real(8) :: t(n), x(n), diff(n, n), h, velocity

    open(10, file='A6Q1_in.txt', status='old')
    open(20, file='A6Q1_out.txt', status='replace')

    read(10, *) (t(i), i=1,n)
    read(10, *) (x(i), i=1,n)

    h = t(2) - t(1)

    do i = 1, n
        diff(i, 1) = x(i)
    end do

    do j = 2, n
        do i = 1, n - j + 1
            diff(i, j) = diff(i+1, j-1) - diff(i, j-1)
        end do
    end do

    write(*, '(A10, A10, A12, A12, A12, A12, A12)') "t", "x", "D1", "D2", "D3", "D4", "D5"
    write(20, '(A10, A10, A12, A12, A12, A12, A12)') "t", "x", "D1", "D2", "D3", "D4", "D5"

    do i = 1, n
        write(*, '(F10.2, F10.4, 5F12.4)') t(i), (diff(i, j), j=1, n-i+1)
        write(20, '(F10.2, F10.4, 5F12.4)') t(i), (diff(i, j), j=1, n-i+1)
    end do

    velocity = (1.0d0 / h) * ((diff(1, 2) - (diff(1, 3)/2.0d0) + (diff(1, 4)/3.0d0) - (diff(1, 5)/4.0d0) + (diff(1, 6)/5.0d0)))

    write(*, *) "--------------------------------------------------------"
    write(*, '(A,F10.4)') "Velocity at t=1 using Newton Forward: ", velocity
    write(20, *) "--------------------------------------------------------"
    write(20, *) "Velocity at t=1 using Newton Forward: ", velocity

    close(10)
    close(20)
end program
