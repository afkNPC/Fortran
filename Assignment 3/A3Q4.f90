program A3Q4
    implicit none
    integer, parameter :: n = 4
    real :: A(n, n), L(n, n), U(n, n), B(n), Y(n), X(n)
    integer :: i, j, k

    open(10, file="A3Q1_in.txt", status='old')

    do i = 1, n
        read(10, *) A(i, :), B(i)
    end do
    close(10)

    print*, "Given Matrix: "
    do i = 1, n
            do j = 1, n
                write(*, "(f12.6)", advance='no') A(i, j)
            end do
            print*
    end do


    L = 0.0
    U = 0.0

    do i = 1, n
        L(i, i) = 1.0
    end do

    do i = 1, n
        do j = i, n
            U(i, j) = A(i, j) - sum(L(i, 1:i-1) * U(1:i-1, j))
        end do
        do j = i + 1, n
            L(j, i) = (A(j, i) - sum(L(j, 1:i-1) * U(1:i-1, i))) / U(i, i)
        end do
    end do

    print*, "L: "
    do i = 1, n
            do j = 1, n
                write(*, "(f12.6)", advance='no') L(i, j)
            end do
            print*
    end do
    print*, " "


    print*, "U: "
    do i = 1, n
            do j = 1, n
                write(*, "(f12.6)", advance='no') U(i, j)
            end do
            print*
    end do
    print*, " "

    do i = 1, n
        Y(i) = B(i) - sum(L(i, 1:i-1) * Y(1:i-1))
    end do

    do i = n, 1, -1
        X(i) = (Y(i) - sum(U(i, i+1:n) * X(i+1:n))) / U(i, i)
    end do

    print *, "Result (X):", X
end program
