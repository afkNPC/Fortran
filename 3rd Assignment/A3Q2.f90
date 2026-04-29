program A3Q2
    implicit none
    integer, parameter :: n = 4
    real :: A(n, n), A_inv(n, n), B(n), X(n)
    real :: aug(n, n+n), factor
    integer :: i, j, k

    open(10, file="A3Q1_in.txt")

    do i = 1, n
        read(10, *) A(i, :), B(i)
    end do
    close(10)

    aug = 0.0
    aug(1:n, 1:n) = A
    do i = 1, n
        aug(i, n+i) = 1.0
    end do

    do i = 1, n
        if (abs(aug(i, i)) < 1e-6) then
            print *, "Error: Zero pivot encountered at row ", i
            stop
        end if

        factor = aug(i, i)
        aug(i, :) = aug(i, :) / factor

        do j = 1, n
            if (i /= j) then
                factor = aug(j, i)
                aug(j, :) = aug(j, :) - factor * aug(i, :)
            end if
        end do
    end do

    A_inv = aug(:, n+1:2*n)

    X = matmul(A_inv, B)

    print *, "Inverse Matrix (A^-1):"
    do i = 1, n
        print '(4F10.4)', A_inv(i, :)
    end do

    print *, ""
    print '(A, 4F10.4)', "Solution (X): ", X

end program

subroutine manual_matmul(A, B, C)
        real, intent(in)  :: A(:,:), B(:,:)
        real, intent(out) :: C(size(A,1), size(B,2))
        integer :: i, j, k
        integer :: L, M, N

        L = size(A, 1)
        M = size(A, 2)
        N = size(B, 2)

        C = 0.0

        do i = 1, L
            do j = 1, N
                do k = 1, M
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
end subroutine manual_matmul
