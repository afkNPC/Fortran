program matrix_addition
    implicit none
    integer :: i, j, row, col
    integer, allocatable :: A(:,:), B(:,:), C(:,:)

    open(10,file='a2q1_in.txt')
    open(11,file='a2q1_out.txt')
    read(10,*) row, col

    allocate(A(row,col), B(row,col), C(row,col))

    do i = 1, row
        read(10,*) (A(i,j), j=1,col)
    end do

    do i = 1, row
        read(10,*) (B(i,j), j=1,col)
    end do

    close(10)

    call add_matrices(A, B, C, row, col)

    write(*,*) "Matrix A:"
    do i = 1, row
        write(*,*) (A(i,j), j=1,col)
    end do

    print *, "Matrix B:"
    do i = 1, row
        write(*,*) (B(i,j), j=1,col)
    end do

    print *, "Resultant Matrix (A + B):"
    do i = 1, row
        write(*,*) (C(i,j), j=1,col)
    end do

!------------------------------------------------------------------------

write(11,*) "Matrix A:"
    do i = 1, row
        write(11,*) (A(i,j), j=1,col)
    end do

    write(11,*) "Matrix B:"
    do i = 1, row
        write(11,*) (B(i,j), j=1,col)
    end do

    write(11,*) "Resultant Matrix (A + B):"
    do i = 1, row
        write(11,*) (C(i,j), j=1,col)
    end do

end program matrix_addition

 subroutine add_matrices(X, Y, Z, r, c)
        integer:: r, c
        integer:: X(r,c), Y(r,c), Z(r,c),i, j

        do i = 1, r
            do j = 1, c
                Z(i,j) = X(i,j) + Y(i,j)
            end do
        end do
end subroutine add_matrices
