program matrix_multiplication
    implicit none
    integer :: i, j, r1,r2,c1,c2
    integer, allocatable :: A(:,:), B(:,:), C(:,:)

    open(10,file='a2q2_in.txt')
    open(11,file='a2q2_out.txt')
    read(10,*) r1, c1
    read(10,*) r2, c2

    allocate(A(r1,c1), B(r2,c2), C(r1,c2))

    do i = 1, r1
        read(10,*) (A(i,j), j=1,c1)
    end do

    do i = 1, r2
        read(10,*) (B(i,j), j=1,c2)
    end do

    close(10)

    call multiply_matrices(A, B, C, r1,c1,r2,c2)

    write(*,*) "Matrix A:"
    do i = 1, r1
        write(*,*) (A(i,j), j=1,c1)
    end do

    print *, "Matrix B:"
    do i = 1, r2
        write(*,*) (B(i,j), j=1,c2)
    end do

    print *, "Resultant Matrix (A + B):"
    do i = 1, r1
        write(*,*) (C(i,j), j=1,c2)
    end do

!------------------------------------------------------------------------

write(11,*) "Matrix A:"
    do i = 1, r1
        write(11,*) (A(i,j), j=1,c1)
    end do

    write(11,*) "Matrix B:"
    do i = 1, r2
        write(11,*) (B(i,j), j=1,c2)
    end do

    write(11,*) "Resultant Matrix (A + B):"
    do i = 1, r1
        write(11,*) (C(i,j), j=1,c2)
    end do

end program

 subroutine multiply_matrices(X, Y, Z, row1,col1,row2,col2)
        integer::row1,col1,row2,col2
        integer:: X(row1,col1), Y(row2,col2), Z(row1,col2),i, j,k

        Z=0
        do i = 1, row1
            do j = 1, col2
                do k=1,col1
                Z(i,j) = Z(i,j) + X (i,k) * Y(k,j)
            end do
        end do
        end do
end subroutine

