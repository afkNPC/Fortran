program Symmetric
    implicit none
    integer :: n, i, j
    real :: A(100,100)

    open(10,file='a2q3_out.txt')


    read *, n

    call symmetric_matrix(A, n)

    write(10,*) 'Random Symmetric Matrix:'
    do i = 1, n
        write(10,'(100f8.3)') (A(i,j), j=1,n)
    end do

end program

subroutine symmetric_matrix(M, n)
        implicit none
        integer, intent(in) :: n
        real, intent(out) :: M(100,100)
        integer :: i, j


        do i = 1, n
            do j = i, n
                M(i,j) = rand()
                M(j,i) = M(i,j)
            end do
        end do
end subroutine
