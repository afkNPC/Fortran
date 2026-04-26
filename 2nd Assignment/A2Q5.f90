program A2Q5
    implicit none

    integer::i,j,n
    integer,allocatable::A(:,:),B(:,:),A_T(:,:),B_T(:,:),AB(:,:),AB_T(:,:),BT_AT(:,:)

    read(*,*)n
    allocate(A(n,n),B(n,n),A_T(n,n),B_T(n,n),AB(n,n),AB_T(n,n),BT_AT(n,n))

    open(10,file='A2Q5_in.txt')

    do i=1,n
        read(10,*)(A(i,j),j=1,n)

    end do

    do i=1,n

        read(10,*)(B(i,j),j=1,n)
    end do

write(*,*)"A"
    do i=1,n
        write(*,'(*(I3))')A(i,:)
    end do
write(*,*)"B"
    do i=1,n
        write(*,'(*(I3))')B(i,:)
    end do

    call Matrix_Product(AB,A,B,n,n,n,n)
    call Matrix_Transpose(AB,AB_T)
    call Matrix_Transpose(B,B_T)
    call Matrix_Transpose(A,A_T)
    call Matrix_Product(BT_AT,B_T,A_T,n,n,n,n)

    write(*,*)"AB_T"
    do i=1,n
        write(*,'(*(I3))')AB_T(i,:)
    end do
    write(*,*)"BT_AT"
    do i=1,n
        write(*,'(*(I3))')BT_AT(i,:)
    end do

    if (all(AB_T==BT_AT)) then
        write(*,*)"success"
    else
        write(*,*)"wrong"
    end if

contains

subroutine Matrix_Product (R,P,Q,r1,c1,r2,c2)
    implicit none

    integer::k,r1,c1,r2,c2
    integer,allocatable::R(:,:),P(:,:),Q(:,:)

    R=0
    do i = 1, r1
        do j = 1, c2
            do k = 1, c1
                R(i,j) = R(i,j) + (P(i,k) * Q(k,j))
            end do
        end do
    end do



end subroutine

subroutine Matrix_Transpose (P,Q)
    implicit none

    integer,allocatable::P(:,:),Q(:,:)

    do i=1,n
        do j=1,n
            Q(j,i)=P(i,j)
        end do
    end do

end subroutine

end program
