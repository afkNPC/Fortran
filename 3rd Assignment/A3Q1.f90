program A1Q1
    implicit none

    integer:: n,i,j,k,p
    real::m
    real, allocatable:: A(:,:)
    real, allocatable:: x(:)

    open(10,file="A3Q1_in.txt")
    open(11,file="A3Q1_out.txt")

    n=4

    allocate(A(n,n+1))
    allocate(x(n))

    do i=1,n
        read(10,*)(a(i,j),j=1,n+1)
    end do

    do i=1,n-1

        p=i

        do k=i,n
            if (A(k,i)/=0.0) then
                p=k
            exit
            end if
        end do

        if (A(p,i)==0.0) then

            write(*,*)"Unique solution does not exist"
            stop
        end if

        if (p/= i) then
            A([i,p],:) = A([p,i],:)
        end if

        do j=i+1, n
            m= A(j,i)/A(i,i)
            A(j,:) = A (j,:) - m*A(i,:)
        end do
    end do

    if (A(n,n) ==0.0) then
        write(*,*) "Unique solution does not exist"
        stop
    end if

    x(n) = A(n,n+1)/A(n,n)

    do i=n-1,1,-1
        x(i) = (A(i,n+1) - sum(A(i,i+1:n)*x(i+1:n)))/A(i,i)
    end do

    write(*,*) "Solution Vector"
    do i=1,n
        write(*,*) "x(",i,")=",x(i)
        write(11,*) "x(",i,")=",x(i)
    end do

end program
