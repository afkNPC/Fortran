program jacobi
    implicit none
    real,allocatable::a(:,:),b(:),x(:)
    real::tol,s
    integer::it,n,i,j
    open(10,file='a5_in.txt')
    open(11,file='a5a_out.txt')

    read(10,*)n
    allocate(a(n,n),b(n),x(n))
     tol=1.0e-3
     it=15
     x=0.0
     do i=1,n
        read(10,*)(a(i,j),j=1,n)
     end do
     read(10,*)(b(i),i=1,n)
     do i=1,n
        s=0.0
        do j=1,n
            if(j/=i)then
                s=s+abs(a(i,j))
            end if
        end do
        if(a(i,i)>=s)then
            call jac(a,b,n,x,tol,it)
        else
            write(11,*)'matrix is not diagonally dominant'

        end if
     end do


end program
subroutine jac(a,b,m,x,t,it1)
    implicit none
    real::a(m,m),b(m),x0(m),x(m),s,er,diff,t
    integer::k,i,j,m,it1,l

 do k=1,it1
    x0=x
    do i=1,m
        s=0.0
        do j=1,m
            if(j/=i)then
                s=s+a(i,j)*x0(j)
            end if
        end do
        x(i)=(b(i)-s)/a(i,i)
    end do
    er=0.0
    do l=1,m
        diff=abs(x(l)-x0(l))
        if(diff>er)then
            er=diff
        end if
    end do

    if(er>t)then
         write(11,*)k,(x(i),i=1,m),er
else
    write(11,*)k,(x(i),i=1,m),er
      write(11,*)''
            write(11,*)'solutions'
            write(11,*)(x(i),i=1,m)
            100 format (4(f8.3))

            stop

    end if

 end do

end subroutine
