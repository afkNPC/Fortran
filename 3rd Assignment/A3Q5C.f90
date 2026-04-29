program sor
    implicit none
    real,allocatable::a(:,:),b(:),x(:)
    real::tol,w,s
    integer::it,n,i,j,k
    open(10,file='a5c_in.txt')
    open(11,file='a5c_out.txt')

    read(10,*)n
    allocate(a(n,n),b(n),x(n))
     tol=1.0e-5
     it=100
     x=1.0
     w=1.25
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
        if(abs(a(i,i))>=s)then
            call sors(a,b,n,x,tol,it,w)
        else
            write(11,*)'matrix is not diagonally dominant'
        end if

     end do

end program
subroutine SORS(a,b,m,x,t,it1,w)
    implicit none
    real::a(m,m),b(m),x0(m),x(m),s,er,diff,t,s1,s2,w
    integer::k,i,j,m,it1,l

 do k=1,it1
    x0=x
    do i=1,m
        s1=0.0
        do j=1+i,m
            s1=s1+a(i,j)*x0(j)
        end do
        s2=0.0
        do j=1,i-1
            s2=s2+a(i,j)*x(j)
        end do
        x(i)=(((b(i)-s1-s2)*w)/a(i,i))+((1-w)*x0(i))
    end do
    er=0.0
    do l=1,m
        diff=abs(x(l)-x0(l))
        if(diff>er)then
            er=diff
        end if
    end do
    !write(11,*)(x(i),i=1,m),k
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


