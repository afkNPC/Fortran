program GCD_integer
    implicit none

    integer :: n,i,num,g
    integer, allocatable:: arr(:)

    write(*,*)"enter the number of integers"
    read *, n
    allocate(arr(n))

    write(*,*)"enter the numbers"
    do i=1,n
        read(*,*)num
        arr(i)=num
    end do

    g=arr(1)
    do i=2,n
        call gcd(g,arr(i),g)
    end do

    write(*,*)g

end program

subroutine gcd(a,b,c)
    implicit none

    integer::a,b,c,temp

    do while (b /= 0)
        temp = mod(a, b)
        a = b
        b = temp
    end do

    c=a

end subroutine
