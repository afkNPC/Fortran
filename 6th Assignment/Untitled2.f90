program A6Q2
    implicit none
    integer :: n, i
    real :: t(100), x(100)
    real :: d1(100), d2(100), d3(100), d4(100), d5(100)
    real :: h, velocity

    open(10, file='A6Q2_in.txt')
    open(11, file='A6Q2_out.txt')

    read(10,*) n
    read(10,*) (t(i), i=1,n)
    read(10,*) (x(i), i=1,n)

    h = t(2) - t(1)

    ! Differences
    do i=1,n-1
        d1(i) = x(i+1) - x(i)
    end do

    do i=1,n-2
        d2(i) = d1(i+1) - d1(i)
    end do

    do i=1,n-3
        d3(i) = d2(i+1) - d2(i)
    end do

    do i=1,n-4
        d4(i) = d3(i+1) - d3(i)
    end do

    do i=1,n-5
        d5(i) = d4(i+1) - d4(i)
    end do

    ! Newton Backward formula (last point)
    velocity = (1.0/h)*( d1(n-1) + d2(n-2)/2.0 + d3(n-3)/3.0 + d4(n-4)/4.0 + d5(n-5)/5.0 )

    write(11,*) "Velocity at t = 11 is:"
    write(11,*) velocity

    close(10)
    close(11)
end program
