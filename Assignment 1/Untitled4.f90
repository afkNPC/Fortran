program gcd_many
    implicit none
    integer :: n, i, result
    integer, dimension(100) :: arr

    ! Read how many numbers
    print *, 'Enter how many numbers:'
    read *, n

    ! Read the numbers
    print *, 'Enter the numbers:'
    read *, (arr(i), i=1,n)

    ! Initialize result with first number
    result = arr(1)

    ! Call subroutine to compute GCD of all
    call gcd_array(arr, n, result)

    print *, 'GCD of given numbers = ', result
end program gcd_many


subroutine gcd_array(a, n, g)
    implicit none
    integer, intent(in) :: n
    integer, dimension(n), intent(in) :: a
    integer, intent(inout) :: g
    integer :: i

    do i = 2, n
        g = gcd_two(g, a(i))
    end do
end subroutine gcd_array


integer function gcd_two(x, y)
    implicit none
    integer, intent(in) :: x, y
    integer :: a, b, temp

    a = x
    b = y


    do while (b /= 0)
        temp = mod(a, b)
        a = b
        b = temp
    end do

    gcd_two = a
end function gcd_two
