program Assignment_Integration
    implicit none
    integer:: n
    integer :: i, j, k
    real(8) :: ax, bx, ay, by, az, bz, hx, hy, hz
    real(8) :: sum_trap, sum_s13, sum_s38, x, y, z, f_val

    ! --- PART (A): DOUBLE INTEGRAL ---
    ! Integral of (2y^2)/(x+y) from x=0.6 to 1.0 and y=1.0 to 1.6
    ax = 0.6d0; bx = 1.0d0
    ay = 1.0d0; by = 1.6d0
    hx = (bx - ax) / n
    hy = (by - ay) / n

    sum_trap = 0.0d0; sum_s13 = 0.0d0; sum_s38 = 0.0d0

    do i = 0, n
        x = ax + i * hx
        do j = 0, n
            y = ay + j * hy
            f_val = (2.0d0 * y**2) / (x + y)

            sum_trap = sum_trap + (w_trap(i)*w_trap(j)) * f_val
            sum_s13  = sum_s13  + (w_s13(i) *w_s13(j))  * f_val
            sum_s38  = sum_s38  + (w_s38(i) *w_s38(j))  * f_val
        end do
    end do

    print *, "--- PART (A) RESULTS ---"
    print *, "Trapezoidal: ", sum_trap * (hx * hy / 4.0d0)
    print *, "Simpson 1/3: ", sum_s13  * (hx * hy / 9.0d0)
    print *, "Simpson 3/8: ", sum_s38  * (hx * hy * 9.0d0 / 64.0d0)
    print *

    ! --- PART (B): TRIPLE INTEGRAL ---
    ! Integral of xyz from x=0 to 2, y=0 to 3, z=0 to 1
    ax = 0.0d0; bx = 2.0d0
    ay = 0.0d0; by = 3.0d0
    az = 0.0d0; bz = 1.0d0
    hx = (bx - ax) / n
    hy = (by - ay) / n
    hz = (bz - az) / n

    sum_trap = 0.0d0; sum_s13 = 0.0d0; sum_s38 = 0.0d0

    do i = 0, n
        x = ax + i * hx
        do j = 0, n
            y = ay + j * hy
            do k = 0, n
                z = az + k * hz
                f_val = x * y * z

                sum_trap = sum_trap + (w_trap(i)*w_trap(j)*w_trap(k)) * f_val
                sum_s13  = sum_s13  + (w_s13(i) *w_s13(j) *w_s13(k))  * f_val
                sum_s38  = sum_s38  + (w_s38(i) *w_s38(j) *w_s38(k))  * f_val
            end do
        end do
    end do

    print *, "--- PART (B) RESULTS ---"
    print *, "Trapezoidal: ", sum_trap * (hx * hy * hz / 8.0d0)
    print *, "Simpson 1/3: ", sum_s13  * (hx * hy * hz / 27.0d0)
    print *, "Simpson 3/8: ", sum_s38  * (hx * hy * hz * 27.0d0 / 512.0d0)
    print *, "Analytical Check: 4.5"

contains

    ! Weight for Trapezoidal Rule
    function w_trap(idx) result(w)
        integer, intent(in) :: idx
        real(8) :: w
        if (idx == 0 .or. idx == n) then ; w = 1.0d0 ; else ; w = 2.0d0 ; end if
    end function

    ! Weight for Simpson's 1/3 Rule
    function w_s13(idx) result(w)
        integer, intent(in) :: idx
        real(8) :: w
        if (idx == 0 .or. idx == n) then ; w = 1.0d0
        else if (mod(idx, 2) /= 0)   then ; w = 4.0d0
        else
            w = 2.0d0
        end if
    end function

    ! Weight for Simpson's 3/8 Rule
    function w_s38(idx) result(w)
        integer, intent(in) :: idx
        real(8) :: w
        if (idx == 0 .or. idx == n) then ; w = 1.0d0
        else if (mod(idx, 3) == 0)   then ; w = 2.0d0
        else
            w = 3.0d0
        end if
    end function

end program Assignment_Integration
