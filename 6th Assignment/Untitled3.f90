PROGRAM ForwardVelocityWithTable
    IMPLICIT NONE

    REAL :: t(6), x(6)
    REAL :: h, velocity
    REAL :: delta(6,6)
    INTEGER :: i, j, n

    ! Data points
    t = (/1.0, 3.0, 5.0, 7.0, 9.0, 11.0/)
    x = (/0.1405, 0.7676, 3.5135, 9.9351, 21.5892, 40.0324/)
    n = 6
    h = 2.0

    PRINT *, "=============================================="
    PRINT *, "NEWTON'S FORWARD INTERPOLATION METHOD"
    PRINT *, "=============================================="
    PRINT *, "Finding velocity at t = 1"
    PRINT *, "=============================================="

    ! Step 1: Build forward difference table (like PDF Example E4.1)
    DO i = 1, n
        delta(i,1) = x(i)
    END DO

    DO j = 2, n
        DO i = 1, n-j+1
            delta(i,j) = delta(i+1,j-1) - delta(i,j-1)
        END DO
    END DO

    ! Step 2: Print forward difference table (exactly as shown in PDF)
    PRINT *, ""
    PRINT *, "Forward Difference Table:"
    PRINT *, "--------------------------------------------------------------"
    PRINT *, " t x Δy Δ²y Δ³y Δ⁴y Δ⁵y"
    PRINT *, "--------------------------------------------------------------"
    DO i = 1, n
        WRITE(*, '(F6.1, F10.4, 5F9.4)') t(i), delta(i,1), &
            (delta(i,j), j=2, MIN(6, n-i+2))
    END DO
    PRINT *, "--------------------------------------------------------------"

    ! Step 3: Extract forward differences from first row (at t=1)
    PRINT *, ""
    PRINT *, "Forward differences at t = t0 = 1.0:"
    PRINT *, "-----------------------------------"
    WRITE(*, '(A, F10.4)') " Δy0 = ", delta(1,2)
    WRITE(*, '(A, F10.4)') " Δ²y0 = ", delta(1,3)
    WRITE(*, '(A, F10.4)') " Δ³y0 = ", delta(1,4)
    WRITE(*, '(A, F10.4)') " Δ⁴y0 = ", delta(1,5)
    WRITE(*, '(A, F10.4)') " Δ⁵y0 = ", delta(1,6)

    ! Step 4: Apply Newton's Forward Formula (Eq. 4.4 from PDF)
    ! f'(x0) = (1/h)[Δy0 - (1/2)Δ²y0 + (1/3)Δ³y0 - (1/4)Δ⁴y0 + (1/5)Δ⁵y0]

    velocity = (1.0/h) * (delta(1,2) - 0.5*delta(1,3) + (1.0/3.0)*delta(1,4) &
               - 0.25*delta(1,5) + 0.2*delta(1,6))

    PRINT *, ""
    PRINT *, "Newton's Forward Formula (Eq. 4.4):"
    PRINT *, "-----------------------------------"
    PRINT *, "f'(t0) = (1/h)[Δy0 - ½Δ²y0 + ⅓Δ³y0 - ¼Δ⁴y0 + ⅕Δ⁵y0]"
    PRINT *, ""
    WRITE(*, '(A, F6.3)') " h = ", h
    WRITE(*, '(A, F10.6)') " Sum = ", (delta(1,2) - 0.5*delta(1,3) + (1.0/3.0)*delta(1,4) - 0.25*delta(1,5) + 0.2*delta(1,6))
    WRITE(*, '(A, F10.6)') " Velocity = (1/", h, ") × Sum = ", velocity

    PRINT *, ""
    PRINT *, "=============================================="
    PRINT *, "ANSWER FOR Q1:"
    PRINT *, "=============================================="
    WRITE(*, '(A, F10.4, A)') " Velocity at t = 1 second = ", velocity, " units/sec"
    PRINT *, "=============================================="

END PROGRAM ForwardVelocityWithTable
