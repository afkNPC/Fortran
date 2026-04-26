program prime
    implicit none

    integer::div_c

    call prime1(div_c)

end program

subroutine prime1(dc)
    implicit none
     integer::i,dc,n

    do n=1,20
        dc=0

      do i=1,n
         if ((mod(n,i))== 0) then
        dc=dc+1
        end if

      end do
      !write(*,*)dc

      if (dc==2) then
        write(*,*)n,"is prime"
        else
        write(*,*)n,"is not prime"

      end if

    end do

end subroutine
