program A5Q1
    implicit none

    integer::n,i
    real::f_exact,Wed,w,simp13,simp38,a,b,h,x,u,f,t,f0,fn,trap,s1,s3,exact

    a=1
    b=9
    n=30
    x=a

    h=(b-a)/n

    exact=f_exact(b)-f_exact(a)

    do i=2,30
        x=a+i*h

        !Trap
        t=t+f(x)

        !Simp13
      if (mod(i,2)/=0) then
        s1=s1+f(x)*4
      else
        s1=s1+f(x)*2
      end if

      !Simp38
      if (mod(i,3)==0) then
        s3=s3+f(x)*2
      else
        s3=s3+f(x)*3
      end if

      !Weddle
      if (mod(i,6)==0) then
        w=w+2*f(x)
    else if (mod(i,3)==0) then
        w=w+6*f(x)
    else if (mod(i,2)==0) then
        w=w+f(x)
    else
        w=w+5*f(x)
      end if

      !write(*,*)i,'=',x,f(x)
    end do

    Wed=((3*h)/10)*(f(a)+w+f(b))

    simp38=((3*h)/8)*(f(a)+s3+f(b))

    Simp13=(h/3)*(f(a)+s1+f(b))

    Trap=(h/2)*(f(a)+2*t+f(b))
    !write(*,*)trap,simp13,simp38,Wed


    write(*,*)'    Method','           ','Approximate Value','     ','Exact Value','         ','Relative Error'
    write(*,*)'Trapezoidal Rule','     ',trap,'',exact,'   ',(abs((exact-trap)/exact)*100)
    write(*,*)"Simpson's 1/3 Rule",'   ',simp13,'',exact,'   ',(abs((exact-simp13)/exact)*100)
    write(*,*)"Simpson's 3/8 Rule",'   ',simp38,'',exact,'   ',(abs((exact-simp38)/exact)*100)
    write(*,*)"Weddle's Rule",'        ',Wed,'',exact,'   ',(abs((exact-Wed)/exact)*100)
end program

real function f(t)
    implicit none

    real::t
    f= ((2*t**2)+(t**2.5)-1)/(t**2)

end function

real function f_exact(t)
    implicit none

    real::t
    f_exact= (2.0*t+(2.0/3.0)*t**1.5+(1.0/t))

end function
