! Limits . f90 : Determines machine precision
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program Real4
   Implicit None
   Integer :: i , n
   Real *4 :: epsilon_m , one
   n=60 ! Establish the number of iterations
   ! Set initial values :
   epsilon_m = 1.0
  one = 1.0
  ! Within a DO−LOOP, calculate each step and print .
  ! This loop will execute 60 times in a row as i is
  ! incremented from 1 to n ( since n = 60) :

  do i = 1, n , 1 ! Begin the do−loop
    epsilon_m = epsilon_m / 2.0 ! Reduce epsilon m
    one = 1.0 + epsilon_m ! Re−calculate one
    print * , i , one , epsilon_m ! Print values so far
  end do ! End loop when i>n
 End Program Real4 
