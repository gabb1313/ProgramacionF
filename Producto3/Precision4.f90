! Precision . f90 : Determina la precision de la computadora
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program Precision4
   Implicit None
   Integer :: i , n
   Real *4 :: epsilon_m , one
   n=60 
   epsilon_m = 1.0
  one = 1.0
  
  do i = 1, n , 1 
    epsilon_m = epsilon_m / 2.0 
    one = 1.0 + epsilon_m 
    print * , i , one , epsilon_m 
  end do 
 End Program Precision4
