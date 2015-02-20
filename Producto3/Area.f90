! Area . f90 : Calcula el area de un circulo
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program areacirculo
  Implicit None 
   Real *8 :: radius , circum , area 
   Real *8 :: PI = 4.0 * atan(1.0)
  Integer :: model_n = 1 
   print * , 'Enter a radius:' 
   read * , radius 
  circum = 2.00 * PI * radius 
  area = radius * radius * PI 
  print * , 'Program number =' , model_n 
  print * , 'Radius =' , radius 
  print * , 'Circumference =' , circum 
  print * , 'Area =' , area 
 End Program areacirculo 
