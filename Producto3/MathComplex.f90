! Mathcomplex . f90 : demo de algunas funciones matematicas en Fortran
! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
Program Mathcomplex 
  Complex *8 :: x = -1.0 , y = 2.0 , z = 0 , xx , yy , zz
  xx = sqrt (x) 
  yy = atan (y) 
  zz = log (z)
  print * , xx, yy, zz 
End Program Mathcomplex
