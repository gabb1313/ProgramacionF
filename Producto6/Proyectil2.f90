!************************************************  
!This program plots projectile motion of an object.  
!The program requires user input for initial velocity   
!and angle of the object.The algorithm uses a time   
!step of 0.01 second i.e. it calculates object's  
!location in the x and y plane every 0.01 second.  
!**********By: Waleed Ishaque, 2013**************

!Modificaciones para el curso de Programacion en FORTRAN
!Universidad de Sonora, Gabriela Carretas, 2015.

module constantes
implicit none
  real, parameter :: pi = 4.0*atan(1.0)
  real, parameter :: g = 9.80
  real, parameter :: ar = (4.0*atan(1.0))/180 
  real, parameter :: c = 0.47
  real, parameter :: ro = 1.29
  real, parameter :: rad = 0.05
  real, parameter :: m = 0.25
  integer, parameter :: npts = 5000

  !g es la gravedad, pi es "pi"
  !ar es para convertir grados a radianes
  !c es el coeficiente de arrastre de una esfera
  !ro es la densidad del aire a nivel del mar
  !rad es el radio de la esfera
  !m es la masa del proyectil
  !npts es la cantidad de puntos que graficara el programa

end module constantes

!---------------------------------------
!Subrutina 1
subroutine sinfriccion (ag, vo, xo, yo, rs, hs, ts)
  use constantes
  implicit none
  integer :: i
  real :: ag, vo, xo, yo, rs, hs, ts
  real, dimension (0:npts) :: vx, vy, x, y, t  
  !--------------------------------------
  !vo es la velocidad inicial del objeto   
  !ag es el angulo inicial del objeto
  !xo, yo son la posición inicial del objeto
 
  !ts es el tiempo total de vuelo
  !rs es la distancia maxima de x
  !hs es la altura maxima que alcanza el proyectil

  !vx, vy son contadores de las componentes de velocidad respectivas
  !x,y son contadores de las componentes de posición respectivas
  !i es un contador
  !-------------------------------------

  !Para convertir el angulo a radianes   
  ag = ag*ar

  !Las componentes de velocidades
  vx(0) = vo*cos(ag)
  vy(0) = vo*sin(ag)

  !Comenzaremos a graficar con este algoritmo   
  open(1, file='sinfriccion.dat')   
  do i=1, npts, 1

      !Calculamos el instante
     t(i) = float(i)*0.01 
     
     !Calculamos la posicion para cada instante     
     x(i) = xo+vx(0)*t(i)   
     y(i) = yo+vy(0)*t(i) - 0.5*g*t(i)*t(i)   

     !Escribimos los resultados en el algoritmo graficador   
     write(1,1001) x(i), y(i)
     1001 format (f11.5, f11.5)

     !El programa termina cuando vuelve al suelo   
     if (y(i)<0) exit   
  end do
  close(1)

  !Calculamos el tiempo total de vuelo
  ts = (2*vy(0))/g

  !Calculamos la altura maxima del proyectil
  hs = yo+((vy(0)*vy(0))/(2*g))

  !Calculamos la distancia maxima condicionada
  if (ag<=0) then
     rs = 0
  else if (ag==(pi/2)) then
     rs = 0
  else
     rs = xo+vx(0)*ts
  endif
end subroutine sinfriccion
    
!---------------------------------------
!Subrutina 2
subroutine confriccion (ag, vo, A, d, xo, yo, tf, hf, rf)    
  use constantes
  implicit none
  integer :: ii
  real :: ag, vo, A, d, xo, yo, tf, hf, rf
  real, dimension (0:npts) :: vxx, vyy, ax, ay, xx, yy, tt
  !----------------------
  !A es el area del proyectil, en este caso, una esfera
  !d es la densidad de la esfera con respecto al aire
  !tf es el tiempo total de vuelo con friccion
  !hf es la altura maxima que alcanza el proyectil con friccion  
  !rf es la distancia maxima de x con friccion
  
  !ax, ay son contadores de las componentes de velocidad respectivas con friccion
  !vxx, vyy son contadores de las componentes de velocidad respectivas con friccion  
  !xx,yy son contadores de las componentes de posicion respectivas con friccion
  !ii es un contador
  !----------------------

  !Para el area del proyectil
  A = rad*rad*pi
  
  !Para la densidad del proyectil
  d = (ro*c*A*0.5)

  !Las componentes de posicion iniciales
  xx(0) = xo
  yy(0) = yo

  !Las componentes de velocidad iniciales
  vxx(0) = vo*cos(ag)
  vyy(0) = vo*sin(ag)
  
  !Las componentes de aceleración iniciales
  ax(0) = -(d/m)*vxx(0)*vxx(0)
  ay(0) = -(g)-((d/m)*vyy(0)*vyy(0))

  !Tiempo inicial
  tt(0) = 0

  !Comenzaremos a graficar con este algoritmo   
  open(2, file='friccion.dat')   
  !Escribiendo los valores inciales
  write (2,1001) xx(0), yy(0)
  1001 format (f11.5, f11.5)
  do ii = 0, npts, 1
     
     !Calculamos el instante
     tt(ii+1) = tt(ii)+0.01 
    
     !Calculamos las componentes de velocidad para cada instante 
     vxx(ii+1) = vxx(ii)+ax(ii)*tt(ii+1)   
     vyy(ii+1) = vyy(ii)+ay(ii)*tt(ii+1)  

     !Calculamos las componentes de aceleracion para cada instante
     ax(ii+1) = -(d/m)*vxx(ii)*vxx(ii)
     ay(ii+1) = -(g)-((d/m)*vyy(ii)*vyy(ii))

     !Calculamos la posicion para cada instante
     xx(ii+1) = xx(ii)+vxx(ii)*tt(ii+1)+(0.5*ax(ii)*tt(ii+1)*tt(ii+1))
     yy(ii+1) = yy(ii)+vyy(ii)*tt(ii+1)+(0.5*ay(ii)*tt(ii+1)*tt(ii+1))
     
     !Escribimos los resultados en el algoritmo graficador   
     write(2,1001) xx(ii+1), yy(ii+1)  

     !El programa termina cuando vuelve al suelo   
     if (yy(ii+1)<0) exit   
  end do
  close(2)

  !Calculamos el tiempo total de vuelo
  tf = tt(ii)*10.0

  !Calculamos la altura maxima del proyectil
  hf = maxval(yy)

  !Calculamos el desplazamiento maximo del proyectil
  rf = xx(ii)
end subroutine confriccion

!----------------------------------------------------     
program proyectil2
  use constantes
  implicit none
  real :: ag, vo, xo, yo, rs, hs, ts
  real, dimension (0:npts) :: vx, vy, x, y, t, vxx, vyy, ax, ay, xx, yy, tt
  real :: A, d, tf, hf, rf
  real :: ex, ey
  !ex es el error porcentual en x
  !ey es el error porcentual en y

  !El usuario debe proporcionar datos iniciales
  write (*,*) 'Programa comparativo entre tiros parabolicos con y sin friccion del aire'
  write (*,*) 'Se tiene en consideracion de proyectil a una esfera con las siguientes especificaciones:'
  write (*,*) 'Masa=0.25 kg y Radio=0.05 m'  
  write (*,*) '------------------------------------------------'  
  write (*,*) 'Ingrese el angulo del proyectil en grados (Real)'   
  read *, ag   
  write (*,*) 'Ingrese la posicion inicial x en metros (Real)'   
  read *, xo
  write (*,*) 'Ingrese la posicion inicial y en metros (Real)'   
  read *, yo
  write (*,*) 'Ingrese la velocidad del proyectil en m/s (Real)'   
  read *, vo  
  write (*,*) '------------------------------------------------'   
  !---------------------------------------
  call sinfriccion (ag, vo, xo, yo, rs, hs, ts)
  call confriccion (ag, vo, A, d, xo, yo, tf, hf, rf)

  !Para calcular el error porcentual del tiro sin friccion
  ex = ((rs-rf)/rf)*100
  ey = ((hs-hf)/hf)*100

  !Generamos los resultados para el usuario
  write (*,*) 'Para un proyectil con velocidad inicial=',vo,'m/s y un angulo=',ag,'radianes'
  write (*,*) '------------------------------------------------'  
  write (*,*) 'Resultados en el tiro sin friccion:'
  write (*,*) 'Tiempo total de vuelo=',ts,'s'
  write (*,*) 'Altura maxima=',hs,'m'
  write (*,*) 'Distancia maxima=',rs,'m'
  write (*,*) '------------------------------------------------'  
  write (*,*) 'Resultados en el tiro con friccion:'
  write (*,*) 'Tiempo total de vuelo=',tf,'s'
  write (*,*) 'Altura maxima=',hf,'m'
  write (*,*) 'Distancia maxima=',rf,'m'
  write (*,*) '------------------------------------------------'  
  write (*,*) 'La diferencia en x entre ambos casos es=',ex,'%'
  write (*,*) 'La diferencia en y entre ambos casos es=',ey,'%'
  !cerramos el programa   
end program proyectil2

