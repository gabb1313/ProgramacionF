!************************************************  
!This program plots projectile motion of an object.  
!The program requires user input for initial velocity   
!and angle of the object.The algorithm uses a time   
!step of 0.01 second i.e. it calculates object's  
!location in the x and y plane every 0.01 second.  
!**********By: Waleed Ishaque, 2013************** 

!Modificaciones para el curso de Programacion en FORTRAN
!Universidad de Sonora, Gabriela Carretas, 2015.
program proyectil 
     implicit none  
     !Definimos las constantes:
     real, parameter :: pi = 4.0*atan(1.0) 
     real :: vo, ag, ar, t, h, r, vx, vy  
     real, parameter :: g = 9.80  
     real:: x(1000),y(1000)  
     integer :: i 

     !g es la gravedad, pi es "pi"
     !vo es la velocidad inicial del objeto   
     !ag es el angulo inicial del objeto 
     !ar es el angulo inicial del objeto convertido a radianes  
     !t es el tiempo total de vuelo
     !r es la distancia maxima de x
     !h es la altura maxima que alcanza el proyectil
     !x,y son las componentes de velocidad respectivas
     !i es un contador de desplazamiento

     !El usuario debe proporcionar datos iniciales  
     write(*,*) 'Ingrese el angulo del proyectil (Real)'   
     read *, ag   
     write(*,*) 'Ingrese la velocidad del proyectil (Real)'   
     read *, vo   
     
     !Para convertir el angulo a radianes   
     ar = ag*pi/180.0
     
     !Las componentes de velocidades
     vx = vo*cos(ar)
     vy = vo*sin(ar)

     !Comenzaremos a graficar con este algoritmo   
     open(1, file='proyectil.dat')   
     do i=1,1000
        !Calculamos las componentes de velocidad en x,y cada decima de segundo  
        t = (float(i)*0.01)   
        x(i) = vx*t   
        y(i) = vy*t - 0.5*g*t*t   
        !Escribimos los resultados en el algoritmo graficador   
        write(1,*) x(i), y(i)  
        !El programa termina cuando vuelve al suelo   
        if (y(i)<0) exit   
     end do
     close(1)
     
     !Calculamos el tiempo total de vuelo
     t = (2*vy)/g

     !Calculamos la altura maxima del proyectil
     h = (vy*vy)/2*g 

     !Calculamos la distancia maxima condicionada
     IF (ag<=0) THEN
        r = 0
     ELSE IF (ag==90) THEN
        r = 0
     ELSE
        r = vx*t
     ENDIF

     !Generamos los resultados para el usuario
     write (*,*) 'Para un proyectil con velocidad inicial=',vo,'m/s y un angulo=',ag,'grados'
     write (*,*) 'Resultados:'
     write (*,*) 'Tiempo total de vuelo=',t,'s'
     write (*,*) 'Altura maxima=',h,'m'
     write (*,*) 'Distancia maxima=',r,'m'

     !cerramos el programa   
end program proyectil 
