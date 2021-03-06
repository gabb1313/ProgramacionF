module datos
implicit none
  integer, parameter :: total = 7632 
  integer, parameter :: mes = 1440
  integer, parameter :: meses = 5760
  integer, parameter :: nomes = 5  
  integer, parameter :: dias = 159 
  integer, parameter :: dia =  48  
  integer, parameter :: mdia = 24  
  integer, parameter :: sdia = 96
  !------------------------    
  !donde *** es ***
  !total - datos totales utilizables en la hoja de calculo
  !mes - datos totales por mes
  !meses - datos de meses completos
  !nomes - numero total de meses
  !dias - numero total de dias
  !dia - datos por dia
  !mdia - datos para medio dia
  !sdia - datos para el doble de particiones por dia
  !------------------------    
end module datos
!----------------------------------------------------     
program mareitas
  use datos
  implicit none
  integer :: i, j
  real :: mmax, mtmax, dmax, dtmax, smax, stmax  
  real :: mmin, mtmin, dmin, dtmin, smin, stmin
  real, dimension (1:total) :: t, h, doy, dt, pm, pd, ps
  real, dimension (1:5) :: tt, hh  
  real, dimension (1:dias) :: ttt, hhh
  real, dimension (1:sdia) :: tti, hhe
  !------------------------    
  !donde *** es ***
  !i, j - contadores
  !mmax, dmax, smax - mareas maximas por mes, dia y en el doble 
  !de particiones por dia
  !mmin, dmin, smin - mareas minimas por mes, dia y en el doble
  !de particiones por dia
  !mtmax, dtmax, stmax - mareas maximas con tiempo por mes, dia 
  !y en el doble de particiones por dia
  !mtmin, dtmin, stmin - mareas minimas con tiempo por mes, dia 
  !y en el doble de particiones por dia
  !t, tt, ttt, tti - contadores de tiempo
  !h, hh, hhh, hhe - contadores de altura en la marea
  !doy - dia del año en que fue registrado el dato
  !pm, pd, ps - periodo de marea mensual, diaria y semidiurna
  !------------------------   

  !Comenzamos abriendo el archivo a analizar
  open (2, file="mareas.csv")
  open (3, file="mareas.dat", status="replace")

  !Recuperamos datos necesarios
  do i=1, total, 1
     read (2,*) t(i), h(i), doy(i), dt(i)
     write(3,*) dt(i), h(i)
  end do
  close (2)
  close (3)
  !------------------------  

  !Archivo para mareas por mes
  open (4, file="maxxmes.dat")
  open (5, file="minxmes.dat")

  !Para mareas altas
  do j=0, meses, mes
     mmax = -1
     do i=1, mes, 1
        if (h(i+j)>mmax) then
           mmax = h(i+j)
           mtmax = dt(i+j)
        end if
     end do
     write (4,*) mtmax, mmax
  end do
  close (4)

  !Para mareas bajas
  do j=0, meses, mes
     mmin = 0
     do i=1, mes, 1
        if (h(i+j)<mmin) then
           mmin = h(i+j)
           mtmin = dt(i+j)
        end if
     end do
     write (5,*) mtmin, mmin
  end do
  close (5)
  !------------------------  

  !Archivo para mareas por dia
  open (6, file="maxxdia.dat")
  open (7, file="minxdia.dat")
  !Para mareas altas
  do j=0, total-1, dia
     dmax = -1
     do i=1, dia, 1
        if (h(i+j)>dmax) then
           dmax = h(i+j)
           dtmax = dt(i+j)
        end if
     end do
  write (6,*) dtmax, dmax
  end do

  !Para mareas bajas
  do j=0, total-1, dia
     dmin = 0
     do i=1, dia, 1
        if (h(i+j)<dmin) then
           dmin = h(i+j)
           dtmin = dt(i+j)
        end if
     end do
     write (7,*) dtmin, dmin
  end do
  
  close (6)
  close (7)
  !------------------------  

  !Archivo para mareas semidiurnas
  open (8, file="maxxsd.dat")
  open (9, file="minxsd.dat")

  !Para mareas altas
  do j=0, total, mdia
     smax = -1
     do i=1, mdia, 1
        if (h(i+j)>smax) then
           smax = h(i+j)
           stmax = dt(i+j)
        end if
     end do
     write (8,*) stmax, smax
  end do

  !Para mareas bajas
  do j=0, total, mdia
     smin = 0
     do i=1, mdia, 1
        if (h(i+j)<smin) then
           smin = h(i+j)
           stmin = dt(i+j)
        end if
     end do
     write (9,*) stmin, smin
  end do
 
  close (8)
  close (9)
  !------------------------  
  !Calculamos el concentrado de mareas maximas en los datos completos
  open (4, file="maxxmes.dat")
  open (6, file="maxxdia.dat")
  open (8, file="maxxsd.dat")

  !Para los meses
  do i=1, nomes, 1
     read (4,*) tt(i), hh(i)
     if (i>1) then
        pm(i) = tt(i) - tt(i-1)
     end if
  end do

  !Para los dias
  do i=1, dias, 1
     read (6,*) ttt(i), hhh(i)
     if (i>1) then
        pd(i) = ttt(i) - ttt(i-1)
     end if
  end do
  close (4)
  close (6)

  !Para las mareas semidiurnas
  do i=1, sdia, 1
     read (8,*) tti(i), hhe(i)
     if (i>1) then
        ps(i) = tti(i) - tti(i-1)
     end if
  end do
  close (8)
  !------------------------  

  !Imprimimos los resultados en una hoja de datos
  open (10, file="resultados.dat")

  write (10,*) 'Ciclo lunar=', sum(pm)/4, 'dias'
  write (10,*) 'Marea diurna=', sum(pd)/(dias-1), 'dias'
  write (10,*) 'Marea semidiurna=', sum(ps)/(sdia-1), 'dias'

  close (10)
end program mareitas
