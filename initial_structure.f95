!	PROGRAM TO GENERATE COORDINATES OF N-SIDED POLYGON
!	DATE - 05/04/2023
!	AUTHOR - SHUBHADEEP NAG

	implicit none

	real,allocatable :: x(:),y(:),z(:)
	real :: a,r
	integer :: iatom,natom

	open (2,file='initial_coordinate.xyz')

	write(*,*) 'Enter the number of sides of polygon you want to generate'

20	read(*,*) natom
	if(natom.le.0) then
	 write(*,*) 'I guess there is no polygon with 0 sides, so please give an integer number'
	 goto 20
	endif

	write(*,*) 'Now, give the circum-radius and do not put it 0'
	read(*,*) r

	write(*,*) 'Give the sides of the cube in which this polygon will be placed'
	read(*,*) a

	allocate (x(1:natom),y(1:natom),z(1:natom))

	z = a*0.5

	write(2,*) natom
	write(2,*) natom,'-sided Polygon coordinates'
	
	do iatom = 1,natom

		x(iatom) = r*cos(2*3.14*iatom*1.0/natom) + a*0.5; y(iatom) = r*sin(2*3.14*iatom*1.0/natom) + a*0.5

		write(2,*) 'Ar',x(iatom),y(iatom),z(iatom)!,2*3.14*iatom*1.0/natom

	enddo !iatom
	
	end