!	PROGRAM TO GENERATE COORDINATES OF N-SIDED POLYGON
!	DATE - 05/04/2023
!	AUTHOR - SHUBHADEEP NAG

	implicit none

	real,allocatable :: x(:),y(:),z(:)
	real,allocatable :: p1x(:),p1y(:),p1z(:),ap1x(:),ap1y(:),ap1z(:)
	real,allocatable :: p2x(:),p2y(:),p2z(:),ap2x(:),ap2y(:),ap2z(:)
	real :: a,r,sgma,theta0, theta1, theta2
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
	allocate (p1x(1:natom),p1y(1:natom),p1z(1:natom),ap1x(1:natom),ap1y(1:natom),ap1z(1:natom))
	allocate (p2x(1:natom),p2y(1:natom),p2z(1:natom),ap2x(1:natom),ap2y(1:natom),ap2z(1:natom))

	sgma = 2.0; z = a*0.5

	write(2,*) natom*3
	write(2,*) natom,'-sided Polygon coordinates'

	do iatom = 1,natom

		x(iatom) = r*sin(2*3.14159*iatom*1.0/natom) + a*0.5; y(iatom) = r*cos(2*3.14159*iatom*1.0/natom) + a*0.5
		
		write(2,*) 'Ar',x(iatom),y(iatom),z(iatom)!,2*3.14*iatom*1.0/natom

	enddo !iatom
	theta0 = (2*natom-4)*90/natom
	theta1 = (90-theta0*0.5)*0.0174533
	theta2 = (180-(90-theta0*0.5))*0.0174533
	write(*,*) theta0, theta1, theta2
	do iatom = 1,natom

		p1x(iatom) = (x(iatom) + sgma*0.5*cos(-theta1))
		p1y(iatom) = (y(iatom) + sgma*0.5*sin(-theta1))
		p1z(iatom) = a*0.5
	
		p2x(iatom) = (x(iatom) + sgma*0.5*cos(-theta2))
		p2y(iatom) = (y(iatom) + sgma*0.5*sin(-theta2))
		p2z(iatom) = a*0.5

	enddo !iatom

	do iatom = 1,natom

		ap1x(iatom) = ((p1x(iatom)-x(iatom))*cos(-2*3.14159*iatom/natom) &
				- (p1y(iatom)-y(iatom))*sin(-2*3.14159*iatom/natom))+x(iatom)
		ap1y(iatom) = ((p1x(iatom)-x(iatom))*sin(-2*3.14159*iatom/natom) &
				+ (p1y(iatom)-y(iatom))*cos(-2*3.14159*iatom/natom))+y(iatom)
		ap1z(iatom) = a*0.5
	
		ap2x(iatom) = ((p2x(iatom)-x(iatom))*cos(-2*3.14159*iatom/natom) &
				- (p2y(iatom)-y(iatom))*sin(-2*3.14159*iatom/natom))+x(iatom)
		ap2y(iatom) = ((p2x(iatom)-x(iatom))*sin(-2*3.14159*iatom/natom) &
				+ (p2y(iatom)-y(iatom))*cos(-2*3.14159*iatom/natom))+y(iatom)
		ap2z(iatom) = a*0.5
		
		write(2,*) 'P',ap1x(iatom),ap1y(iatom),ap1z(iatom)
		write(2,*) 'P',ap2x(iatom),ap2y(iatom),ap2z(iatom)

	enddo !iatom
	
	end