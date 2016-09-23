! 			Calculo da area de um circulo a = pi*r**2
!
		REAL*8 a, pi, r
		Open(1, file = 'dados.dat', status = 'unknown')
!		'new','old','seratch' 
		pi = 3.1415D0
		Write(*,10)
10		FORMAT('De o valor do Raio: ')
		Write(1,10)
		READ(*,11) r
11		FORMAT(F6.2)
		Write(*,11) r
		Print *,r
		Write (*,*) r
		Write(1,11) r

! 		Calculo da area
		a = pi * r ** 2
		Call Calculo(r,a)
		Write(*,13)
13		FORMAT('A area vale: ')
		Print *,a
		Write(*,12) a
12		FORMAT(F8.2)
		Write(1,12) a
		Close(1)
		end
		subroutine Calculo(r,a)
		REAL*8 r,a
		pi = 3.1415D0
		a = pi * r ** 2
		return
		end
