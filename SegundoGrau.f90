!		a * x ** 2 + b * x + c
!		Raizes:
!		x1 = (-b + dsqrt(delta)) / (2.D0 * a)
!		x2 = (-b - dsqrt(delta)) / (2.D0 * a)
!		delta = b ** 2 - 4.D0 * a * c

!		Criticar o valor de a
		REAL*8 a, b, c, x1, x2, delta
		Open(1, file = 'dadosSegundoGrau.dat', status = 'unknown')

10		FORMAT('De o valor de a: ')
		Write(*,10)
		READ(*,11) a
11		FORMAT(F6.2)
!		
		If (a.eq.0.D0) Then
			Write(*,12)
12			FORMAT('Valor de a eh zero')
			Stop
		EndIf

13		FORMAT('De o valor de b: ')
		Write(*,13)
		READ(*,14) b
14		FORMAT(F6.2)

15		FORMAT('De o valor de c: ')
		Write(*,15)
		READ(*,16) c
16		FORMAT(F6.2)

		delta = b ** 2 - 4.D0 * a * c
!		Criticar valor de delta
		If (delta.lt.0.D0) Then
			Write(*,17)
17			FORMAT('Delta Negativo')
			Stop
		EndIf

		x1 = (-b + dsqrt(delta)) / (2.D0 * a)
		x2 = (-b - dsqrt(delta)) / (2.D0 * a)

18		FORMAT('Valor de x1: ')
		Write(*,19) x1
19		FORMAT(F8.2)

20		FORMAT('Valor de x2: ')
		Write(*,21) x2
21		FORMAT(F8.2)
		Close(1)
		End
