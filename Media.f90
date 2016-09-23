!		Calculo de soma aritmetica
!		m = soma / n
		REAL*8 m, soma, val
		INTEGER n

		soma = 0.D0
8		FORMAT('Numero de notas: ')
		Write(*,8)
		READ(*,10) n
10		FORMAT(I3)

		Do i = 1,n
11			FORMAT('Valor nota: ')
			Write(*,11)
			READ(*,12) val
12			FORMAT(F6.2)
			soma = soma + val
		EndDo
		m = soma/dfloat(n)
13		FORMAT('Valor de media: ')
		Write(*,13)
		Write(*,15) m
15		FORMAT(F8.2)
		End
