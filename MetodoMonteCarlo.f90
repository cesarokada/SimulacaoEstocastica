			REAL * 8 rnum(10000), dmax, pmod, x, y, pi
!			rnum eh o numero pseudo-aleatório
			INTEGER ISEED, num, countIn, countOut
!			ISEED = semente
!			num = quantidade de rnum
			OPEN(1, file='dados.dat', status='unknown')
			pmod = 2147483647.D0
			dmax = 1.D0/pmod
			countOut = 0
			countIn = 0
			WRITE(*,80)
80		FORMAT('Valor inicial ou semente')
			READ(*,92) ISEED
92		FORMAT(I5)
			WRITE(*,81) ISEED
81		FORMAT('A semente eh: ',I8)
			WRITE(*,82)
82		FORMAT('Quantidade de nos aleatorios a serem gerados')
			READ(*,92) num
			WRITE(*,83)
83		FORMAT('Quantidade de numeros aleatorios gerados')
!
!			inicialização
!
			DO 1 i = 1, 10000
1			rnum(i) = 0.D0
!
			rnum(1) = ISEED * dmax
			DO 10 i = 2, num
			rnum(i) = cong(ISEED)
			x = rnum(i)
			y = rnum(i-1)
			if((x**2 + y**2).le.1) then
				countIn = countIn + 1 
			else 
				countOut = countOut + 1
			endif
10		CONTINUE
			pi = 4.D0 * (dfloat(countIn)/dfloat((countIn + countOut)))
			WRITE(*,95) pi
95			FORMAT(F10.5)
			CLOSE(1)
			END
!
			FUNCTION cong(ISEED)
			REAL * 8 rmod, pmod, dmax
			INTEGER ISEED, IMOD
			rmod = DFLOAT(ISEED)
			pmod = 2147483647.D0
			dmax = 1.0D0 / pmod
			rmod = rmod * 16807.D0
			IMOD = rmod * dmax
			rmod = rmod - pmod * IMOD
			cong = rmod * dmax
			ISEED = rmod
			RETURN
			END