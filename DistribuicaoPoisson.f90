			REAL * 8 rnum(10000), dmax, pmod, p, y, lambda, b, fat
!			rnum eh o numero pseudo-aleatório
			INTEGER ISEED, num, x, n
!			ISEED = semente
!			num = quantidade de rnum
			OPEN(1, file='dados.dat', status='unknown')
			pmod = 2147483647.D0
			dmax = 1.D0/pmod
			lambda = 0.5D0
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
			rnum(1) = ISEED * dmax
			DO 10 i = 2, num
			p = 1.0D0
			n = 0
			b = exp(-lambda)
2			rnum(i) = cong(ISEED)
			p = p * rnum(i)
			if(p.ge.b) then
				n = n + 1
				goto 2
			else
				x = n - 1
			end if
			if(x.ge.0) then
				fat = 1	
				Do j = 1,x
					fat = fat * j
				end do
				y = ((lambda**x) * (exp(-lambda))) / fat
				WRITE(1, 93) x, y
93				FORMAT(I5, F10.5)
			end if
10		CONTINUE
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