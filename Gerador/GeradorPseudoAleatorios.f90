		REAL * 8 RNUM(10000), dmax, pmod
!		rnum é o numero pseudo-aleatório
		Integer ISEED, numero
!		ISEED = semente
!		num = quantidade de rnum
		OPEN(1,file='dados.dat',Status='unknown')
		pmod = 2147483647.D0
		dmax = 1.D0/pmod
		Write(*,80)
80		FORMAT('Valor inicial ou semente')
		READ(*,92) ISEED
92		FORMAT(I5)
		Write(*,81) ISEED
81		FORMAT('A semente eh:',I8)
		Write(*,82)
82		FORMAT('quantidade de nros aleatorios a serem gerados')
		READ(*,92) num
		Write(*,83)
		Write(1,83)
83		FORMAT('quantidade de numeros aleatorios gerados')
!		Inicializacao
		DO 1 i = 1, 10000
1			RNUM(i) = 0.D0
		RNUM(1) = ISEED * dmax
		Do 10 i = 2, num
			RNUM(i) = cong(ISEED)
			if(num.le.200) Write(*,93) RNUM(i),ISEED
			Write(1,93) RNUM(i),ISEED
93			FORMAT(F10.5,I15)
10		continue
		close(1)
		end

		FUNCTION	cong(ISEED)
			REAL*8 RMod,PMod,dmax
			Integer ISEED, IMod, c
			c = 1
			RMod = DFLOAT(ISEED)
			PMod = 1024.D0
			dmax = 1.0D0/PMod
			RMod = RMod * 129.D0 + c
			IMod = RMod * dmax
			RMod = RMod - PMod * IMod
			cong = RMod * dmax
			ISEED = RMod
			RETURN
		end


