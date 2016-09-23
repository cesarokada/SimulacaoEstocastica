		REAL * 8 RNUM(10000), dmax, pmod, sum, sumVar, media, var
		REAL * 8 desvioPadrao, sumPearson, corrPearson, sumY, sumVarY
		REAL * 8 fact1, fact2, fact3, qui
!		rnum é o numero pseudo-aleatório
		Integer ISEED, numero, contQui(10)
!		ISEED = semente
!		num = quantidade de rnum
		OPEN(1,file='dados.dat',Status='unknown')
		pmod = 2147483647.D0
		dmax = 1.D0/pmod
		sum = 0.D0
		media = 0.D0
		sumVar = 0.D0
		sumPearson = 0.D0
		sumY = 0.D0
		sumVarY = 0.D0
		qui = 0.D0
		DO i = 1, 10
			contQui(i) = 0
		END DO
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
			sum = sum + RNUM(i)
			sumY = sumY + RNUM(i-1)
			sumVarY = sumVarY + RNUM(i-1)**2
			sumVar = sumVar + RNUM(i)**2
			sumPearson = sumPearson + (RNUM(i) * RNUM(i-1))

			if(RNUM(i) .ge. 0.D0  .and. RNUM(i) .lt. 0.1D0) then
				contQui(1) = contQui(1) + 1
			else if(RNUM(i) .ge. 0.1D0 .and. RNUM(i) .lt. 0.2D0) then
				contQui(2) = contQui(2) + 1
			else if(RNUM(i) .ge. 0.2D0 .and. RNUM(i) .lt. 0.3D0) then
				contQui(3) = contQui(3) + 1
			else if(RNUM(i) .ge. 0.3D0 .and. RNUM(i) .lt. 0.4D0) then 
				contQui(4) = contQui(4) + 1
			else if(RNUM(i) .ge. 0.4D0 .and. RNUM(i) .lt. 0.5D0) then 
				contQui(5) = contQui(5) + 1
			else if(RNUM(i) .ge. 0.5D0 .and. RNUM(i) .lt. 0.6D0) then
				contQui(6) = contQui(6) + 1
			else if(RNUM(i) .ge. 0.6D0 .and. RNUM(i) .lt. 0.7D0) then
				contQui(7) = contQui(7) + 1
			else if(RNUM(i) .ge. 0.7D0 .and. RNUM(i) .lt. 0.8D0) then
				contQui(8) = contQui(8) + 1
			else if(RNUM(i) .ge. 0.8D0 .and. RNUM(i) .lt. 0.9D0) then
				contQui(9) = contQui(9) + 1
			else if(RNUM(i) .ge. 0.9D0 .and. RNUM(i) .lt. 1.0D0) then
				contQui(10) = contQui(10) + 1
			end if

			if(num.le.200) Write(*,93) RNUM(i),ISEED
			Write(1,93) RNUM(i),ISEED
93			FORMAT(F10.5,I15)
10		continue
		media = sum/dfloat(num)
		var = (sumVar - (sum**2)/DFLOAT(num - 1)) / DFLOAT(num-2)
		desvioPadrao = dsqrt(var)

		fact1 = DFLOAT(num - 1) * sumPearson - (sum*sumY)
		fact2 = dsqrt(DFLOAT(num - 1) * sumVar - sum**2)
		fact3 = dsqrt(DFLOAT(num - 1) * sumVarY - sumY**2)
		corrPearson = fact1 / (fact2 * fact3)

		DO i = 1, 10
			fact1 = contQui(i) - ((num - 1)/10)
			fact2 = (fact1**2) / ((num - 1) / 10)
			qui = qui + fact2
		END DO

		Write(*,95) media
		Write(*,96) desvioPadrao
		Write(*,97) corrPearson
		Write(*,98) qui
95		FORMAT('Media: ',F6.2)
96		FORMAT('desvio Padrao: ',F6.2)
97		FORMAT('Correlacao Pearson: ',F10.5)
98		FORMAT('Teste Quiquadrado: ',F10.5)
		close(1)
		end

		FUNCTION	cong(ISEED)
			REAL*8 RMod,PMod,dmax
			Integer ISEED, IMod
			RMod = DFLOAT(ISEED)
			PMod = 2147483647.D0
			dmax = 1.0D0/PMod
			RMod = RMod * 16807.D0
			IMod = RMod * dmax
			RMod = RMod - PMod * IMod
			cong = RMod * dmax
			ISEED = RMod
			RETURN
		end


