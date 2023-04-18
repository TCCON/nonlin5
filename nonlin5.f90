PROGRAM nonlin5
	
	IMPLICIT NONE
	
	INTEGER(KIND = 4) :: argcnt
	CHARACTER(LEN = 255) :: inpfile
	INTEGER(KIND = 4) :: l_inpfile
	INTEGER(KIND = 4) :: st_inpfile
	
	INTEGER(KIND = 4), PARAMETER :: iou = 11
	INTEGER(KIND = 4) :: ioerr
	INTEGER(KIND = 4) :: par_no
	INTEGER(KIND = 4) :: l_ro
	CHARACTER(LEN = 255) :: ro
	CHARACTER(LEN = 255) :: ifgfile
	CHARACTER(LEN = 255) :: testfile
	CHARACTER(LEN = 255) :: newfile
	CHARACTER(LEN = 255) :: paramsfile
	CHARACTER(LEN = 255) :: cyclesfile
	REAL(KIND = 8) :: sia
	REAL(KIND = 8) :: fvsi
	INTEGER(KIND = 4) :: maxcycle
	REAL(KIND = 8) :: initstep
	REAL(KIND = 8) :: stepadjcoef
	INTEGER(KIND = 4) :: ntry
	REAL(KIND = 8) :: minstep
	
	INTEGER(KIND = 4), PARAMETER :: mip = 2**24
	REAL(KIND = 4), DIMENSION(mip) :: ifg
	INTEGER(KIND = 4) :: st_ifg
	INTEGER(KIND = 4) :: counter
	INTEGER(KIND = 4) :: nifgtot
	INTEGER(KIND = 4) :: i
	REAL(KIND = 8) :: dati2000
	
	INTEGER(KIND = 4) :: iposfwd
	REAL(KIND = 4) :: maxvalfwd
	INTEGER(KIND = 4) :: iposbwd
	REAL(KIND = 4) :: maxvalbwd
	
	INTEGER(KIND = 4), PARAMETER :: nifg = 256
	REAL(KIND = 8), DIMENSION(-nifg:nifg) :: ifgfwd
	REAL(KIND = 8), DIMENSION(-nifg:nifg) :: ifgbwd
	REAL(KIND = 8), DIMENSION(-nifg:nifg) :: apo
	REAL(KIND = 8), DIMENSION(0:nifg) :: specfwd
	REAL(KIND = 8), DIMENSION(0:nifg) :: specbwd
	
	REAL(KIND = 8), DIMENSION(0:nifg) :: wsignal
	REAL(KIND = 8), DIMENSION(0:nifg) :: wparasit
	
	REAL(KIND = 8) :: pi
	REAL(KIND = 8) :: xval
	REAL(KIND = 8) :: nue
	
	REAL(KIND = 8) :: costval
	REAL(KIND = 8) :: newcostval
	
	REAL(KIND = 8), DIMENSION(-nifg:nifg) :: wrkifgfwd
	REAL(KIND = 8), DIMENSION(-nifg:nifg) :: wrkifgbwd
	
	REAL(KIND = 8) :: a
	REAL(KIND = 8) :: b
	REAL(KIND = 8) :: c
	
	REAL(KIND = 8) :: stepadj
	INTEGER(KIND = 4) :: itry
	INTEGER(KIND = 4) :: icycle
	REAL(KIND = 8) :: anew
	REAL(KIND = 8) :: bnew
	REAL(KIND = 8) :: cnew
	REAL(KIND = 8) :: rnd
	
	INTRINSIC TRIM, LEN_TRIM, MOD, ATAN, DBLE, REAL, COS, &
				RANDOM_SEED, RANDOM_NUMBER, &
				COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT

	WRITE(*, '(/, A, /)') 'Nonlin 5 FX Turbo 4x4'
	
	! Get the input file name
	argcnt = COMMAND_ARGUMENT_COUNT()
	SELECT CASE (argcnt)
		CASE (0)
			inpfile = 'nonlin5.inp'
			l_inpfile = LEN_TRIM(inpfile)
		CASE (1)
			CALL GET_COMMAND_ARGUMENT(1, inpfile, l_inpfile, st_inpfile)
			IF (st_inpfile /= 0) THEN
				WRITE(*, '(A)') 'Error, getting input file name failed'
				STOP
			END IF
		CASE DEFAULT
			WRITE(*, '(A)') 'Error, command not recognized; wrong number of arguments'
			STOP
	END SELECT
	WRITE(*, '(A)') 'Input file: ' // inpfile(1:l_inpfile)

	! Read the input file
	OPEN(iou, FILE = inpfile(1:l_inpfile), STATUS = 'OLD', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open ' // inpfile(1:l_inpfile)
		STOP
	END IF
	
	par_no = 1
	DO
		READ(iou, '(A)', ERR = 100, END = 100) ro
		
		l_ro = LEN_TRIM(ro)
		IF (l_ro == 0) THEN
			CYCLE
		END IF

		IF (ro(1:1) == '%') THEN
			CYCLE
		ELSE
			SELECT CASE (par_no)
				CASE (1)
					READ(ro, '(A)', ERR = 100, END = 100) ifgfile
				CASE (2)
					READ(ro, '(A)', ERR = 100, END = 100) testfile
				CASE (3)
					READ(ro, '(A)', ERR = 100, END = 100) newfile
				CASE (4)
					READ(ro, '(A)', ERR = 100, END = 100) paramsfile
				CASE (5)
					READ(ro, '(A)', ERR = 100, END = 100) cyclesfile
				CASE (6)
					READ(ro, *, ERR = 100, END = 100) sia
				CASE (7)
					READ(ro, *, ERR = 100, END = 100) fvsi	
				CASE (8)
					READ(ro, *, ERR = 100, END = 100) maxcycle
				CASE (9)
					READ(ro, *, ERR = 100, END = 100) initstep
				CASE (10)
					READ(ro, *, ERR = 100, END = 100) stepadjcoef
				CASE (11)
					READ(ro, *, ERR = 100, END = 100) ntry
				CASE (12)
					READ(ro, *, ERR = 100, END = 100) minstep
					EXIT		
			END SELECT
			par_no = par_no + 1
		END IF
	END DO

	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // inpfile(1:l_inpfile)
		STOP
	END IF
	
	! Get the interferogram
	CALL get_ifg_opus(ifgfile, mip, nifgtot, ifg, dati2000, st_ifg)
	
	IF (st_ifg /= 0) THEN
		WRITE(*, '(A)') 'Error in reading from ' // TRIM(ifgfile)
		STOP
	END IF

	! Check whether the interferogram is even
	WRITE(*, '(A, I0)') 'IFG points: ', nifgtot
	IF (MOD(nifgtot, 2) /= 0) THEN
		WRITE(*, '(A)') 'Error, IFG points is not even'
		STOP
	END IF
	
	! Locate the FWD and BWD centerburst	
	! FWD
	iposfwd = 0
	maxvalfwd = 0.0
	DO i = 1, nifgtot / 2
		IF (ifg(i) > maxvalfwd) THEN
			maxvalfwd = ifg(i)
			iposfwd = i
		END IF
	END DO
	
	! BWD
	iposbwd = 0
	maxvalbwd = 0.0
	DO i = nifgtot / 2 + 1, nifgtot
		IF (ifg(i) > maxvalbwd) THEN
			maxvalbwd = ifg(i)
			iposbwd = i
		END IF
	END DO
	
	WRITE(*, '(A, I0, A, I0)') 'IFG centerburst position (FWD, BWD): ', iposfwd, ', ',  iposbwd
	WRITE(*, '(A, F6.3, A, F6.3)') 'IFG centerburst value (FWD, BWD): ', maxvalfwd, ', ',   maxvalbwd
	
	! Cut out the double-sided centerburst ifgs
	! FWD
	counter = -nifg
	DO i = iposfwd - nifg, iposfwd + nifg
		ifgfwd(counter) = REAL(ifg(i), 8)
		counter = counter + 1
	END DO
	
	! BWD	
	counter = -nifg
	DO i = iposbwd - nifg, iposbwd + nifg
		ifgbwd(counter) = REAL(ifg(i), 8)
		counter = counter + 1
	END DO
	
	! Generate the apodization function
	pi = 4.0 * ATAN(1.0)
	DO i = -nifg, nifg
		xval = DBLE(i) / DBLE(nifg)
		apo(i) = 0.5 + 0.5 * COS(pi * xval)
	END DO
	
	! Perform the DFT
	CALL dft(nifg, ifgfwd, apo, specfwd)
	CALL dft(nifg, ifgbwd, apo, specbwd)
	
	! Output the original spectra
	OPEN(iou, FILE = TRIM(testfile), STATUS = 'REPLACE', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open ' // TRIM(testfile)
		STOP
	END IF
	
	DO i = 0, nifg
		nue = 15798.0 * DBLE(i) / DBLE(nifg)
		WRITE(iou, '(F12.6, F12.7, F12.7)', ERR = 101) nue, specfwd(i), specbwd(i)
	END DO
	
	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // TRIM(testfile)
		STOP
	END IF
	
	! Define the weighting regions for the parent and parasitic (out-of-band) signal
	DO i = 0, nifg
		wsignal(i) = 0.0
		wparasit(i) = 0.0
		nue = 15798.0 * DBLE(i) / DBLE(nifg)
		IF ((nue > 4100.0) .AND. (nue < 9700.0)) THEN
			wsignal(i) = 1.0
		END IF
		IF ((nue > 100.0) .AND. (nue < 3600.0)) THEN
			wparasit(i) = 1.0
		END IF
		IF ((nue > 14200.0) .AND. (nue < 15750.0)) THEN
			wparasit(i) = 1.0
		END IF
	END DO
	
	! Search for the improved nonlinearity  coefficients
	OPEN(iou, FILE = TRIM(cyclesfile), STATUS = 'REPLACE', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open ' // TRIM(cyclesfile)
		STOP
	END IF
	
	CALL RANDOM_SEED()
	
	costval = costfun(nifg, specfwd, specbwd, wsignal, wparasit)
	
	a = 0.0
	b = 0.0
	c = 0.0
	stepadj = 1.0

	WRITE(*, '(I5, A, ES12.3)') 0, ' Initial step width: ', initstep
	WRITE(*, '(I5, A, ES23.15, ES11.3, ES11.3, ES11.3)') 0, ' Initial values: ', costval, a, b, c

	itry = 0
	DO icycle = 1, maxcycle
	
		CALL RANDOM_NUMBER(rnd)
		anew = a + stepadj * initstep * (rnd - 0.5)
		IF (anew < 0.0) THEN
			anew = 0.0
		END IF
		CALL RANDOM_NUMBER(rnd)
		bnew = b + stepadj * initstep * (rnd - 0.5)
		IF (bnew < 0.0) THEN
			bnew = 0.0
		END IF
		CALL RANDOM_NUMBER(rnd)
		cnew = c + stepadj * initstep * (rnd - 0.5)
		IF (cnew < 0.0) THEN
			cnew = 0.0
		END IF
		
		CALL restoreifg(anew, bnew, cnew, nifg, ifgfwd, ifgbwd, wrkifgfwd, wrkifgbwd)
		
		CALL dft(nifg, wrkifgfwd, apo, specfwd)
		CALL dft(nifg, wrkifgbwd, apo, specbwd)
		
		newcostval = costfun(nifg, specfwd, specbwd, wsignal, wparasit)
		
		IF (newcostval < costval) THEN
			WRITE(*, '(I5, A, ES23.15, ES11.3, ES11.3, ES11.3)') icycle, &
				' Improved values found: ', newcostval, anew, bnew, cnew
			WRITE(iou, '(I5, ES23.15, ES11.3, ES11.3, ES11.3)', ERR = 102) icycle, &
				newcostval, anew, bnew, cnew		
			costval = newcostval
			a = anew
			b = bnew
			c = cnew
			itry = 0
		ELSE
			WRITE(iou, '(I5, ES23.15, ES11.3, ES11.3, ES11.3)', ERR = 102) icycle, &
				newcostval, anew, bnew, cnew
			itry = itry + 1
			IF (itry > ntry) THEN
				stepadj = stepadjcoef * stepadj
				IF ((stepadj * initstep) < minstep) THEN
					WRITE(*, '(I5, A)') icycle, ' Finished! Step width below the threshold'
					EXIT
				END IF
				WRITE(*, '(I5, A, ES12.3)') icycle, ' New step width: ', stepadj * initstep
				itry = 0
			END IF
			
		END IF

	END DO

	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // TRIM(cyclesfile)
		STOP
	END IF
	
	CALL restoreifg(a, b, c, nifg, ifgfwd, ifgbwd, wrkifgfwd, wrkifgbwd)
	
	CALL dft(nifg, wrkifgfwd, apo, specfwd)
	CALL dft(nifg, wrkifgbwd, apo, specbwd)
	
	! Output the optimized spectra and parameters
	OPEN(iou, FILE = TRIM(newfile), STATUS = 'REPLACE', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open ' // TRIM(newfile)
		STOP
	END IF
	
	! Spectra
	DO i = 0, nifg
		nue = 15798.0 * DBLE(i) / DBLE(nifg)
		WRITE(iou, '(F12.6, F12.7, F12.7)', ERR = 103) nue, specfwd(i), specbwd(i)
	END DO

	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // TRIM(newfile)
		STOP
	END IF
	
	! Parameters
	OPEN(iou, FILE = TRIM(paramsfile), STATUS = 'REPLACE', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open: ' // TRIM(paramsfile)
		STOP
	END IF
	
	WRITE(iou, '(F20.8)', ERR = 104) dati2000
	WRITE(iou, '(F8.1, F10.4)', ERR = 104) sia, fvsi
	WRITE(iou, '(2I8)', ERR = 104) iposfwd, iposbwd
	WRITE(iou, '(2F8.3)', ERR = 104) maxvalfwd, maxvalbwd
	WRITE(iou, '(ES23.15)', ERR = 104) costval
	WRITE(iou, '(I5)', ERR = 104) icycle
	WRITE(iou, '(3ES11.3)', ERR = 104) a, b, c

	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // TRIM(paramsfile)
		STOP
	END IF
	
	STOP
	
	! In case of error
100 WRITE(*, '(A)') 'Error in reading from ' // inpfile(1:l_inpfile)
	STOP
101 WRITE(*, '(A)') 'Error in writing to ' // TRIM(testfile)
	STOP
102 WRITE(*, '(A)') 'Error in writing to ' // TRIM(cyclesfile)
	STOP	
103 WRITE(*, '(A)') 'Error in writing to ' // TRIM(newfile)
	STOP	
104 WRITE(*, '(A)') 'Error in writing to ' // TRIM(paramsfile)
	STOP	
	
	CONTAINS
	
		FUNCTION costfun(nifg, specfwd, specbwd, wsignal, wparasit)
			
			! Input variables
			INTEGER(KIND = 4), INTENT(IN) :: nifg
			REAL(KIND = 8), DIMENSION(0:nifg), INTENT(IN) :: specfwd
			REAL(KIND = 8), DIMENSION(0:nifg), INTENT(IN) :: specbwd
			REAL(KIND = 8), DIMENSION(0:nifg), INTENT(IN) :: wsignal
			REAL(KIND = 8), DIMENSION(0:nifg), INTENT(IN) :: wparasit
			
			! Output variables
			REAL(KIND = 8) :: costfun
			
			! Other variables
			INTEGER(KIND = 4) :: i
			REAL(KIND = 8) :: refval
			REAL(KIND = 8) :: costval
			
			refval = 0.0
			costval = 0.0
			DO i = 0, nifg
				refval = refval + (specfwd(i) + specbwd(i)) * wsignal(i)
				costval = costval + (specfwd(i) + specbwd(i)) * wparasit(i)
			END DO
			costfun = costval / refval
			
		END FUNCTION costfun
		
		SUBROUTINE restoreifg(a, b, c, nifg, ifgfwd, ifgbwd, wrkifgfwd, wrkifgbwd)
		
			! Input variables
			REAL(KIND = 8), INTENT(IN) :: a
			REAL(KIND = 8), INTENT(IN) :: b
			REAL(KIND = 8), INTENT(IN) :: c
			INTEGER(KIND = 4), INTENT(IN) :: nifg
			REAL(KIND = 8), DIMENSION(-nifg:nifg), INTENT(IN) :: ifgfwd
			REAL(KIND = 8), DIMENSION(-nifg:nifg), INTENT(IN) :: ifgbwd
			
			! Output variables
			REAL(KIND = 8), DIMENSION(-nifg:nifg), INTENT(OUT) :: wrkifgfwd
			REAL(KIND = 8), DIMENSION(-nifg:nifg), INTENT(OUT) :: wrkifgbwd
			
			! Other variables
			INTEGER(KIND = 4) :: i
			
			DO i = -nifg, nifg
				wrkifgfwd(i) = ifgfwd(i) + &
					a * ifgfwd(i) * ifgfwd(i) + &
					b * ifgfwd(i) * ifgfwd(i) * ifgfwd(i) + &
					c * ifgfwd(i) * ifgfwd(i) * ifgfwd(i) * ifgfwd(i)		
				wrkifgbwd(i) = ifgbwd(i) + &
					a * ifgbwd(i) * ifgbwd(i) + &
					b * ifgbwd(i) * ifgbwd(i) * ifgbwd(i) + &
					c * ifgbwd(i) * ifgbwd(i) * ifgbwd(i) * ifgbwd(i)	
			END DO
			
		END SUBROUTINE restoreifg
		
		SUBROUTINE dft(nifg, ifg, apo, spec)
			
			! Input variables
			INTEGER(KIND = 4), INTENT(IN) :: nifg
			REAL(KIND = 8), DIMENSION(-nifg:nifg), INTENT(IN) :: ifg
			REAL(KIND = 8), DIMENSION(-nifg:nifg), INTENT(IN) :: apo
			
			! Output variables
			REAL(KIND = 8), DIMENSION(0:nifg), INTENT(OUT) :: spec
			
			! Other variables
			INTEGER(KIND = 4) :: i
			INTEGER(KIND = 4) :: j
			REAL(KIND = 8) :: pi
			REAL(KIND = 8) :: factor
			REAL(KIND = 8) :: meanifg
			REAL(KIND = 8) :: valcos
			REAL(KIND = 8) :: valsin
			REAL(KIND = 8) :: argument
			REAL(KIND = 8) :: valifg

			pi = 4.0 * ATAN(1.0)
			factor = pi / DBLE(nifg)

			meanifg = 0.0		
			DO i = -nifg, nifg
				meanifg = meanifg + ifg(i)		
			END DO
			meanifg = meanifg / DBLE(2 * nifg + 1)

			DO i = 0, nifg
				valcos = 0.0
				valsin = 0.0
				DO j = -nifg, nifg
					argument = factor * DBLE(i) * DBLE(j)
					valifg = apo(j) * (ifg(j) - meanifg)
					valcos = valcos + valifg * COS(argument)
					valsin = valsin + valifg * SIN(argument)
				END DO	
				spec(i) = SQRT(valcos * valcos + valsin * valsin)
			END DO

		END SUBROUTINE dft
		
END PROGRAM nonlin5





