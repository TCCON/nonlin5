PROGRAM collate_params5

	USE IFPORT
	
	IMPLICIT NONE
	
	INTEGER(KIND = 4) :: argcnt
	CHARACTER(LEN = 255) :: inpdir
	INTEGER(KIND = 4) :: l_inpdir
	INTEGER(KIND = 4) :: st_inpdir
	
	INTEGER(KIND = 4) :: n
	
	TYPE(FILE$INFO) :: info
	INTEGER(8) :: handle
	INTEGER(4) :: l
	
	INTEGER(KIND = 4) :: iou
	INTEGER(KIND = 4) :: ioerr
	
	INTEGER(KIND = 4), PARAMETER :: mpar = 200000
	REAL(KIND = 8), DIMENSION(mpar) :: dati2000
	REAL(KIND = 8), DIMENSION(mpar) :: sia
	REAL(KIND = 8), DIMENSION(mpar) :: fvsi
	INTEGER(KIND = 4), DIMENSION(mpar) :: iposfwd
	REAL(KIND = 4), DIMENSION(mpar) :: maxvalfwd
	INTEGER(KIND = 4), DIMENSION(mpar) :: iposbwd
	REAL(KIND = 4), DIMENSION(mpar) :: maxvalbwd
	REAL(KIND = 8), DIMENSION(mpar) :: costval
	INTEGER(KIND = 4), DIMENSION(mpar) :: icycle
	REAL(KIND = 8), DIMENSION(mpar) :: a
	REAL(KIND = 8), DIMENSION(mpar) :: b
	REAL(KIND = 8), DIMENSION(mpar) :: c
	
	INTEGER(KIND = 4), DIMENSION(mpar) :: ix
	INTEGER(KIND = 4), DIMENSION(1) :: m_ix
	INTEGER(KIND = 4) :: m_ix2
	INTEGER(KIND = 4) :: k
	REAL(KIND = 8) :: tmp
	INTEGER(KIND = 4) :: tmp2

	
	INTRINSIC COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT, &
		TRIM, MINLOC
	
	WRITE(*, '(/, A, /)') 'Collate Parameters 5 Deluxe'
	
	! Get the directory path
	argcnt = COMMAND_ARGUMENT_COUNT()
	SELECT CASE (argcnt)
		CASE (1)
			CALL GET_COMMAND_ARGUMENT(1, inpdir, l_inpdir, st_inpdir)
			IF (st_inpdir /= 0) THEN
				WRITE(*, '(A)') 'Error, getting directory path failed'
				STOP
			END IF
		CASE DEFAULT
			WRITE(*, '(A)') 'Error, command not recognized; wrong number of arguments'
			STOP
	END SELECT
	
	
	! Go through the parameter files
	WRITE(*, '(A)') 'Reading parameter files from ' // inpdir(1:l_inpdir) // ' ...'
	
	handle = FILE$FIRST
	n = 0
	DO
	
		! Get one parameter file 
		l = GETFILEINFOQQ(inpdir(1:l_inpdir) // 'params_*.dpt', info, handle)
		IF (l == 0) THEN
			EXIT
		END IF
		
		n = n + 1
		IF (n > mpar) THEN
			WRITE(*, '(A)') 'Error, too many files found'
			STOP
		END IF
		
		! Read the file
		OPEN(iou, FILE = TRIM(inpdir) // TRIM(info.name), STATUS = 'OLD', IOSTAT = ioerr)
		IF (ioerr /= 0) THEN
			WRITE(*, '(A)') 'Error, failed to open ' // TRIM(info.name)
			STOP
		END IF
		
		READ(iou, *, IOSTAT = ioerr) dati2000(n), sia(n), fvsi(n), &
			iposfwd(n), iposbwd(n), maxvalfwd(n), maxvalbwd(n), &
			costval(n), icycle(n), a(n), b(n), c(n)
		IF (ioerr /= 0) THEN
			WRITE(*, '(A)') 'Error, failed to read ' // TRIM(info.name)
			STOP
		END IF
		
		CLOSE(iou, IOSTAT = ioerr)
		IF (ioerr /= 0) THEN
			WRITE(*, '(A)') 'Error, failed to close ' // TRIM(info.name)
			STOP
		END IF
	
	END DO
	
	WRITE(*, '(I8, A)') n, ' files found'
	IF (n == 0) THEN
		STOP
	END IF

	! Sort the parameter arrays by time
	ix(1:n) = (/ (k, k = 1, n) /)
	DO k = 1, n - 1
		m_ix = MINLOC(dati2000(k:n))
		m_ix2 = m_ix(1) + (k - 1)
		tmp = dati2000(k)
		dati2000(k) = dati2000(m_ix2)
		dati2000(m_ix2) = tmp
		tmp2 = ix(k)
		ix(k) = ix(m_ix2)
		ix(m_ix2) = tmp2
	END DO
	
	sia(1:n) = sia(ix(1:n))
	fvsi(1:n) = fvsi(ix(1:n))
	iposfwd(1:n) = iposfwd(ix(1:n))
	iposbwd(1:n) = iposbwd(ix(1:n))
	maxvalfwd(1:n) = maxvalfwd(ix(1:n))
	maxvalbwd(1:n) = maxvalbwd(ix(1:n))
	costval(1:n) = costval(ix(1:n))
	icycle(1:n) = icycle(ix(1:n))
	a(1:n) = a(ix(1:n))
	b(1:n) = b(ix(1:n))
	c(1:n) = c(ix(1:n))
	
	! Write the parameters to a file
	WRITE(*, '(A)') 'Writing parameters to collate_params5.out ...'
	
	OPEN(iou, FILE = 'collate_params5.out', STATUS = 'REPLACE', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open collate_params5.out'
	END IF
	
	DO k = 1, n
	
		WRITE(iou, '(F20.8, F8.1, F10.4, 2I8, 2F8.3, ES23.15, I8, 3ES11.3)', &
			IOSTAT = ioerr) &
			dati2000(k), sia(k), fvsi(k), &
			iposfwd(k), iposbwd(k), maxvalfwd(k), maxvalbwd(k), &
			costval(k), icycle(k), a(k), b(k), c(k)
		IF (ioerr /= 0) THEN
			WRITE(*, '(A)') 'Error, failed to write to collate_params5.out'
			STOP
		END IF
	
	END DO
		
	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close collate_params5.out'
		STOP
	END IF
	
	WRITE(*, '(A)') 'DONE!'

END PROGRAM collate_params5
