PROGRAM pre_nonlin5

	USE IFPORT
	
	IMPLICIT NONE
	
	INTEGER(KIND = 4), PARAMETER :: iou = 11
	INTEGER(KIND = 4) :: ioerr
	INTEGER(KIND = 4) :: par_no
	INTEGER(KIND = 4) :: l_ro
	CHARACTER(LEN = 255) :: ro
	
	CHARACTER(LEN = 255) :: nonlin5_path
	CHARACTER(LEN = 255) :: i2s_in_path
	CHARACTER(LEN = 20) :: chans
	CHARACTER(LEN = 20) :: ichan
	CHARACTER(LEN = 20) :: upside_down
	CHARACTER(LEN = 20) :: maxcycle
	CHARACTER(LEN = 20) :: initstep
	CHARACTER(LEN = 20) :: stepadjcoef
	CHARACTER(LEN = 20) :: ntry
	CHARACTER(LEN = 20) :: minstep
	INTEGER(KIND = 4) :: first_date
	INTEGER(KIND = 4) :: last_date
	REAL(KIND = 8) :: min_sia
	REAL(KIND = 8) :: max_fvsi
	INTEGER(4) :: maxlinenum
	CHARACTER(LEN = 3) :: jobs
	INTEGER(KIND = 4) :: l_nonlin5
	INTEGER(KIND = 4) :: l_i2s_in
	
	CHARACTER(LEN = 14) :: run_file
	INTEGER(KIND = 4), PARAMETER :: iou3 = 13
	INTEGER(4) :: rs
	
	CHARACTER(LEN = 21) :: script_file
	INTEGER(KIND = 4), PARAMETER :: iou4 = 14
	INTEGER(4) :: linenum
	INTEGER(4) :: filenum
	CHARACTER(LEN = 6) :: filenum_str

	TYPE(FILE$INFO) :: info
	INTEGER(8) :: handle
	CHARACTER(LEN = 255) :: i2s_in_file
	INTEGER(4) :: spec_date
	INTEGER(4) :: l
	INTEGER(4) :: i2s_par_no
	CHARACTER(LEN = 255) :: ifgfile_path
	
	CHARACTER(LEN = 21) :: spectrum_name
	INTEGER(4) :: year
	INTEGER(4) :: mon
	INTEGER(4) :: day
	INTEGER(4) :: run
	REAL(KIND = 8) :: lat
	REAL(KIND = 8) :: lon
	REAL(KIND = 8) :: alt
	REAL(KIND = 8) :: tins
	REAL(KIND = 8) :: pins
	REAL(KIND = 8) :: hins
	REAL(KIND = 8) :: tout
	REAL(KIND = 8) :: pout
	REAL(KIND = 8) :: hout
	REAL(KIND = 8) :: sia
	REAL(KIND = 8) :: fvsi
	REAL(KIND = 8) :: wspd
	REAL(KIND = 8) :: wdir
	
	INTEGER(4) :: l_spectrum_name
	CHARACTER(LEN = 8) :: date_str
	CHARACTER(LEN = 4) :: run_str
	CHARACTER(LEN = 25) :: input_file_name
	
	INTEGER(KIND = 4), PARAMETER :: iou2 = 12
	
	INTRINSIC TRIM, LEN_TRIM, SCAN	
	
	! Read the input file
	OPEN(iou, FILE = 'pre-nonlin5.inp', STATUS = 'OLD', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open pre-nonlin5.inp'
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
					READ(ro, '(A)', ERR = 100, END = 100) nonlin5_path
				CASE (2)
					READ(ro, '(A)', ERR = 100, END = 100) i2s_in_path
				CASE (3)
					READ(ro, '(A)', ERR = 100, END = 100) chans
				CASE (4)
					READ(ro, '(A)', ERR = 100, END = 100) ichan
				CASE (5)
					READ(ro, '(A)', ERR = 100, END = 100) upside_down
				CASE (6)
					READ(ro, '(A)', ERR = 100, END = 100) maxcycle
				CASE (7)
					READ(ro, '(A)', ERR = 100, END = 100) initstep
				CASE (8)
					READ(ro, '(A)', ERR = 100, END = 100) stepadjcoef
				CASE (9)
					READ(ro, '(A)', ERR = 100, END = 100) ntry
				CASE (10)
					READ(ro, '(A)', ERR = 100, END = 100) minstep
				CASE (11)
					READ(ro, *, ERR = 100, END = 100) first_date
				CASE (12)
					READ(ro, *, ERR = 100, END = 100) last_date
				CASE (13)
					READ(ro, *, ERR = 100, END = 100) min_sia
				CASE (14)
					READ(ro, *, ERR = 100, END = 100) max_fvsi
				CASE (15)
					READ(ro, *, ERR = 100, END = 100) maxlinenum
				CASE (16)
					READ(ro, '(A)', ERR = 100, END = 100) jobs
					EXIT	
			END SELECT
			par_no = par_no + 1
		END IF
	END DO

	CLOSE(iou, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close pre-nonlin5.inp'
		STOP
	END IF
	
	l_nonlin5 = LEN_TRIM(nonlin5_path)
	l_i2s_in = LEN_TRIM(i2s_in_path)
	
	! Go through opus-i2s input files
	WRITE(*, '(/, A, /)') 'Searching opus-i2s input files from ' // i2s_in_path(1:l_i2s_in)
	
	! Open the run file
	run_file = 'run_nonlin5.sh'
	OPEN(iou3, FILE = nonlin5_path(1:l_nonlin5) // run_file, &
		STATUS = 'REPLACE', IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to open ' // run_file
		STOP
	END IF
	
	linenum = 0
	filenum = 0
	
	handle = FILE$FIRST
	DO
		! Get one opus-i2s input file 
		l = GETFILEINFOQQ(i2s_in_path(1:l_i2s_in) // 'opus-i2s_*.in', info, handle)
		IF (l == 0) THEN
			EXIT
		END IF
		i2s_in_file = info.name
		
		! Read the opus-i2s input file
		WRITE(*, '(A)') 'Reading file: ' // TRIM(i2s_in_file)
		OPEN(iou, FILE = i2s_in_path(1:l_i2s_in) // TRIM(i2s_in_file), &
			STATUS = 'OLD', IOSTAT = ioerr)
		IF (ioerr /= 0) THEN
			WRITE(*, '(A)') 'Error, failed to open ' // TRIM(i2s_in_file)
			STOP
		END IF
		
		! Extract the OPUS file directory path
		i2s_par_no = 0
		DO
			READ(iou, '(A)', IOSTAT = ioerr) ro
			IF (ioerr /= 0) THEN
				WRITE(*, '(A)') 'Error in reading file ' // TRIM(i2s_in_file)
				STOP
			END IF
			l_ro = LEN_TRIM(ro)
			IF (l_ro == 0) THEN
				CYCLE
			END IF
			IF (ro(1:1) == ':') THEN
				CYCLE
			END IF
			
			i2s_par_no = i2s_par_no + 1
			SELECT CASE (i2s_par_no)
				CASE (1)
					READ(ro, '(A)', IOSTAT = ioerr) ifgfile_path
					IF (ioerr /= 0) THEN
						WRITE(*, '(A)') 'Error in reading file ' // TRIM(i2s_in_file)
						STOP
					END IF
				CASE (29)
					EXIT
			END SELECT
		END DO
		
		! Go through spectra
		DO
			READ(iou, '(A)', IOSTAT = ioerr) ro
			IF (ioerr > 0) THEN  ! Error
				WRITE(*, '(A)') 'Error in reading file ' // TRIM(i2s_in_file)
				STOP
			ELSE IF (ioerr < 0) THEN  ! End of file
				EXIT
			END IF
			
			l_ro = LEN_TRIM(ro)
			IF (l_ro == 0) THEN
				CYCLE
			END IF
			IF (ro(1:1) == ':') THEN
				CYCLE
			END IF
			
			READ(ro, *, IOSTAT = ioerr) spectrum_name, year, mon, day, run, lat, lon, alt, &
				tins, pins, hins, tout, pout, hout, sia, fvsi, wspd, wdir
			IF (ioerr /= 0) THEN
				WRITE(*, '(A)') 'Error in reading file ' // TRIM(i2s_in_file)
				STOP
			END IF
			l_spectrum_name = LEN_TRIM(spectrum_name)
			
			! Skip the spectrum if it is outside the time period
			spec_date = year * 10000 + mon * 100 + day 
			IF ((spec_date < first_date) .OR. (spec_date > last_date)) THEN
				WRITE(*, '(A)') ' ' // spectrum_name(1:l_spectrum_name) // &
					': skipped (DATE)'
				CYCLE
			END IF
			
			! Skip the spectrum if the solar intensity is too low or variation too high
			IF ((sia < min_sia) .OR. (fvsi > max_fvsi)) THEN
				WRITE(*, '(A)') ' ' // spectrum_name(1:l_spectrum_name) // &
					': skipped (SIA/FVSI)'
				CYCLE
			END IF	
			
			! Create the nonlin5 input file for the spectrum
			WRITE(date_str, '(I4.4, 2I2.2)') year, mon, day
			WRITE(run_str, '(I4.4)') run
			input_file_name = 'nonlin5_' // date_str // '_' // run_str // '.inp'
			
			OPEN(iou2, FILE = nonlin5_path(1:l_nonlin5) // 'inputs/' // &
				input_file_name, STATUS = 'REPLACE', IOSTAT = ioerr)
			IF (ioerr /= 0) THEN
				WRITE(*, '(A)') 'Error, failed to open ' // input_file_name
				STOP
			END IF
			
			WRITE(iou2, '(A)', ERR = 101) TRIM(ifgfile_path) // &
				spectrum_name(1:l_spectrum_name)			
			WRITE(iou2, '(A)', ERR = 101) nonlin5_path(1:l_nonlin5) // &
				'spec_orig/spec_orig_' // date_str // '_' // run_str // '.dpt'
			WRITE(iou2, '(A)', ERR = 101) nonlin5_path(1:l_nonlin5) // &
				'spec_opti/spec_opti_' // date_str // '_' // run_str // '.dpt'
			WRITE(iou2, '(A)', ERR = 101) nonlin5_path(1:l_nonlin5) // &
				'params/params_' // date_str // '_' // run_str // '.dpt'
			WRITE(iou2, '(A)', ERR = 101) nonlin5_path(1:l_nonlin5) // &
				'cycles/cycles_' // date_str // '_' // run_str // '.dpt'
			WRITE(iou2, '(A)', ERR = 101) TRIM(chans)
			WRITE(iou2, '(A)', ERR = 101) TRIM(ichan)
			WRITE(iou2, '(A)', ERR = 101) TRIM(upside_down)
			WRITE(iou2, '(F0.1)', ERR = 101) sia
			WRITE(iou2, '(F0.4)', ERR = 101) fvsi
			WRITE(iou2, '(A)', ERR = 101) TRIM(maxcycle)
			WRITE(iou2, '(A)', ERR = 101) TRIM(initstep)
			WRITE(iou2, '(A)', ERR = 101) TRIM(stepadjcoef)
			WRITE(iou2, '(A)', ERR = 101) TRIM(ntry)
			WRITE(iou2, '(A)', ERR = 101) TRIM(minstep)
			
			CLOSE(iou2, IOSTAT = ioerr)
			IF (ioerr /= 0) THEN
				WRITE(*, '(A)') 'Error, failed to close ' // input_file_name
				STOP
			END IF

			WRITE(*, '(A)') ' ' // TRIM(spectrum_name) // ': ' // &
				input_file_name // ' created'
			
			linenum = linenum + 1
			
			! Open a new script file
			IF (linenum == 1) THEN
				filenum = filenum + 1
				IF (filenum > 1) THEN
					CLOSE(iou4, IOSTAT = ioerr)
					IF (ioerr /= 0) THEN
						WRITE(*, '(A)') 'Error, failed to close ' // script_file
						STOP
					END IF
				END IF
				WRITE(filenum_str, '(I6.6)') filenum
				script_file = 'run_nonlin5_' // filenum_str // '.sh'
				OPEN(iou4, FILE = nonlin5_path(1:l_nonlin5) // 'scripts/' // &
					script_file, STATUS = 'REPLACE', IOSTAT = ioerr)
				IF (ioerr /= 0) THEN
					WRITE(*, '(A)') 'Error, failed to open ' // script_file
					STOP
				END IF
				
				! Add the script file to the run file
				WRITE(iou3, '(A)', ERR = 102) 'echo "' // script_file // ' is running ..."'
				WRITE(iou3, '(A)', ERR = 102) 'parallel --delay 1 --jobs ' // &
					TRIM(jobs) // ' < ' // nonlin5_path(1:l_nonlin5) // &
					'scripts/' // script_file
				
			ELSE IF (linenum == maxlinenum) THEN
				linenum = 0
			END IF
			
			! Add the nonlin5 input file to the script file
			WRITE(iou4, '(A)', IOSTAT = ioerr) nonlin5_path(1:l_nonlin5) // &
				'nonlin5 ' // nonlin5_path(1:l_nonlin5) // 'inputs/' // &
				input_file_name // ' > /dev/null'
			IF (ioerr /= 0) THEN
				WRITE(*, '(A)') 'Error, failed to write to ' // script_file
				STOP
			END IF
				
		END DO
		
		CLOSE(iou, IOSTAT = ioerr)
		IF (ioerr /= 0) THEN
			WRITE(*, '(A)') 'Error, failed to close ' // TRIM(i2s_in_file)
			STOP
		END IF
		
	END DO
	
	CLOSE(iou4, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // script_file
		STOP
	END IF
	
	WRITE(iou3, '(A)', ERR = 102) 'echo "DONE!"' 
	
	CLOSE(iou3, IOSTAT = ioerr)
	IF (ioerr /= 0) THEN
		WRITE(*, '(A)') 'Error, failed to close ' // run_file
		STOP
	END IF
	
	! Make the run file executable
	rs = CHMOD(TRIM(nonlin5_path) // run_file, '+x')
	IF (rs /= 0) THEN
		WRITE(*, '(A)') &
			'Error, failed to change access mode of ' // run_file
		STOP
	END IF
	
	STOP
	
	! In case of error
100 WRITE(*, '(A)') 'Error in reading from pre-nonlin5.inp'
	STOP
101 WRITE(*, '(A)') 'Error in writing to ' // input_file_name
	STOP
102 WRITE(*, '(A)') 'Error in writing to ' // run_file
	STOP	
	
END PROGRAM pre_nonlin5
