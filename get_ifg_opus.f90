SUBROUTINE get_ifg_opus(ifgfile, mip, nifgtot, ifg, dati2000, st_ifg)

	IMPLICIT NONE

	! Input variables
	CHARACTER(LEN = 255), INTENT(IN) :: ifgfile
	INTEGER(KIND = 4), INTENT(IN) :: mip
	
	! Output variables
	INTEGER(KIND = 4), INTENT(OUT) :: nifgtot
	REAL(KIND = 4), DIMENSION(mip), INTENT(OUT) :: ifg
	REAL(KIND = 8), INTENT(OUT) :: dati2000
	INTEGER(KIND = 4), INTENT(OUT) :: st_ifg

	! get_igram_run_parameters variables
	!  Input:
	INTEGER(KIND = 4) :: catslice                  ! Slice number from catalog, used in slice file name
	INTEGER(KIND = 4) :: runno                     ! Run number increasing throughout the day
	INTEGER(KIND = 4) :: verbose                   ! Level of verbosity for displayed messages
	INTEGER(KIND = 4), PARAMETER :: mns = 100      ! Maximum number of interferograms per scan set (max NSS)
	INTEGER(KIND = 4), PARAMETER :: msl = mns * 26 ! Maximum number of interferogram slices per scan set
	INTEGER(KIND = 4), PARAMETER :: mch = 2        ! Maximum number of data channels
	INTEGER(KIND = 4), PARAMETER :: mi4 = 40       ! Maximum number of I*4 items in file header
	INTEGER(KIND = 4), PARAMETER :: mr8 = 40       ! Maximum number of R*8 items in file header	
	INTEGER(KIND = 4) :: chan1                     ! Starting channel number to process
	INTEGER(KIND = 4) :: chan2                     ! Ending channel number to process
	! Input/output:
	INTEGER(KIND = 4) :: errnum                    ! Error code (0=ok, <0=fatal, >0=recoverable)
    ! Output:
	INTEGER(KIND = 4) :: nptvec(msl)               ! Number of points, from OPUS header
	INTEGER(KIND = 4) :: bpdata(msl, mch)          ! Byte pointers into the data blocks of slices
	INTEGER(KIND = 4) :: nss                       ! Number of Sample Scans in this set of slices
	INTEGER(KIND = 4) :: tpx                       ! Number of points in one FWD or REV scan
	REAL(KIND = 8) :: timvec(mns)                  ! Time vector contains one entry for each scan
	REAL(KIND = 8) :: timsli(msl)                  ! Time vector contains one entry for each slice
	INTEGER(KIND = 4) :: runsta(mns)               ! Run starting slice number ( > 0) or run error ( < 0)
	INTEGER(KIND = 4) :: runend(mns)               ! Run ending slice number
	INTEGER(KIND = 4) :: i4head(mi4)               ! Vector to hold the I*4 header items
	REAL(KIND = 8) :: r8head(mr8)                  ! Vector to hold the R*8 header items
	CHARACTER(LEN = 128) :: DTCstr                 ! Variable to hold detector description
	CHARACTER(LEN = 128) :: INSstr                 ! Variable to hold instrument description
	
	! get_igram_data variables
	! Input
	INTEGER(KIND = 4) :: ichan                     ! Channel number (1=InGaAs=slave, 2=Si=master)
	
	! Other variables
	INTEGER(KIND = 4) :: iscan                     ! Scan number within a set of NSS scans
	INTEGER(KIND = 4) :: i
	INTEGER(KIND = 4) :: l_ifgfile

	INTRINSIC SCAN, LEN_TRIM
	
	catslice = 0
	runno = 1
	verbose = 0
	chan1 = 1
	chan2 = 2
	ichan = 1
	errnum = 0
	iscan = 1
	
	st_ifg = 0
	
	l_ifgfile = LEN_TRIM(ifgfile)
	i = SCAN(ifgfile, '/', BACK = .TRUE.)

	IF ((i == 0) .OR. (i == l_ifgfile)) THEN
		st_ifg = 1
		STOP
	END IF
	
	CALL get_igram_run_parameters(ifgfile(1:i), ifgfile((i+1):l_ifgfile), &
		catslice, runno, verbose, mns, msl, mip, mch, mi4, mr8, &
		chan1, chan2, errnum, nptvec, bpdata, nss, tpx, timvec, &
		timsli, runsta, runend, i4head, r8head, DTCstr, INSstr)

	IF (errnum /= 0) THEN
		st_ifg = 2
		STOP
	END IF
	
	dati2000 = timvec(iscan)
	
	CALL get_igram_data(ifgfile(1:i), ifgfile((i+1):l_ifgfile), catslice, &
		runsta(iscan), runend(iscan), verbose, &
		msl, mip, mch, nptvec, bpdata, ichan, nifgtot, ifg)	
	
END SUBROUTINE get_ifg_opus