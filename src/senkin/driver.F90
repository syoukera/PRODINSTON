SUBROUTINE DRIVER(T_CFD, P_CFD, Y_CFD, delta_t_CFD, TOLS_CFD, MAKE_OUTPUT, IWORK, RWORK)
! C
! C*****DOUBLE PRECISION
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
! C*****END DOUBLE PRECISION
! C*****SINGLE PRECISION
! C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
! C*****END SINGLE PRECISION
      PARAMETER ( LENIWK = 1000000, LENRWK = 20000000, LENCWK = 10000, LENSYM = 16)
      DIMENSION IWORK (LENIWK), RWORK (LENRWK), Y_CFD(*), TOLS_CFD(*)
      LOGICAL LEXIST, MAKE_OUTPUT
      CHARACTER CWORK(LENCWK)*(LENSYM)
      DATA LIN/5/, LOUT/11/, LINKCK/25/, LSAVE/7/, LIGN/9/, LREST/10/
! C
! C     LIN    = Unit number for Keyword input
! C     LOUT   = Unit number for text output to terminal
! C     LIGN   = Unit number for text output file
! C     LSAVE  = Unit number for binary output file
! C     LINKCK = Unit number for CHENKIN linking file
! C     LREST  = Unit number for binary restart file
! C     LENIWK = Length of integer work array
! C     LENRWK = Length of real work array
! C     LENCWK = Length of character work array
! C     LENSYM = Length of a character string in character work array
! C     IWORK  = Integer work array
! C     RWORK  = Real work array
! C     CWORK  = Character work array
! C
! C*****vms
! C      SET I/O UNITS AND OPEN FILES. OPERATING SYSTEM IS vms VMS.
! C      OPEN (LINKCK, STATUS='OLD', FORM='UNFORMATTED')
! C      OPEN (LSAVE, STATUS='NEW', FORM='UNFORMATTED')
! C      OPEN (LOUT, STATUS='NEW', FORM='FORMATTED')
! C      OPEN (LIGN, STATUS='NEW', FORM='FORMATTED')
! C      OPEN (LIN, STATUS='OLD', FORM='FORMATTED')
! C      INQUIRE (FILE='restart', EXIST=LEXIST)
! C      IF (LEXIST) OPEN (LREST,STATUS='OLD',FORM='UNFORMATTED')
! C*****END vms
! C
! C*****unix
      OPEN (LINKCK, FORM='UNFORMATTED', FILE='link/cklink')
      IF (MAKE_OUTPUT) THEN
            OPEN (LSAVE, FORM='UNFORMATTED', FILE ='output/save')
            OPEN (LOUT, FORM='FORMATTED', FILE='output/terminalout')
            OPEN (LIGN, FORM='FORMATTED', FILE = 'output/skout')
            OPEN (LIN, FORM='FORMATTED', FILE='input/inp')
            INQUIRE (FILE='output/restart', EXIST=LEXIST)
            IF (LEXIST) THEN 
                  OPEN (LREST, FORM='UNFORMATTED', FILE='output/restart')
            ENDIF
      ENDIF
! C*****END unix
! C
! C     PASS CONTROL TO SENKIN
! C
      CALL SENKIN (LIN, LOUT, LINKCK, LSAVE, LIGN, LREST,      &
                  LENRWK, RWORK, LENIWK, IWORK, LENCWK, CWORK, & 
                  T_CFD, P_CFD, Y_CFD, delta_t_CFD, TOLS_CFD,  &
                  MAKE_OUTPUT)
! C
      RETURN
      END
! C
      SUBROUTINE TEMPT (TIME, TEMP)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      RETURN
      END
! C
      SUBROUTINE VOLT (TIME, VOL, DVDT)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      RETURN
      END

