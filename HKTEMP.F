C1234&
C
C     Temperature Calculation Subroutine 
C           Based on CHEMKIN Package
C                  CODED BY Hiroshi KAWANABE
C                           2006/07/03 Ver 1.0
C
      SUBROUTINE HKTEMP (HMBS, Y, ICKWRK, RCKWRK, T, NMAX, TRES)
C
C
C
C
C     SUBROUTINE HKTEMP (HBMS, Y, ICKWRK, RCKWRK, T)
C     Returns the temperature of the mixture;
C
C  INPUT
C     HBMS   - Mean enthalpy in mass units.
C                   cgs units - ergs/gm
C                   Data type - real scalar
C     Y      - Mass fractions of the species.
C                   cgs units - none
C                   Data type - real array
C                   Dimension Y(*) at least KK, the total number of
C                   species.
C     ICKWRK - Array of integer workspace.
C                   Data type - integer array
C                   Dimension ICKWRK(*) at least LENIWK.
C     RCKWRK - Array of real work space.
C                   Data type - real array
C                   Dimension RCKWRK(*) at least LENRWK.
C
C  OUTPUT
C     T      - Temperature.
C                   cgs units - K
C                   Data type - real scalar
C
C  END PROLOGUE
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      DIMENSION ICKWRK(60000), RCKWRK(60000), Y(160)
C
C     Calculated HBMS from Temporary Temperature T
C
      DO IL=1,NMAX
       CALL CKHBMS (T, Y, ICKWRK, RCKWRK, HTEMP)
       CALL CKCPBS (T, Y, ICKWRK, RCKWRK, CPBMS)
C
       TNEW=T-(HTEMP-HMBS)/CPBMS
C
       TDELT=DABS((T-TNEW)/TNEW)
       IF (TDELT.LE.TRES) GOTO 100
	   T=TNEW
      END DO
C
c      WRITE (6,*) 'Loop Number exceeded in HKTEMP'
c	  WRITE (6,*) 'T and TNEW', T, TNEW
C
  100 CONTINUE
      RETURN
	  END