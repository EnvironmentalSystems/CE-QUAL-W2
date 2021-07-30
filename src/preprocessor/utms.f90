      MODULE UTMS_DATA
      CHARACTER*1  :: YN,DATNUM,DATUM
      REAL*8       :: RAD,ER,RF,ESQ,PI, F,L, LS,F4,F2,ETS,EN,COSFI,C5,C3,C1,A7,A6,A5,A4,A3,A2,A1, OMO
      LOGICAL      :: FILFLAG,FILPRT,FIL81FL
      CHARACTER*1  :: EORW
      CHARACTER*30 :: NAME,GPPFIL,GPFIL,PCFIL,FIL81
      CHARACTER*80 :: CARDR
      CHARACTER*1  :: NORS
      CHARACTER*11 :: CLAT
      CHARACTER*12 :: CLON
      CHARACTER*8  :: TITLE,GNUM
      CHARACTER*4  :: ZONE
      INTEGER*4    :: LD,LM,LOD,LOM,FOUND,J,IZ,ISEC,IMIN,IDEG,ICM,I,JSEC
       REAL*8      :: SLAT,SLON,FI,LAM,LCM,UCM,LOD1,CONV,SF,OR,EPS,R,A,B,C,U,V,W,SO,CM,FE,FN
       REAL*8      :: KP,GRIDAZ,TD,TM,ND,NM,NORTH,EAST,TOL,GEOD,CSEC, TN,TS,SINFI,S,RN,PR,OM
           
      CHARACTER*5  :: XGD,GDVAL(1001)

      CHARACTER*1 PM,ESGN,GSGN
      CHARACTER*1 ELEVT
      CHARACTER*2 ORDER
      CHARACTER*2 ELXX
      CHARACTER*6 GD
      INTEGER*4	STANUM, ISN, IPRT, I3, I4, I2, ICON,GDNUM(1001)
      CHARACTER*4 ELEV
      CHARACTER*11 REST
      CHARACTER*7 ELNUM

      CHARACTER*11 ANORTH
      CHARACTER*10 AEAST
      CHARACTER*1  BUFF(20)
      
      REAL*8 SEC,RHOSEC
      DATA RHOSEC/2.062648062471D05/
      DATA DATNUM /'2'/    ! 1=NAD27, 2=NAD83, 3=INT24, 4=WGS72
      
      END MODULE UTMS_DATA
      SUBROUTINE UTMS
!     THIS PROGRAM CONVERTS GPS	TO UNIVERSIAL TRANSVERSE MERACTOR COORDINATES
!     AND VICE VERSA FOR THE NAD27 AND NAD83 DATUM.
!     THIS PROGRAM WAS WRITTEN BY E. CARLSON
!     SUBROUTINES TMGRID, TCONST, TMGEOD, TCONPC,
!     WERE WRITTEN BY T. VINCENTY, NGS,	IN JULY	1984 .
!     THE ORGINAL PROGRAM WAS WRITTEN IN SEPTEMBER OF 1988.
!
!     THIS PROGRAM WAS UPDATED ON FEBUARY 16, 1989.  THE UPDATE	WAS
!     HAVING THE OPTION	OF SAVING AND *81* RECORD FILE.
!
!
!     THIS PROGRAM WAS UPDATED ON APRIL	3, 1990.  THE FOLLOWING	UPDATE
!     WERE MADE:
!      1. CHANGE FROM JUST A CHOICE OF NAD27 OF	NAD83 REFERENCE
!	  ELLIPSOIDS TO; CLARKE	1866, GRS80/WGS84, INTERNATIONAL, AND
!	  ALLOW	FOR USER DEFINED OTHER.
!      2. ALLOW	USE OF LATITUDES IN SOUTHERN HEMISPHERE	AND LONGITUDES
!	  UP TO	360 DEGREES WEST.
!
!     THIS PROGRAM WAS UPDATED ON DECEMBER 1, 1993. THE	FOLLOWING UPDATE
!     WAS MADE:
!      1. THE NORTHINGS	WOULD COMPUTE THE RIGHT	LATITUDE IN THE	SOUTHERN
!	  HEMISHPERE.
!      2. THE COMPUTED LATITUDE	ON LONGIDUTES WOULD BE EITHER  IN E OR W.
!
!****************************************************************     *
!		   DISCLAIMER					      *
!								      *
!   THIS PROGRAM AND SUPPORTING	INFORMATION IS FURNISHED BY THE	      *
! GOVERNMENT OF	THE UNITED STATES OF AMERICA, AND IS ACCEPTED AND     *
! USED BY THE RECIPIENT	WITH THE UNDERSTANDING THAT THE	UNITED STATES *
! GOVERNMENT MAKES NO WARRANTIES, EXPRESS OR IMPLIED, CONCERNING THE  *
! ACCURACY, COMPLETENESS, RELIABILITY, OR SUITABILITY OF THIS	      *
! PROGRAM, OF ITS CONSTITUENT PARTS, OR	OF ANY SUPPORTING DATA.	      *
!								      *
!   THE	GOVERNMENT OF THE UNITED STATES	OF AMERICA SHALL BE UNDER NO  *
! LIABILITY WHATSOEVER RESULTING FROM ANY USE OF THIS PROGRAM.	THIS  *
! PROGRAM SHOULD NOT BE	RELIED UPON AS THE SOLE	BASIS FOR SOLVING A   *
! PROBLEM WHOSE	INCORRECT SOLUTION COULD RESULT	IN INJURY TO PERSON   *
! OR PROPERTY.							      *
!								      *
!   THIS PROGRAM IS PROPERTY OF	THE GOVERNMENT OF THE UNITED STATES   *
! OF AMERICA.  THEREFORE, THE RECIPIENT	FURTHER	AGREES NOT TO ASSERT  *
! PROPRIETARY RIGHTS THEREIN AND NOT TO	REPRESENT THIS PROGRAM TO     *
! ANYONE AS BEING OTHER	THAN A GOVERNMENT PROGRAM.		      *
!								      *
!**********************************************************************
      USE UTMS_DATA

      IMPLICIT NONE
      PI=4.D0*DATAN(1.D0)
      RAD=180.D0/PI

	  CALL DATUMM
 	  CALL IUTGP    ! INTERACTIVE

      RETURN
      END
!*****************************************************
      SUBROUTINE HDUTGP
!****************************************************
    USE UTMS_DATA
    IMPLICIT NONE
    
	IF(IPRT.EQ.1) THEN
	   WRITE(3,5) GNUM
    5	   FORMAT('1',/,T50,'PRELIMINARY COORDINATE LISTING ',/	,T59,'FOR ',A8,//)
	ELSEIF(IPRT.EQ.2) THEN
	   WRITE(3,6) GNUM
    6	   FORMAT('1',/,T54,'FINAL COORDINATE LISTING ',/,T60,'FOR ',A8,//)
	ELSE
	   WRITE(3,7)
    7	   FORMAT('1',//////)
	ENDIF
	 IF(DATNUM.EQ.'1') THEN
      WRITE(3,10)
   10 FORMAT(T54,'NATIONAL GEODETIC SURVEY',/,  &
     	     T58,'GP TO	UTMs PROGRAM',T118,'VERSION 2.1',/,T55,  &
     	     'CLARKE 1866 ELLIPSOID',//,  &
     	      1X,' STATION NAME',T34,'LATITUDE',  &
     	     T50,'LONGITUDE',T68,'NORTHING(Y)',T80,'EASTING(X)',  &
     	     T92,'ZONE',T97,'CONVERGENCE',T110,'SCALE',T121,'ELEV',  &
     	     T129,'GEOID',/,T68,'METER',  &
     	     T80,'METER',T97,'D',T100,'M',T104,'S',  &
     	     T110,'FACTOR',T121,'(M)',T129,'HT(M)',//)
	 ENDIF
	 IF(DATNUM.EQ.'2') THEN
      WRITE(3,12)
   12 FORMAT(T54,'NATIONAL GEODETIC SURVEY',/,  &
     	     T58,'GP TO	UTMs PROGRAM',T118,'VERSION 2.1',/,T56,  &
     	     'GRS80/WGS84 ELLIPSOID',//,  &
     	      1X,' STATION NAME',T34,'LATITUDE',  &
     	     T50,'LONGITUDE',T68,'NORTHING(Y)',T80,'EASTING(X)',  &
     	     T92,'ZONE',T97,'CONVERGENCE',T110,'SCALE',T121,'ELEV',  &
     	     T129,'GEOID',/,T68,'METER',  &
     	     T80,'METER',T97,'D',T100,'M',T104,'S',  &
     	     T110,'FACTOR',T121,'(M)',T129,'HT(M)',//)
	 ENDIF
	 IF(DATNUM.EQ.'3') THEN
      WRITE(3,13)
   13 FORMAT(T54,'NATIONAL GEODETIC SURVEY',/,  &
     	     T58,'GP TO	UTMs PROGRAM',T118,'VERSION 2.1',/,T53,  &
     	     'INTERNATIONAL 1910 ELLIPSOID',//,  &
     	      1X,' STATION NAME',T34,'LATITUDE',  &
     	     T50,'LONGITUDE',T68,'NORTHING(Y)',T80,'EASTING(X)',  &
     	     T92,'ZONE',T97,'CONVERGENCE',T110,'SCALE',T121,'ELEV',  &
     	     T129,'GEOID',/,T68,'METER',  &
     	     T80,'METER',T97,'D',T100,'M',T104,'S',  &
     	     T110,'FACTOR',T121,'(M)',T129,'HT(M)',//)
	 ENDIF
	 IF(DATNUM.EQ.'4') THEN
      WRITE(3,14)
   14 FORMAT(T54,'NATIONAL GEODETIC SURVEY',/,  &
     	     T58,'GP TO	UTMs PROGRAM',T118,'VERSION 2.1',/,T59,  &
     	     'WGS72 ELLIPSOID',//,  &
     	      1X,' STATION NAME',T34,'LATITUDE',  &
     	     T50,'LONGITUDE',T68,'NORTHING(Y)',T80,'EASTING(X)',  &
     	     T92,'ZONE',T97,'CONVERGENCE',T110,'SCALE',T121,'ELEV',  &
     	     T129,'GEOID',/,T68,'METER',  &
     	     T80,'METER',T97,'D',T100,'M',T104,'S',  &
     	     T110,'FACTOR',T121,'(M)',T129,'HT(M)',//)
	 ENDIF
	 IF(DATNUM.EQ.'5') THEN
      WRITE(3,15)
   15 FORMAT(T54,'NATIONAL GEODETIC SURVEY',/,  &
     	     T58,'GP TO	UTMs PROGRAM',T118,'VERSION 2.1',/,T59,  &
     	     'OTHER ELLIPSOID',//,       &
     	      1X,' STATION NAME',T34,'LATITUDE', &
     	     T50,'LONGITUDE',T68,'NORTHING(Y)',T80,'EASTING(X)', &
     	     T92,'ZONE',T97,'CONVERGENCE',T110,'SCALE',T121,'ELEV', &
     	     T129,'GEOID',/,T68,'METER', &
     	     T80,'METER',T97,'D',T100,'M',T104,'S', &
     	     T110,'FACTOR',T121,'(M)',T129,'HT(M)',//)
	 ENDIF
      RETURN
      END
!******************************************************************
       SUBROUTINE IUTGP
!******************************************************************
      USE UTMS_DATA
      USE GRID
      IMPLICIT NONE

      FILFLAG=.FALSE.
      FIL81FL=.FALSE.

	OPEN(3,STATUS='UNKNOWN',FILE='test_out_utm.txt',ERR=900)
	FILFLAG=.TRUE.
    IPRT=1
	CALL HDUTGP
    GO TO 901
900   WRITE(*,*)'Test_out_utm.txt file creation error'
901   CONTINUE
      DO J=1,NWB
	  NAME='          '
      
      LD=INT(LAT(J))
      LM=INT((LAT(J)-FLOAT(LD))*60.)
      SLAT=(((LAT(J)-FLOAT(LD))*60.)-FLOAT(LM))*60.
      NORS='N'               ! CHARACTER*1 N OR S
      LOD=INT(LONG(J))
      LOM=INT((LONG(J)-FLOAT(LOD))*60.)
      SLON=(((LONG(J)-FLOAT(LOD))*60.)-FLOAT(LOM))*60.
      EORW='W'               ! CHARACTER*1 DIRECTION OF LONGITUDE - E OR	W 

      !LD=45                 ! INTEGER LATITUDE:DEGREES DD 
      !LM=10               ! INTEGER MINUTES MM
      !SLAT=25.5               ! REAL SECONDS SS.SSSSS
      !NORS='N'               ! CHARACTER*1 N OR S
      !LOD=118                ! INTEGER LONGITUDE: DEGREES DDD
      !LOM=10                ! INTEGER MINUTES MM
      !SLON=45.5               ! REAL SECONDS SS.SSSSS
      !EORW='E'               ! CHARACTER*1 DIRECTION OF LONGITUDE - E OR	W 

      ISEC = SLAT * 1.0D5 + 0.5D0
      JSEC = SLON * 1.0D5 + 0.5D0

      CALL DRGPUT
      UTMZONE(J)=IZ
      YNORTHING(J)=NORTH
      XEASTING(J)=EAST
      
      
      
      END DO
      
      RETURN
      END
!***********************************************************************
       SUBROUTINE DRGPUT
!*********************************************************************
!
!
!      THIS IS THE DRIVER TO COMPUTE UTM NORTHINGS AND EASTINGS
!      FOR EACH	PRIMARY	ZONE AND THE AJACENT ZONE IF THE LONGITUDE
!      IS WITH 5 MINUTES OF THE	ZONE BOUNDARIES
!
!      THE OUTPUT IS FOR THE DATA SHEET	PROGRAM
!
!      VARIABLES
!      CARDR = A MODIFIED 80 RECORD CARD WITH A	LENGTH OF 211 COLS
!      ER = EQUATORIAL RADIUS OF THE ELLIPSOID (SEMI-MAJOR AXIS)
!      RF = RECIPROCAL OF FLATTING OF THE ELLIPSOD
!      ESQ= E SQUARED
!      RAD = RADIAN CONVERSION FACTOR
!      CM = CENTRAL MERIDIAN ( COMPUTED	USEING THE LONGITUDE)
!      SF = SCALE FACTOR OF CENTRAL MERIDIAN ( ALWAYS .9996 FOR	UTM)
!      OR = SOUTHERNMOST PARALLEL OF LATITUDE (	ALWAYS ZERO FOR	UTM)
!      R, A, B,	C, U, V, W = ELLIPSOID CONSTANTS USED FOR COMPUTING
!			     MERIDIONAL	DISTANCE FROM LATITUDE
!      SO = MERIDIONAL DISTANCE	(MULTIPLIED BY SCALE FACTOR )
!	    FROM THE EQUATOR TO	THE SOUTHERNMOST PARALLEL OF LATITUDE
!	    ( ALWAYS ZERO FOR UTM)
!
       USE UTMS_DATA
       IMPLICIT NONE

!      CONVERT THE LATITUDE AND	LONGITUDE TO PI	AND LAM
       TD=DBLE(FLOAT(LD))
       TM=DBLE(FLOAT(LM))
       FI=(TD+(TM+SLAT/60.D0)/60.D0)/RAD
       ND=DBLE(FLOAT(LOD))
       NM=DBLE(FLOAT(LOM))
       IF((EORW.EQ.'E').OR.(EORW.EQ.'e')) THEN
	 LAM=(360.D0-(ND+(NM+SLON/60.D0)/60.D0))/RAD
	 LOD1=(360.D0-(ND+(NM+SLON/60.D0)/60.D0))
	 LOD=DINT(LOD1)
       ENDIF
       IF((EORW.EQ.'W').OR.(EORW.EQ.'w')) THEN
	 LAM=(ND+(NM+SLON/60.D0)/60.D0)/RAD
	 LOD=LOD
       ENDIF
!      FIND THE	ZONE FOR LONGITUDE LESS	THAN 180 DEGREES
	IF(LOD.LT.180) THEN
	   IZ=LOD/6
	   IZ= 30 -IZ
	   ICM=(183-(6*IZ))
	   CM=DBLE(FLOAT(ICM))/RAD
	   UCM=(ICM+3)/RAD
	   LCM=(ICM-3)/RAD
	ENDIF
!	FIND THE ZONE FOR LONGITUDE GREATER THAN 180 DEGREES
	 IF(LOD.GE.180)	THEN
	    IZ=(LOD)/6
	    IZ=	90 - IZ
	    ICM=(543 - (6*IZ))
	    CM=	DBLE(FLOAT(ICM))/RAD
	    UCM=(ICM+3)/RAD
	    LCM=(ICM-3)/RAD
	 ENDIF
       TOL=(5.0D0/60.0D0)/RAD
       FN = 0.D0
       IF((NORS.EQ.'S').OR.(NORS.EQ.'s')) THEN
	  FN = 10000000.D0
       ENDIF
       IF((NORS.EQ.'N').OR.(NORS.EQ.'n')) THEN
	  FN = 0.D0
       ENDIF
       FE=500000.0D0
       SF=0.9996D0
       OR=0.0D0
       FOUND=0
       CALL TCONST
!      COMPUTE THE NORTH AND EASTINGS
 200  CALL TMGRID
!      WRITE THE ZONE NUMBER
       IF (IZ.GT.9) THEN
	 WRITE(ZONE,600) IZ
 600	 FORMAT(1X,I2)
       ELSE
	 WRITE(ZONE,605) IZ
 605	 FORMAT(1X,I2.2)
       ENDIF
!      WRITE THE OUTPUT	TO THE PLANE FILE FOR THE DATA SHEET
!      PROGRAM
      CALL DATAUT
!      DO THE TEST TO SEE IF THE LONGITUDE IS WITHIN 5 MINUTES
!      OF THE BOUNDARIES FOR THE ZONE AND IF SO	COMPUTE	THE
!      NORHT AND EASTING FOR THE ADJACENT ZONE
       IF(FOUND.NE.0) THEN
	  RETURN
       ENDIF
       IF(DABS(UCM-LAM).LE.TOL)	THEN
	  CM=DBLE(FLOAT(ICM+6))/RAD
	  IZ=IZ-1
	  IF(IZ.EQ.0) IZ=60
	  FOUND=FOUND+1
	  GO TO	200
       ENDIF
       IF(DABS(LCM-LAM).LE.TOL)	THEN
	  CM=DBLE(FLOAT(ICM-6))/RAD
	  IZ=IZ+1
	  IF(IZ.EQ.61) IZ=1
	  FOUND=FOUND+1
	  GO TO	200
       ENDIF
       RETURN
       END
!********************************************************************
      SUBROUTINE DATAUT
!*******************************************************************
      USE UTMS_DATA
      IMPLICIT NONE

      IF (CONV.LT.0) THEN
	PM='-'
      ELSE
	PM=' '
      ENDIF
      CALL TODMS(DABS(CONV),IDEG,IMIN,CSEC)
       IF(FILFLAG) THEN
		  IF(ICON.GE.48) THEN
              IPRT=2
		     CALL HDUTGP
		     ICON = 0
		  ENDIF
	  IF((ORDER(1:1).EQ.'4').AND.(ELEVT.EQ.' ')) THEN
	       ELNUM='	     '
	  ELSEIF((ORDER(1:1).EQ.' ').AND.(ELEVT.EQ.' ')) THEN
	       ELNUM='	     '
	  ELSEIF((ELEVT.EQ.'B').OR.(ELEVT.EQ.'L')) THEN
	       WRITE(ELNUM,200)	ELEV,ELXX
  200	       FORMAT(A4,'.',A2)
	  ELSEIF((ELEVT.EQ.'R').OR.(ELEVT.EQ.'T')) THEN
	       WRITE(ELNUM,205)	ELEV,ELXX(1:1)
  205	       FORMAT(A4,'.',A1,' ')
	  ELSEIF((ELEVT.EQ.'P').OR.(ELEVT.EQ.'E').OR.(ELEVT.EQ.'V')) THEN
	       WRITE(ELNUM,210)	ELEV
  210	       FORMAT(A4,'.  ')
	  ELSE
	       WRITE(ELNUM,215)	ELEV
  215	       FORMAT(A4,' SC')
	  ENDIF
!** DO THE DO LOOP TO FIND THE GEOD HT
	    GD='      '
	    XGD='     '
	 DO 60 I=1,ISN
	    IF(GDNUM(I).EQ.STANUM) THEN
	       XGD=GDVAL(I)
	    ENDIF
   60	  CONTINUE
	     IF(XGD.NE.'     ')	THEN
		READ(XGD,FMT='(F5.1)') GEOD
		 GSGN='	'
	      IF(GEOD.LE.0.0D0)	THEN
		 GSGN='-'
	      ENDIF
		  GEOD=DABS(GEOD)
		  WRITE(GD,FMT='(F6.2)') GEOD
		  GD(1:1)=GSGN
	     ENDIF
	IF(J.EQ.0) THEN
	  WRITE(3,10) NAME,LD,LM,SLAT,NORS,LOD,LOM,SLON,EORW,  &
     		   NORTH,EAST,ZONE,PM,IDEG,IMIN,CSEC,KP,ELNUM,GD
 10	  FORMAT(1X,A30,T34,I2,1X,I2,1X,F8.5,A1,T50,I3.3,1X,I2.2,1X, &
     	   F8.5,A1,T67,F12.3,T80,F11.3,T92,A4,T97,A1,I1,1X,I2,1X,F5.2, &
     	   T109,F10.8,T120,A7,T128,A6)
		ICON= ICON + 1
	ELSE
	  WRITE(3,20) NORTH,EAST,ZONE,PM,IDEG,IMIN,CSEC,KP
  20	  FORMAT(T67,F12.3,T80,F11.3,T92,A4,T97,A1,I1,1X,I2,1X, &
     		 F5.2,T109,F10.8)
	       ICON = ICON + 1
	ENDIF
      ELSE
	 IF(DATNUM.EQ.'1') THEN
	   PRINT *, ' UTMS FOR THE CLARK 1866 ELLIPSOID	(NAD27 DATUM) '
	   PRINT *,'		 '
	 ENDIF
	 IF(DATNUM.EQ.'2') THEN
	    PRINT *,'  UTMS FOR	THE GRS80/WGS84	ELLIP (NAD83 DATUM) '
	    PRINT *,'				'
	 ENDIF
	 IF(DATNUM.EQ.'3') THEN
	    PRINT *,'  UTMS FOR	THE INTERNATIONAL ELLIP	(INT24 DATUM) '
	    PRINT *,'				'
	 ENDIF
	 IF(DATNUM.EQ.'4') THEN
	    PRINT *,'  UTMS FOR	THE WGS72 ELLIPSOID '
	    PRINT *,'				'
	 ENDIF
	 IF(DATNUM.EQ.'5') THEN
	    PRINT *,'  UTMS FOR	THE OTHER ELLIPSOID '
	    PRINT *,'				'
	 ENDIF
	WRITE(*,30)
   30	FORMAT('   NORTH(Y)	 EAST(X)   ZONE	  ',  &
     	       ' CONVERGENCE  SCALE')
	WRITE(*,40)NORTH,EAST,ZONE,PM,IDEG,IMIN,CSEC,KP
   40	FORMAT(1X,F12.3,1X,F11.3,2X,A4,2X,A1,I1,1X,I2,1X,  &
     	       F5.2,3X,F10.8)
      ENDIF
      IF(FIL81FL) THEN
	CALL CHGDEC(11,3,EAST,BUFF)
	  AEAST(1:1)  =	BUFF(1)
	  AEAST(2:2)  =	BUFF(2)
	  AEAST(3:3)  =	BUFF(3)
	  AEAST(4:4)  =	BUFF(4)
	  AEAST(5:5)  =	BUFF(5)
	  AEAST(6:6)  =	BUFF(6)
	  AEAST(7:7)  =	BUFF(7)
	  AEAST(8:8)  =	BUFF(9)
	  AEAST(9:9)  =	BUFF(10)
	  AEAST(10:10) = BUFF(11)
	CALL CHGDEC(12,3,NORTH,BUFF)
	  ANORTH(1:1)  = BUFF(1)
	  ANORTH(2:2)  = BUFF(2)
	  ANORTH(3:3)  = BUFF(3)
	  ANORTH(4:4)  = BUFF(4)
	  ANORTH(5:5)  = BUFF(5)
	  ANORTH(6:6)  = BUFF(6)
	  ANORTH(7:7)  = BUFF(7)
	  ANORTH(8:8)  = BUFF(8)
	  ANORTH(9:9)  = BUFF(10)
	  ANORTH(10:10)	= BUFF(11)
	  ANORTH(11:11)	= BUFF(12)
       READ(CARDR,300) STANUM,NAME,REST
  300  FORMAT(T11,I3,T15,A30,T70,A11)
	IF(DATNUM.EQ.'1') THEN
	WRITE(4,310) STANUM,NAME,AEAST,ANORTH,ZONE(2:3),REST
  310	FORMAT(T7,'*81*',T11,I3,T15,A30,T45,A10,T55,A11,   &
     	       T66,'00',A2,T70,A11)
	ENDIF
	IF(DATNUM.EQ.'2') THEN
	WRITE(4,320) STANUM,NAME,ANORTH,AEAST,ZONE(2:3),REST
  320	FORMAT(T7,'*81*',T11,I3,T15,A30,T45,A11,T56,A10,   &
     	       T66,'00',A2,T70,A11)
	ENDIF
	IF(DATNUM.EQ.'3') THEN
	WRITE(4,310) STANUM,NAME,AEAST,ANORTH,ZONE(2:3),REST
	ENDIF
       IF(DATNUM.EQ.'4') THEN
	WRITE(4,310) STANUM,NAME,AEAST,ANORTH,ZONE(2:3),REST
	ENDIF
       IF(DATNUM.EQ.'4') THEN
	WRITE(4,310) STANUM,NAME,AEAST,ANORTH,ZONE(2:3),REST
	ENDIF
       IF(DATNUM.EQ.'5') THEN
	WRITE(4,310) STANUM,NAME,AEAST,ANORTH,ZONE(2:3),REST
	ENDIF
      ENDIF
      RETURN
      END
!*********************************************************************
      SUBROUTINE TODMS(RAD,IDG,MIN,SEC)
!*********************************************************************
!     RADIANS TO DEGREES,MINUTES AND SECONDS
      USE UTMS_DATA, ONLY: RHOSEC
      IMPLICIT NONE 
      REAL*8    :: RAD,SEC
      INTEGER*4 :: IDG,MIN
 
      SEC=RAD*RHOSEC
      IDG=SEC/3600.D0
      SEC=SEC-DBLE(IDG*3600)
      MIN=SEC/60.D0
      SEC=SEC-DBLE(MIN*60)
      IF((60.D0-DABS(SEC)).GT.5.D-6) GO	TO 100
      SEC=SEC-DSIGN(60.D0,SEC)
      MIN=MIN+ISIGN(1,MIN)
  100 IF(IABS(MIN).LT.60) GO TO	101
      MIN=MIN-ISIGN(60,MIN)
      IDG=IDG+ISIGN(1,IDG)
  101 MIN=IABS(MIN)
      SEC=DABS(SEC)
      IF(RAD.GE.0.D0) GO TO 102
      IF(IDG.EQ.0) MIN=-MIN
      IF(IDG.EQ.0.AND.MIN.EQ.0)SEC=-SEC
  102 RETURN
      END
!*********************************************************************
      SUBROUTINE CHGDEC (NNN,MMM,SS,CHAR)
!     -----------------------------------------------------
      CHARACTER*1  DASH,ZERO,DOL,BLK1,CHAR(*),IB(20),TT
      CHARACTER*20 JB
      INTEGER*4   IDG,MIN
      REAL*8      S,SS,DEC,W,SEC,TEN,TOL
!
      EQUIVALENCE (IB,JB)
!
      DATA BLK1,DOL,ZERO,DASH/' ','$','0','-'/,TEN/10.0D0/
!
!        CHAR      1-16      LENGTH OF CHARACTER ARRAY FIELD
!        NR        1-13      LENGTH OF FIELD TO BE USED FROM LEFT
!        NP        7-13      LOCATION OF DECIMAL POINT FROM RIGHT
!        NEG       CHAR(1)   PUT THE MINUS SIGN HERE
!        EXAMPLE:            RANGE NR=14 AND WITH A POINT NP=6
!                            W=  -3600.376541
!
! CHAR FIELD     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
! BLANK FILLED   B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B BLANK
! NR ADDED      |B  B  B  B  B  B  B  B  B  B  B  B  B  B| B  B LIMIT
! NP ADDED      |B  B  B  B  B  B  B  .  B  B  B  B  B  B| B  B POINT
! W ENTERED     |B  B  -  3  6  0  0  .  3  7  6  5  4  1| 8  4
! DOL (SIGN)    |B  B  -  3  6  0  0  .  3  7  6  5  4  1| $  4
! NEG (SIGN)    |B  B  -  3  6  0  0  .  3  7  6  5  4  1| $  4
!
!  -------------------------------
!     SETUP OUTSIDE CONSTANTS

      NR=NNN
      NP=MMM
      DEC=SS
!
!     CHECK TO SEE IF NR AND NF ARE WITHIN LIMITS
!
      NNP=IABS(NP)
      KTST=0
      M=IABS(NR)
!
!     IF NR IS GREATER THAN ZERO -- DECIMAL NUMBER
!
      IF(NR.GT.0)GOTO 1
! -------------------------------------
!     THIS IS A DEG-MIN-SEC FORMAT
!
      IF(NNP.GT.5)NNP=5
!
!     ENTRY IS DDD-MM-SS.SS
!           OR  HH-MM-SS.SS
!
      DEC=DABS(DEC)
      SEC=DEC*3600.0D0
      IDG=SEC/3600.0D0
      SEC=SEC-DBLE(FLOAT(IDG))*3600.0D0
      MIN=SEC/60.0D0
      SEC=SEC-DBLE(FLOAT(MIN))*60.0D0
      DEC=DBLE(FLOAT(IDG))*1000000.0D0+DBLE(FLOAT(MIN))*1000.0D0+SEC

      KTST=1
      M=15

!     ROUND THE DECIMAL NUMBER

    1 IP=-1*(NNP+1)
      TOL=5.0D0*(TEN**IP)
      W=(DABS(DEC) + TOL)
      IF(DEC.LT.0.0D0) W=-W
      DEC=W

      N=NNP
      IF(M.GT.15)M=15
      IF(M.LT.4)M=4
      IF(N.GE.M)N=M-1
      W=DEC

!     CONVERT THE DECIMAL NUMBER

      WRITE(JB,100) W
  100 FORMAT(F20.10)

!     BLANK FILL THE ARRAY

      DO 5 IQ=1,16
    5 CHAR(IQ)=BLK1

!     LOOK FOR THE FIRST NON-BLANK CHARACTER

      DO 6 I=1,10
      TT=IB(I)
    6 IF(TT.NE.BLK1)GOTO 7

!     COMPUTE THE PROPER NUMBER LENGTH WITH THE PROPER DECIMALS

    7 K=11-I
      K=K+N
      IF(K.GT.M)M=K
      L=10-(M-N)
      IF(L.LE.0)L=0

      MM=M+1
      J=0
      IDEC=0
      DO 30 I=1,MM
      K=I+L
      IF(K.GT.20)K=20
      J=J+1
      CHAR(J)=IB(K)
   30 IF(CHAR(J).EQ.'.') IDEC=J
      N=IDEC+NNP+1

      IF(KTST.EQ.0)GOTO 40

!     FILL-OUT THE DEG-MIN-SEC FIELD

      CHAR(4)=DASH
      CHAR(7)=DASH

!     ZERO-OUT ALL BLANK CHARACTERS

      DO 10 I=1,9
   10 IF(CHAR(I).EQ.BLK1)CHAR(I)=ZERO

   40 CHAR(N)=DOL

      RETURN
      END
!***********************************************************
      SUBROUTINE DATUMM
      USE UTMS_DATA
      IMPLICIT NONE

!     FIND THE	RIGHT SEMI MAJOR AXIS AND FLATTING

	 IF(DATNUM.EQ.'1')	THEN
!	FOR THE	NAD 27 DATUM

		ER=6378206.4D0
		RF=294.978698D0
		F=1.D0/RF
		ESQ=(F+F-F*F)
	ELSEIF (DATNUM.EQ.'2') THEN
!     FOR THE NAD83 DATUM

		ER=6378137.D0
		RF=298.257222101D0
		F=1.D0/RF
		ESQ=(F+F-F*F)
	ELSEIF (DATNUM.EQ.'3') THEN
!     FOR THE INT24 DATUM

		ER=6378388.D0
		RF=297.0D0
		F=1.D0/RF
		ESQ=(F+F-F*F)
	ELSEIF (DATNUM.EQ.'4') THEN
!     FOR THE WGS72

		ER=6378135.D0
		RF=298.26D0
		F=1.D0/RF
		ESQ=(F+F-F*F)
    ELSE
        WRITE(*,*)'ERROR: WRONG DATNUM SPECIFIED'
        STOP
     ENDIF
	 RETURN
END
      SUBROUTINE TMGRID
      USE UTMS_DATA
      IMPLICIT NONE
!
!****	TRANSVERSE MERCATOR PROJECTION
!	CONVERSION OF GEODETIC COORDINATES TO GRID COORDINATES
!****	Programmed by T. Vincenty, NGS,	in July	1984.
!****************  SYMBOLS AND	DEFINITIONS *************************
!   Latitude positive north, longitude positive	west.  All angles are
!     in radian	measure.
!   N, E are northing and easting coordinates respectively.
!   LAT, LON are latitude and longitude	respectively.
!   CONV is convergence.
!   KP is point	scale factor.
!   ER is equatorial radius of the ellipsoid (=	major semiaxis).
!   ESQ	is the square of first eccentricity of the ellipsoid.
!   EPS	is the square of second	eccentricity of	the ellipsoid.
!   CM is the central meridian of the projection zone.
!   FE is false	easting	value at the central meridian.
!   FN is "false northing" at the southernmost latitude, usually zero.
!   SF is scale	factor at the central meridian.
!   SO is meridional distance (multiplied by the scale factor) from
!     the equator to the southernmost parallel of latitude for the zone.
!   R is the radius of the rectifying sphere (used for computing
!     meridional distance from latitude	and vice versa).
!   A, B, C, U,	V, W are other precomputed constants for determination
!     of meridional distance from latitude and vice versa.
!
!   The	formula	used in	this subroutine	gives geodetic accuracy	within
!   zones of 7 degrees in east-west extent.  Within State transverse
!   Mercator projection	zones, several minor terms of the equations
!   may	be omitted (see	a separate NGS publication).  If programmed
!   in full, the subroutine can	be used	for computations in surveys
!   extending over two zones.
!
!********************************************************************
      OM=FI + A*SIN(2.*FI) + B*SIN(4.*FI) + C*SIN(6.*FI)
      S=R*OM*SF
      SINFI=SIN(FI)
      COSFI=COS(FI)
      TN=SINFI/COSFI
      TS=TN**2
      ETS=EPS*COSFI**2
      L=(LAM-CM)*COSFI
      LS=L*L
      RN=SF*ER/SQRT(1.-ESQ*SINFI**2)

      A2=RN*TN/2.
      A4=(5.-TS+ETS*(9.+4.*ETS))/12.
      A6=(61.+TS*(TS-58.)+ETS*(270.-330.*TS))/360.
      A1=-RN
      A3=(1.-TS+ETS)/6.
      A5=(5.+TS*(TS-18.)+ETS*(14.-58.*TS))/120.
      A7=(61.-479.*TS+179.*TS**2-TS**3)/5040.
      NORTH=S-SO + A2*LS*(1.+LS*(A4+A6*LS)) - FN
      EAST=FE +	A1*L*(1.+ LS*(A3+LS*(A5+A7*LS)))

      IF(NORTH.LT.0.0) THEN
       NORTH = NORTH * (-1.0D0)
      ENDIF

!** CONVERGENCE
      C1=-TN
      C3=(1.+3.*ETS+2.*ETS**2)/3.
      C5=(2.-TS)/15.
      CONV=C1*L*(1.+LS*(C3+C5*LS))

!** POINT SCALE FACTOR
      F2=(1.+ETS)/2.
      F4=(5.-4.*TS+ETS*( 9.-24.*TS))/12.
      KP=SF*(1.+F2*LS*(1.+F4*LS))

      RETURN
      END
!********************************************************************
      SUBROUTINE TCONST	
      USE UTMS_DATA
      IMPLICIT NONE
!
!**** TRANSVERSE MERCATOR PROJECTION
!      PRECOMPUTATION OF CONSTANTS
!**** Programmed by T.	Vincenty, NGS, in July 1984.
!******************* SYMBOLS AND DEFINITIONS  **********************
!   ER is equatorial radius of the ellipsoid (=	major semiaxis).
!   RF is reciprocal of	flattening of the ellipsoid.
!   SF is scale	factor of the central meridian.
!   OR is southernmost parallel	of latitude (in	radians) for which
!     the northing coordinate is zero at the central meridian.
!   R, A, B, C,	U, V, W	are ellipsoid constants	used for computing
!     meridional distance from latitude	and vice versa.
!   SO is meridional distance (multiplied by the scale factor) from
!     the equator to the southernmost parallel of latitude.
!*****************************************************************
!
      F=1.D0/RF
      ESQ=(F+F-F**2)
      EPS=ESQ/(1.-ESQ)
      PR=(1.-F)*ER
      EN=(ER-PR)/(ER+PR)
      A=-1.5D0*EN + (9./16.)*EN**3
      B= 0.9375D0*EN**2	- (15./32.)*EN**4
      C=-(35./48.)*EN**3
      U=1.5D0*EN - (27./32.)*EN**3
      V=1.3125D0*EN**2 - (55./32.)*EN**4
      W=(151./96.)*EN**3
      R=ER*(1.-EN)*(1.-EN**2)*(1.+2.25D0*EN**2+(225./64.)*EN**4)
      OMO=OR + A*SIN(2.*OR) + B*SIN(4.*OR) + C*SIN(6.*OR)
      SO=SF*R*OMO
      RETURN
      END