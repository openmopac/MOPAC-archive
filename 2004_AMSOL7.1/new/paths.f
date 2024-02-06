      SUBROUTINE PATHS 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
C     COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR) 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP 
      COMMON /ONESCM/ ICONTR(100)
************************************************************************ 
* 
*   PATH FOLLOWS A REACTION COORDINATE.   THE REACTION COORDINATE IS ON 
*        ATOM LATOM, AND IS A DISTANCE IF LPARAM=1, 
*                           AN ANGLE   IF LPARAM=2, 
*                           AN DIHEDRALIF LPARAM=3. 
* 
************************************************************************ 
      DIMENSION GD(MAXPAR),XLAST(MAXPAR),MDFP(20),XDFP(20) 
      CHARACTER*10 TYPE(3) 
      LOGICAL BFGS                                                      DJG0495
      SAVE
      DATA TYPE / 'ANGSTROMS ','DEGREES   ','DEGREES   '/ 
      BFGS=(IBFGS.NE.0.OR.IDFP.NE.0)                                    DJG0995
      ILOOP=1 
      IF(IRESTA.EQ.1) THEN                                              DJG0496
         MDFP(9)=0 
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP) 
         WRITE(JOUT,'(//10X,'' RESTARTING AT POINT'',I3)')ILOOP         GDH1095
      ENDIF 
      IF(ILOOP.GT.1) GOTO 10 
      CALL PORCPU (TIME1)                                               GL0492
      IF (BFGS) THEN                                                    DJG0495
         WRITE(JOUT,'(''  ABOUT TO ENTER FLEPO FROM PATH'')')           DJG0495
         CALL FLEPO(XPARAM,NVAR,FUNCT)                                  DJG0495
         IF (ISTOP.NE.0) RETURN                                         GDH1095
      ELSE                                                              DJG0495
         WRITE(JOUT,'(''  ABOUT TO ENTER EF FROM PATH'')')              DJG0495
         CALL EF(XPARAM,NVAR,FUNCT)                                     DJG0495
         IF (ISTOP.NE.0) RETURN                                         GDH1095
      ENDIF                                                             DJG0495
      WRITE(JOUT,5)                                                     DJG0495
    5 FORMAT(/)                                                         DJG0495
      WRITE(JOUT,
     .    '(''  OPTIMISED VALUES OF PARAMETERS, INITIAL POINT'')') 
C                                                                       DJG0995
C    SET LAST VALUE OF REACT TO -999.0D0 TO LET WRITES KNOW THAT THIS   DJG0995
C    IS THE FIRST TIME THROUGH FROM PATHS.  AFTER CALL RETURN LAST      DJG0995
C    VALUE OF REACT TO ITS PROPER VALUE.                                DJG0995
C                                                                       DJG0995
C    ICONTR IS A FLAG TO TELL ALL THE SUBROUTINES THAT IT IS THE FIRST  DJG0995
C    PASS THROUGH FOR EACH REACTION COORDINATE                          DJG0995
C                                                                       DJG0995
      TMPHLD=REACT(100)                                                 DJG0995
      REACT(100)=-999.0D0                                               DJG0995
      CALL WRITES(TIME1,FUNCT)                                          GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      REACT(100)=TMPHLD                                                 DJG0995
      DO 8 I=1,100                                                      DJG0995
         ICONTR(I)=1                                                    DJG0995
    8 CONTINUE                                                          DJG0995
      CALL PORCPU (TIME1)                                               GL0492
   10 CONTINUE 
      IF(ILOOP.GT.2) GOTO 40 
      GEO(LPARAM,LATOM)=REACT(2) 
      IF(ILOOP.EQ.1) THEN 
         X0=REACT(1) 
         X1=X0 
         X2=REACT(2) 
         IF(X2.LT. -100.D0) RETURN                                      DJG0496
         DO 20 I=1,NVAR 
            ALPARM(2,I)=XPARAM(I) 
   20    ALPARM(1,I)=XPARAM(I) 
         ILOOP=2 
      ENDIF 
      RNORD=REACT(2)                                                    DJG0995
      IF(LPARAM.GT.1) RNORD=RNORD*57.29577951D0                         DJG0995
      WRITE(JOUT,'(16(''*****'')//17X,''REACTION COORDINATE = ''        GDH1095
     1,F12.4,2X,A10,19X//16(''*****''))')RNORD,TYPE(LPARAM)             DJG0495
      IF (BFGS) THEN                                                    DJG0495
         CALL FLEPO(XPARAM,NVAR,FUNCT)                                  DJG0495
         IF (ISTOP.NE.0) RETURN                                         GDH1095
      ELSE                                                              DJG0495
         CALL EF(XPARAM,NVAR,FUNCT)                                     DJG0495
         IF (ISTOP.NE.0) RETURN                                         GDH1095
      ENDIF                                                             DJG0495
      CALL WRITES(TIME1,FUNCT)                                          GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      CALL PORCPU (TIME1)                                               GL0492
      DO 25 I=1,100                                                     DJG0995
         ICONTR(I)=1                                                    DJG0995
   25 CONTINUE                                                          DJG0995
      DO 30 I=1,NVAR 
   30 ALPARM(3,I)=XPARAM(I) 
C 
C   NOW FOR THE MAIN INTERPOLATION ROUTE 
C 
      IF(ILOOP.EQ.2)ILOOP=3 
   40 CONTINUE 
      DO 110 ILOOP = ILOOP,100 
         IF(REACT(ILOOP).LT. -100.D0) RETURN                            DJG0496
C 
         RNORD=REACT(ILOOP) 
         IF(LPARAM.GT.1) RNORD=RNORD*57.29577951D0 
         WRITE(JOUT,'(16(''*****'')//19X,''REACTION COORDINATE = '' 
     1,F12.4,2X,A10,19X//16(''*****''))')RNORD,TYPE(LPARAM) 
C 
         X3=REACT(ILOOP) 
         C3=(X0**2-X1**2)*(X1-X2)-(X1**2-X2**2)*(X0-X1) 
C      WRITE(JOUT,'(''   C3:'',F13.7)')C3 
         IF (ABS(C3) .LT. 1.D-8) THEN 
C 
C    WE USE A LINEAR INTERPOLATION 
C 
            CC1=0.D0 
            CC2=0.D0 
         ELSE 
C    WE DO A QUADRATIC INTERPOLATION 
C 
            CC1=(X1-X2)/C3 
            CC2=(X0-X1)/C3 
         END IF 
         CB1=1.D0/(X1-X2) 
         CB2=(X1**2-X2**2)*CB1 
C 
C    NOW TO CALCULATE THE INTERPOLATED COORDINATES 
C 
         DO 50 I=1,NVAR 
            DELF0=ALPARM(1,I)-ALPARM(2,I) 
            DELF1=ALPARM(2,I)-ALPARM(3,I) 
            ACONST = CC1*DELF0-CC2*DELF1 
            BCONST = CB1*DELF1-ACONST*CB2 
            CCONST = ALPARM(3,I) - BCONST*X2 - ACONST*X2**2 
            XPARAM(I)=CCONST+BCONST*X3+ACONST*X3**2 
            ALPARM(1,I)=ALPARM(2,I) 
   50    ALPARM(2,I)=ALPARM(3,I) 
C 
C   NOW TO CHECK THAT THE GUESSED GEOMETRY IS NOT TOO ABSURD 
C 
         DO 60 I=1,NVAR 
   60    IF(ABS(XPARAM(I)-ALPARM(3,I)) .GT. 0.2) GOTO 70 
         GOTO 90 
   70    WRITE(JOUT,
     .'('' GEOMETRY TOO UNSTABLE FOR EXTRAPOLATION TO BE USED 
     1''/ ,'' - THE LAST GEOMETRY IS BEING USED TO START THE NEXT'' 
     2,'' CALCULATION'')') 
         DO 80 I=1,NVAR 
   80    XPARAM(I)=ALPARM(3,I) 
   90    CONTINUE 
         X0=X1 
         X1=X2 
         X2=X3 
         GEO(LPARAM,LATOM)=REACT(ILOOP) 
         IF (BFGS) THEN                                                 DJG0495
            CALL FLEPO(XPARAM,NVAR,FUNCT)                               DJG0495
            IF (ISTOP.NE.0) RETURN                                      GDH1095
         ELSE                                                           DJG0495
            CALL EF(XPARAM,NVAR,FUNCT)                                  DJG0495
            IF (ISTOP.NE.0) RETURN                                      GDH1095
         ENDIF                                                          DJG0495
         CALL WRITES(TIME1,FUNCT)                                       GL0492
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         CALL PORCPU (TIME1)                                            GL0492
         DO 100 I=1,NVAR 
  100    ALPARM(3,I)=XPARAM(I) 
         DO 105 I=1,100                                                 DJG0995
            ICONTR(I)=1                                                 DJG0995
  105    CONTINUE                                                       DJG0995
  110 CONTINUE 
      RETURN                                                            GDH0495
      END 
