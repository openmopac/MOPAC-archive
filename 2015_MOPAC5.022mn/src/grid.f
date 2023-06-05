      SUBROUTINE GRID(*)                                                CSTP
************************************************************************
*
*  GRID CALCULATES THE ENERGY-SURFACE RESULTING FROM VARIATION OF
*       TWO COORDINATES. THE STEP-SIZE IS STEP1 AND STEP2, AND A 11
*       BY 11 GRID OF POINTS IS GENERATED
*
************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /GRAVEC/ COSINE
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2
      COMMON /KEYWRD/ KEYWRD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION SURFAC(11,11)
      CHARACTER*160 KEYWRD
CSAV         SAVE                                                           GL0892
      STEP1=READA(KEYWRD,INDEX(KEYWRD,'STEP1')+6)
      STEP2=READA(KEYWRD,INDEX(KEYWRD,'STEP2')+6)
C
C  THE CENTRAL VALUE OF THE FIRST AND SECOND DIMENSIONS ARE
C      GEO(LPARA1,LATOM1) AND GEO(LPARA2,LATOM2)
      NPTS=11
C NPTS MUST BE ODD, IN ORDER TO HAVE A CENTER POINT.
      NPTS2=NPTS/2
      DEGREE=180.D0/3.14159265359D0
      IF(LPARA1.NE.1)STEP1=STEP1/DEGREE
      IF(LPARA2.NE.1)STEP2=STEP2/DEGREE
      START1=GEO(LPARA1,LATOM1)-(NPTS2+1)*STEP1
      START2=GEO(LPARA2,LATOM2)-(NPTS2+1)*STEP2
C
C  NOW TO SWEEP THROUGH THE GRID OF POINTS LEFT TO RIGHT THEN RIGHT
C  TO LEFT. THIS SHOULD AVOID THE GEOMETRY OR SCF GETTING MESSED UP.
C
      GEO(LPARA1,LATOM1)=START1
      GEO(LPARA2,LATOM2)=START2
      IONE=-1.D0
      IF(LPARA1.NE.1) THEN
         C1=DEGREE
      ELSE
         C1=1.D0
      ENDIF
      IF(LPARA2.NE.1) THEN
         C2=DEGREE
      ELSE
         C2=1.D0
      ENDIF
      IF (DOPRNT)                                                       CIO
     & WRITE(6,'(''   FIRST VARIABLE   SECOND VARIABLE   FUNCTION'')')  CIO
      DO 20 ILOOP=1,NPTS
         GEO(LPARA1,LATOM1)=GEO(LPARA1,LATOM1)+STEP1
         IONE=-IONE
         JLOOP1=0
         IF(IONE.LT.0)JLOOP1=NPTS+1
         DO 10 JLOOP=1,NPTS
            JLOOP1=JLOOP1+IONE
            GEO(LPARA2,LATOM2)=GEO(LPARA2,LATOM2)+STEP2*IONE
            CALL FLEPO(XPARAM, NVAR, ESCF,*9999)                        CSTP(call)
            SURFAC(ILOOP,JLOOP1)=ESCF
            IF (DOPRNT)                                                  CIO
     &       WRITE(6,'('' :'',F16.5,F16.5,F13.6)')GEO(LPARA1,LATOM1)*C1, CIO
     &       GEO(LPARA2,LATOM2)*C2,ESCF                                  CIO
   10    CONTINUE
         GEO(LPARA2,LATOM2)=GEO(LPARA2,LATOM2)+STEP2*IONE
   20 CONTINUE
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(/10X,''HORIZONTAL: VARYING SECOND PARAMETER,'',
     1          /10X,''VERTICAL:   VARYING FIRST PARAMETER'')')
      WRITE(6,'(/10X,''WHOLE OF GRID, SUITABLE FOR PLOTTING'',//)')
      ENDIF                                                             CIO
      DO 30 I=1,NPTS
   30 IF (DOPRNT) WRITE(6,'(11F7.2)')(SURFAC(J,I),J=1,NPTS)             CIO
      RETURN
 9999 RETURN 1                                                          CSTP
      END
