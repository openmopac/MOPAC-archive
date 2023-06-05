      SUBROUTINE COMPFG(XPARAM,INT,ESCF,FULSCF,GRAD,LGRAD,*)            CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION XPARAM(MAXPAR),GRAD(MAXPAR)
      LOGICAL LGRAD, FULSCF
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR)                 IR0394
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /ATHEAT/ ATHEAT
C     COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC),KDUMMY,NBAND(NUMATM)
      COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC*2),KDUMMY,NBAND(NUMATM)
      COMMON /ENUCLR/ ENUCLR
      COMMON /NATYPE/ NZTYPE(120),MTYPE(30),LTYPE
      COMMON /ELECT / ELECT
      PARAMETER (MDUMY=MAXPAR**2-MPACK)
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MDUMY)
      COMMON /HMATRX/ H(MPACK)
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE,USEMM
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
C
C PARAMETER FILE FOR HH GAUSSIAN REPLUSION AND COMMON BLOCK TO HOLD     JP1203
C THE GAUSSIAN PARAMETERS                                                 ..
C                                                                         ..
      CHARACTER FHHGAU*64                                                 ..
      COMMON /HHGAU/ HHA0(3),HHR0(3),HHD0(3)                            JP1203
C
C***********************************************************************
C
C   COMPFG CALCULATES (A) THE HEAT OF FORMATION OF THE SYSTEM, AND
C                     (B) THE GRADIENTS, IF LGRAD IS .TRUE.
C
C   ON INPUT  XPARAM = ARRAY OF PARAMETERS TO BE USED IN INTERNAL COORDS
C             LGRAD  = .TRUE. IF GRADIENTS ARE NEEDED, .FALSE. OTHERWISE
C             INT    = NEVER USED, RESERVED FOR FUTURE USE
C             FULSCF = .TRUE. IF FULL SCF TO BE DONE, .FALSE. OTHERWISE.
C
C   ON OUTPUT ESCF  = HEAT OF FORMATION (KCAL/MOL).
C             GRAD   = ARRAY OF GRADIENTS, IF LGRAD = .TRUE.
C
C***********************************************************************
      COMMON /NUMCAL/ NUMCAL
      COMMON /MESAGE/ IFLEPO, IERR                                      IR0694
      COMMON /KEYWRD/KEYWRD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /SCRTCH/ ANSCR(10000),IANSCP                               LF0610
      CHARACTER*160 KEYWRD
      LOGICAL DEBUG, INT, PRINT, ANALYT, LARGE, USEMM
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      DIMENSION COORD(3,NUMATM), W(N2ELEC), DEGREE(3)
      EQUIVALENCE (W,WJ)
         SAVE                                                           GL0892
      DATA ICALCN /0/
C                 MNDO     AM1   RM1   PM3      MINDO/3
C
C On IBM you can initialize common only inside BLOCK DATA               IR0394
C     DATA HTYPE/ 6.1737D0,  3.3191D0,  7.1853D0,  1.7712D0/
      htype(1)=6.1737D0
      htype(2)=3.3191D0
      htype(3)=7.1853D0
      htype(4)=1.7712D0
C
C      WRITE(6,*) "** Entering COMPFG **"
      IF (ICALCN .NE. NUMCAL) THEN
C      WRITE(6,*) "** 1st time in COMPFG **"
         ICALCN = NUMCAL
         LTYPE=0
         DO 30 I=1,NUMAT
            IF(NAT(I).LT.99)THEN
               DO 10 J=1,LTYPE
   10          IF(NAT(I).EQ.MTYPE(J)) GOTO 20
               LTYPE=LTYPE+1
               MTYPE(LTYPE)=NAT(I)
               NZTYPE(NAT(I))=LTYPE
C
C       LTYPE = NUMBER OF TYPES OF REAL ATOM PRESENT
C       MTYPE = TYPES OF REAL ATOMS PRESENT
               J=LTYPE
   20          CONTINUE
            ENDIF
   30    CONTINUE
         ANALYT=(INDEX(KEYWRD,'ANALYT').NE.0)
c#         ANALYT=((INDEX(KEYWRD,'ANALYT').NE.0).OR.
c#     &           (INDEX(KEYWRD,'PMOV1').NE.0))                          LF1010
         IF(ANALYT.OR.PMODS(1).OR.PMODS(7))CALL SETUPG(*9999)             CSTP(call)
         DEGREE(1)=1.D0
         IF(INDEX(KEYWRD,' XYZ').NE.0)THEN
            DEGREE(2)=1.D0
         ELSE
            DEGREE(2)=180.D0/3.141592652589D0
         ENDIF
         DEGREE(3)=DEGREE(2)
         LARGE=(INDEX(KEYWRD,'LARGE') .NE. 0)
         PRINT=(INDEX(KEYWRD,'COMPFG') .NE. 0)
         DEBUG=(INDEX(KEYWRD,'DEBUG') .NE. 0 .AND. PRINT)
      ENDIF
C
C SET UP COORDINATES FOR CURRENT CALCULATION
C
C       PLACE THE NEW VALUES OF THE VARIABLES IN THE ARRAY GEO.
C       MAKE CHANGES IN THE GEOMETRY.
      DO 40 I=1,NVAR
         K=LOC(1,I)
         L=LOC(2,I)
   40 GEO(L,K)=XPARAM(I)
C#      WRITE(6,'(3F18.11)')(XPARAM(I),I=1,3)
C      IMPOSE THE SYMMETRY CONDITIONS + COMPUTE THE DEPENDENT-PARAMETERS
      IF(NDEP.NE.0) CALL SYMTRY(*9999)                                   CSTP(call)
C      NOW COMPUTE THE ATOMIC COORDINATES.
      IF( DEBUG ) THEN
         IF( LARGE ) THEN
            K=NUMAT
         ELSE
            K=MIN(5,NUMAT)
         ENDIF
         IF(DOPRNT) WRITE(6,FMT='('' INTERNAL COORDS'',/100(/,3F12.6))')CIO
     1            ((GEO(J,I)*DEGREE(J),J=1,3),I=1,K)                    CIO
      END IF
cDEBUG***************
CDEBUG         WRITE(6,*) "BEFORE COMPFG CALLS GMETRY:"
CDEBUG         WRITE(6,FMT='('' INTERNAL COORDS'',/100(/,3F12.6))')
CDEBUG     1            ((GEO(J,I)*DEGREE(J),J=1,3),I=1,K)
cDEBUG****************
      CALL GMETRY(GEO,COORD,*9999)                                       CSTP(call)
cDEBUG***************
CDEBUG         WRITE(6,*) "AFTER  COMPFG CALLS GMETRY:"
CDEBUG         WRITE(6,FMT='('' INTERNAL COORDS'',/100(/,3F12.6))')
CDEBUG     1            ((GEO(J,I)*DEGREE(J),J=1,3),I=1,K)
cDEBUG****************
      IF( DEBUG ) THEN
         IF( LARGE ) THEN
            K=NUMAT
         ELSE
            K=MIN(5,NUMAT)
         ENDIF
         IF(DOPRNT)WRITE(6,FMT='('' CARTESIAN COORDS'',/100(/,3F12.6))')CIO
     1            ((COORD(J,I),J=1,3),I=1,K)                            CIO
      END IF
CIO      IF(ANALYT)REWIND 2                                                CIO
      IF(ANALYT) IANSCP=1                                                LF0610 / CIO
      CALL HCORE(COORD, H, W, WJ, WK, ENUCLR,*9999)                      CSTP(call)
C
C COMPUTE THE HEAT OF FORMATION.
C
      IF(NORBS.GT.0.AND.NELECS.GT.0) THEN
         CALL ITER(H, W, WJ, WK, ELECT, FULSCF,.TRUE.,*9999)             CSTP(call)
C GA-FRIENDLY : uncomment the next line if linked with GA code
C        IF(IERR .eq. 2) RETURN                                           GA-ON
      ELSE
         ELECT=0.D0
      ENDIF
      ESCF=(ELECT+ENUCLR)*23.061D0+ATHEAT
      DO 50 I=1,NNHCO
         CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,I),ANGLE, CSTP(call)
     & *9999)                                                           CSTP(call)
         ESCF=ESCF+HTYPE(ITYPE)*SIN(ANGLE)**2
   50 CONTINUE
C
C ADD H-H REPULSIVE CORRECTION FOR PM3 AND AM1                          JP1203
C                                                                         ..  
      IF ( INDEX(KEYWRD,'HHON').NE.0 .AND.                                ..
     &     ((INDEX(KEYWRD,'PM3').NE.0                                   JP1203
     &       .AND.INDEX(KEYWRD,'PM3-D').EQ.0).OR.                       LF0309
     &      (INDEX(KEYWRD,'AM1').NE.0                                   JP1203
     &       .AND.INDEX(KEYWRD,'AM1-D').EQ.0))) THEN                    LF0509
C                                                                       JP1203
C READ THE HH GAUSSIAN FROM AN EXTERNAL FILE                              ..
C                                                                         ..
       IF ( INDEX(KEYWRD,'HHON=').NE.0 ) THEN                             ..
         I=INDEX(KEYWRD,'HHON=')+5                                        ..
         J=INDEX(KEYWRD(I:),' ')+I-1                                      ..
         FHHGAU=KEYWRD(I:J)                                               ..
C                                                                         ..
         OPEN (UNIT=98,STATUS='UNKNOWN',FILE=FHHGAU)                      ..  / No CIO
         READ(98,*) J                                                     ..  / CIO
         IF (J .LT. 0 .OR. J .GT. 3) THEN                                 ..
          IF(DOPRNT)WRITE(6, *)'ONLY UP TO THREE H...H GAUSSIAN ALLOWED'  ..  / CIO
          RETURN 1                                                        ..  / CSTP (stop)
         ENDIF                                                            ..
         DO I = 1, J                                                      ..
           READ(98, *) HHA0(I), HHR0(I), HHD0(I)                          ..  / No CIO
         ENDDO                                                            ..
         CLOSE(98)                                                        ..  / No CIO
C                                                                         ..
C SET SOME DEFAULT VALUES WITH ZERO AMPLITUDE GAUSSIAN IF NOT             ..
C ALL THREE GAUSSIAN ARE SPECIFIED                                        ..
C                                                                         ..
         DO I = J+1, 3                                                    .. 
             HHA0(I)=0.0D0                                                ..
             HHR0(I)=1.0D0                                                ..
             HHD0(I)=0.3D0                                                ..
         ENDDO                                                            ..
C                                                                         ..
C OTHER WISE, PM3-CHC-SRP AND AM1-CHC-SRP GAUSSIAN AS DEFAULT             ..
C                                                                         ..
       ELSE                                                               ..
         DO I = 1, 3                                                      ..
             HHA0(I)=0.0D0                                                ..
             HHR0(I)=1.0D0                                                ..  
             HHD0(I)=0.3D0                                                ..
         ENDDO                                                          JP1203
         IF ( INDEX(KEYWRD,'PM3').NE.0.AND.INDEX(KEYWRD,'PM-3').EQ.0 )  LF0309
     1    THEN                                                          JP1203
            HHA0(1) = 3.198D0                                             ..
            HHR0(1) = 1.700D0                                             ..
            HHD0(1) = 0.522D0                                             ..
         ELSE IF ( (INDEX(KEYWRD,'AM1').NE.0)                           JP1203
     &          .AND.(INDEX(KEYWRD,'AM1-D').EQ.0) ) THEN                LF0509
            HHA0(1) = 1.064D0                                           JP1203
            HHR0(1) = 1.603D0                                             ..
            HHD0(1) = 0.808D0                                             ..
         ENDIF                                                            ..
       ENDIF                                                              ..
C                                                                         ..
C ALL HH REPULSION                                                        ..
C                                                                         ..
       DO I = 1, NUMAT                                                    ..
          DO J = 1, I-1                                                   ..
             IF (NAT(I).EQ.1 .AND. NAT(J).EQ.1) THEN                      ..
                RHH=0.0D0                                                 ..
                DO K=1, 3                                                 ..
                   RHH = RHH + (COORD(K,I)-COORD(K,J))**2                 ..
                ENDDO                                                     ..
                RHH = SQRT(RHH)                                           ..
                DO L=1, 3                                                 ..
                  ESCF = ESCF +                                           ..
     &                   HHA0(L)*EXP(-((RHH-HHR0(L))/HHD0(L))**2)         ..
                ENDDO                                                     .. 
             ENDIF                                                        ..
           ENDDO                                                          ..
       ENDDO                                                              ..
C                                                                         ..
      ENDIF                                                             JP1203
C
      IF(PRINT.AND.DOPRNT) WRITE(6,'(/10X,'' HEAT OF FORMATION'',G30.17)CIO
     &                           ') ESCF                                CIO
C
C FIND DERIVATIVES IF DESIRED
C
      IF(LGRAD) CALL DERIV(GEO,GRAD,*9999)                               CSTP(call)
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY COMPFG_INIT                                                 CSAV
            ANALYT = .FALSE.                                            CSAV
             ANGLE = 0.0D0                                              CSAV
             COORD = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
            DEGREE = 0.0D0                                              CSAV
            FHHGAU = ''                                                 CSAV
            ICALCN = 0                                                  CSAV
             ITYPE = 0                                                  CSAV
                 K = 0                                                  CSAV
                 L = 0                                                  CSAV
             LARGE = .FALSE.                                            CSAV
             PRINT = .FALSE.                                            CSAV
               RHH = 0.0D0                                              CSAV
             USEMM = .FALSE.                                            CSAV
      RETURN                                                            CSAV
      END
