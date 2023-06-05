      SUBROUTINE REACT1(ESCF,*)                                         CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /GEOM  / GEO(3,NUMATM)
      DIMENSION GEOA(3,NUMATM), GEOVEC(3,NUMATM),
     1          P1STOR(MPACK), P2STOR(MPACK),
     2          P3STOR(MPACK), XOLD(MAXPAR), GROLD(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM), NB(NUMATM), NC(NUMATM)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /ISTOPE/ AMS(120)
      COMMON /GRAVEC/ COSINE
      COMMON /KEYWRD/ KEYWRD
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /REACTN/ STEP, GEOA, GEOVEC,CALCST
      COMMON /IOCM/ IREAD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      LOGICAL GRADNT, FINISH, XYZ, INT, GOK(2)
************************************************************************
*
*  REACT1 DETERMINES THE TRANSITION STATE OF A CHEMICAL REACTION.
*
*   REACT WORKS BY USING TWO SYSTEMS SIMULTANEOUSLY, THE HEATS OF
*   FORMATION OF BOTH ARE CALCULATED, THEN THE MORE STABLE ONE
*   IS MOVED IN THE DIRECTION OF THE OTHER. AFTER A STEP THE
*   ENERGIES ARE COMPARED, AND THE NOW LOWER-ENERGY FORM IS MOVED
*   IN THE DIRECTION OF THE HIGHER-ENERGY FORM. THIS IS REPEATED
*   UNTIL THE SADDLE POINT IS REACHED.
*
*   IF ONE FORM IS MOVED 3 TIMES IN SUCCESSION, THEN THE HIGHER ENERGY
*   FORM IS RE-OPTIMIZED WITHOUT SHORTENING THE DISTANCE BETWEEN THE TWO
*   FORMS. THIS REDUCES THE CHANCE OF BEING CAUGHT ON THE SIDE OF A
*   TRANSITION STATE.
*
************************************************************************
      DIMENSION IDUM1(NUMATM), IDUM2(NUMATM), XSTORE(MAXPAR),
     +IDUM3(NUMATM), IDUM4(NUMATM), COORD(3,NUMATM), IROT(2,3)
      EQUIVALENCE (IDUMMY,COORD)
      CHARACTER*160 KEYWRD
         SAVE                                                           GL0892
      DATA IROT/1,2,1,3,2,3/
      GOLD=0.D0
      GOK(1)=.FALSE.
      GOK(2)=.FALSE.
      XYZ=(INDEX(KEYWRD,' XYZ') .NE. 0)
      GRADNT=(INDEX(KEYWRD,'GRAD') .NE. 0)
      I=(INDEX(KEYWRD,' BAR'))
      STEPMX=0.15D0
      IF(I.NE.0) STEPMX=READA(KEYWRD,I)
      MAXSTP=1000
C
C    READ IN THE SECOND GEOMETRY.
C
      IF(XYZ) THEN
         CALL GETGEO(IREAD,LABELS,GEOA,LOC,NA,NB,NC,AMS,NATOMS,INT       CSTP(call)
     &               ,*9999)                                             CSTP(call)
      ELSE
         CALL GETGEO(IREAD,IDUM1,GEOA,IDUMMY,
     1         IDUM1,IDUM2,IDUM3,AMS,NATOMS,INT,*9999)                   CSTP(call)
C
C  IF INTERNAL COORDINATES ARE TO BE USED, CHECK THE CONNECTIVITY
C
      L=0
      DO 8 I=1,NATOMS
      IF(IDUM1(I).NE.NA(I))THEN
      L=L+1
      IF(L.EQ.1.AND.DOPRNT)WRITE(6,'(10X,                               CIO
     &                      ''ERRORS DETECTED IN CONNECTIVITY'')')      CIO
      IF (DOPRNT) WRITE(6,'(A,I3,A,I3,A,I3,A)')' FOR ATOM',I,           CIO
     &   ' THE BOND LABELS ARE DIFFERENT:      ',IDUM1(I),' AND',NA(I)  CIO
      ENDIF
      IF(IDUM2(I).NE.NB(I))THEN
      L=L+1
      IF(L.EQ.1.AND.DOPRNT)WRITE(6,'(10X,                               CIO
     &                       ''ERRORS DETECTED IN CONNECTIVITY'')')     CIO
      IF (DOPRNT) WRITE(6,'(A,I3,A,I3,A,I3,A)')' FOR ATOM',I,           CIO
     &  ' THE BOND ANGLE LABELS ARE DIFFERENT:',IDUM2(I),' AND',NB(I)   CIO
      ENDIF
      IF(IDUM3(I).NE.NC(I))THEN
      L=L+1
      IF(L.EQ.1.AND.DOPRNT)WRITE(6,'(10X,                               CIO
     &                    ''ERRORS DETECTED IN CONNECTIVITY'')')        CIO
      IF (DOPRNT) WRITE(6,'(A,I3,A,I3,A,I3,A)')' FOR ATOM',I,           CIO
     &   ' THE DIHEDRAL LABELS ARE DIFFERENT:  ',IDUM3(I),' AND',NC(I)  CIO
      ENDIF
  8   CONTINUE
      IF(L.NE.0.AND.DOPRNT)WRITE(6,'(10X,A)')                           CIO
     &                 ' CORRECT BEFORE RESUBMISSION'                   CIO
      IF(L.NE.0) RETURN 1                                               CSTP (stop)
      ENDIF
      IF (DOPRNT) CLOSE (IREAD)                                         CIO
      TIME0= SECOND()
C
C  SWAP FIRST AND SECOND GEOMETRIES AROUND
C  SO THAT GEOUT CAN OUTPUT DATA ON SECOND GEOMETRY.
C
      NUMAT2=0
      DO 10 I=1,NATOMS
         IF(LABELS(I).NE.99) NUMAT2=NUMAT2+1
         CONST=1.D0
         DO 10 J=1,3
            X=GEOA(J,I)*CONST
            CONST=0.0174532925D0
            GEOA(J,I)=GEO(J,I)
            GEO(J,I)=X
   10 CONTINUE
      IF(NUMAT2.NE.NUMAT) THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &        '' NUMBER OF ATOMS IN SECOND SYSTEM IS INCORRECT'',/)')   CIO
         IF (DOPRNT) WRITE(6,'(                                         CIO
     &            '' NUMBER OF ATOMS IN FIRST  SYSTEM ='',I4)')NUMAT    CIO
         IF (DOPRNT) WRITE(6,'(                                         CIO
     &            '' NUMBER OF ATOMS IN SECOND SYSTEM ='',I4)')NUMAT2   CIO
         GOTO 270
      ENDIF
      IF (DOPRNT) WRITE(6,'(//10X,'' GEOMETRY OF SECOND SYSTEM'',/)')   CIO
      IF(NDEP.NE.0) CALL SYMTRY(*9999)                                  CSTP(call)
      CALL GEOUT(*9999)                                                 CSTP(call)
C
C     CONVERT TO CARTESIAN, IF NECESSARY
C
      IF(   XYZ   )THEN
         CALL GMETRY(GEO,COORD,*9999)                                   CSTP(call)
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 20 J=1,NUMAT
            SUMX=SUMX+COORD(1,J)
            SUMY=SUMY+COORD(2,J)
   20    SUMZ=SUMZ+COORD(3,J)
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 30 J=1,NUMAT
            GEO(1,J)=COORD(1,J)-SUMX
            GEO(2,J)=COORD(2,J)-SUMY
   30    GEO(3,J)=COORD(3,J)-SUMZ
         IF (DOPRNT) WRITE(6,'(//,                                      CIO
     &            ''  CARTESIAN GEOMETRY OF FIRST SYSTEM'',//)')        CIO
         IF (DOPRNT) WRITE(6,'(3F14.5)')((GEO(J,I),J=1,3),I=1,NUMAT)    CIO
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 40 J=1,NUMAT
            SUMX=SUMX+GEOA(1,J)
            SUMY=SUMY+GEOA(2,J)
   40    SUMZ=SUMZ+GEOA(3,J)
         SUM=0.D0
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 50 J=1,NUMAT
            GEOA(1,J)=GEOA(1,J)-SUMX
            GEOA(2,J)=GEOA(2,J)-SUMY
            GEOA(3,J)=GEOA(3,J)-SUMZ
            SUM=SUM+(GEO(1,J)-GEOA(1,J))**2
     1           +(GEO(2,J)-GEOA(2,J))**2
     2           +(GEO(3,J)-GEOA(3,J))**2
   50    CONTINUE
         DO 100 L=3,1,-1
C
C     DOCKING IS DONE IN STEPS OF 16, 4, AND 1 DEGREES AT A TIME.
C
            CA=COS(4.D0**(L-1)*0.01745329D0)
            SA=SQRT(ABS(1.D0-CA**2))
            DO 90 J=1,3
               IR=IROT(1,J)
               JR=IROT(2,J)
               DO 80 I=1,10
                  SUMM=0.D0
                  DO 60 K=1,NUMAT
                     X         = CA*GEOA(IR,K)+SA*GEOA(JR,K)
                     GEOA(JR,K)=-SA*GEOA(IR,K)+CA*GEOA(JR,K)
                     GEOA(IR,K)=X
                     SUMM=SUMM+(GEO(1,K)-GEOA(1,K))**2
     1                         +(GEO(2,K)-GEOA(2,K))**2
     2                         +(GEO(3,K)-GEOA(3,K))**2
   60             CONTINUE
                  IF(SUMM.GT.SUM) THEN
                     IF(I.GT.1)THEN
                        SA=-SA
                        DO 70 K=1,NUMAT
                           X         = CA*GEOA(IR,K)+SA*GEOA(JR,K)
                           GEOA(JR,K)=-SA*GEOA(IR,K)+CA*GEOA(JR,K)
                           GEOA(IR,K)=X
   70                   CONTINUE
                        GOTO 90
                     ENDIF
                     SA=-SA
                  ENDIF
   80          SUM=SUMM
   90       CONTINUE
  100    CONTINUE
         IF (DOPRNT) THEN                                               CIO
           WRITE(6,'(//,''  CARTESIAN GEOMETRY OF SECOND SYSTEM'',//)')
           WRITE(6,'(3F14.5)')((GEOA(J,I),J=1,3),I=1,NUMAT)
           WRITE(6,'(//,''   "DISTANCE":'',F13.6)')SUM
           WRITE(6,'(//,''  REACTION COORDINATE VECTOR'',//)')
           WRITE(6,'(3F14.5)')((GEOA(J,I)-GEO(J,I),J=1,3),I=1,NUMAT)
         ENDIF                                                          CIO
         NA(1)=99
         J=0
         NVAR=0
         DO 120 I=1,NATOMS
            IF(LABELS(I).NE.99)THEN
               J=J+1
               DO 110 K=1,3
                  NVAR=NVAR+1
                  LOC(2,NVAR)=K
  110          LOC(1,NVAR)=J
               LABELS(J)=LABELS(I)
            ENDIF
  120    CONTINUE
         NATOMS=NUMAT
      ENDIF
C
C   XPARAM HOLDS THE VARIABLE PARAMETERS FOR GEOMETRY IN GEO
C   XOLD   HOLDS THE VARIABLE PARAMETERS FOR GEOMETRY IN GEOA
C
      IF(NVAR.EQ.0)THEN
         IF (DOPRNT) WRITE(6,'(///10X,                                  CIO
     &        ''THERE ARE NO VARIABLES IN THE SADDLE CALCULATION!'')')  CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF
      SUM=0.D0
      DO 130 I=1,NVAR
         GROLD(I)=1.D0
         XPARAM(I)=GEO(LOC(2,I),LOC(1,I))
         XOLD(I)=GEOA(LOC(2,I),LOC(1,I))
  130 SUM=SUM+(XPARAM(I)-XOLD(I))**2
      STEP0=SQRT(SUM)
      ONE=1.D0
      DELL=0.1D0
      EOLD=-2000.D0
      TIME1=SECOND()
      SWAP=0
      LINEAR=(NORBS*(NORBS+1))/2
      DO 140 I=1,LINEAR
         P1STOR(I)=P(I)
         P2STOR(I)=PA(I)
  140 P3STOR(I)=PB(I)
      DO 230 ILOOP=1,MAXSTP
         TIME2=SECOND()
         IF (DOPRNT) WRITE(6,'('' TIME='',F9.2)')TIME2-TIME1            CIO
         TIME1=TIME2
C
C   THIS METHOD OF CALCULATING 'STEP' IS QUITE ARBITARY, AND NEEDS
C   TO BE IMPROVED BY INTELLIGENT GAUSSWORK!
C
         IF (GNORM.LT.1.D-3)GNORM=1.D-3
         IF(DOPRNT)WRITE(6,'('' CURRENT BAR, STEPMX, GNORM'',3F12.7)')  CIO
     1STEP0,STEPMX,GNORM
         STEP=MIN(SWAP,0.5D0, 6.D0/GNORM, DELL,STEPMX*STEP0+0.005D0)
         SWAP=SWAP+1.D0
         DELL=DELL+0.1
         STEP0=STEP0-STEP
         IF(STEP0.LT.0.01D0) GOTO 240
         STEP=STEP0
         DO 150 I=1,NVAR
  150    XSTORE(I)=XPARAM(I)
         CALL FLEPO(XPARAM, NVAR, ESCF,*9999)                           CSTP(call)
         DO 160 I=1,NVAR
  160    XPARAM(I)=GEO(LOC(2,I),LOC(1,I))
         IF (DOPRNT) WRITE(6,'(//10X,''FOR POINT'',I3)')ILOOP           CIO
         IF (DOPRNT) WRITE(6,'('' DISTANCE A - B  '',F12.6)')STEP       CIO
C
C   NOW TO CALCULATE THE "CORRECT" GRADIENTS, SWITCH OFF 'STEP'.
C
         STEP=0.D0
         DO 170 I=1,NVAR
  170    GRAD(I)=GROLD(I)
         CALL COMPFG (XPARAM, .TRUE., FUNCT1,.FALSE.,GRAD,.TRUE.,*9999) CSTP(call)
         DO 180 I=1,NVAR
  180    GROLD(I)=GRAD(I)
         IF (GRADNT) THEN
            IF(DOPRNT)WRITE(6,'(''  ACTUAL GRADIENTS OF THIS POINT'')') CIO
            IF (DOPRNT) WRITE(6,'(8F10.4)')(GRAD(I),I=1,NVAR)           CIO
         ENDIF
         IF (DOPRNT) WRITE(6,'('' HEAT            '',F12.6)')FUNCT1     CIO
         GNORM=SQRT(ddot(nvar,grad,1,grad,1))
         IF (DOPRNT) WRITE(6,'('' GRADIENT NORM   '',F12.6)')GNORM      CIO
         COSINE=COSINE*ONE
         IF (DOPRNT) WRITE(6,'('' DIRECTION COSINE'',F12.6)')COSINE     CIO
         CALL GEOUT(*9999)                                              CSTP(call)
         IF(SWAP.GT.2.9D0 .OR. ILOOP .GT. 3 .AND. COSINE .LT. 0.D0
     1  .OR. ESCF .GT. EOLD)
     2  THEN
            IF(SWAP.GT.2.9D0)THEN
               SWAP=0.D0
            ELSE
               SWAP=0.5D0
            ENDIF
C
C   SWAP REACTANT AND PRODUCT AROUND
C
            FINISH=(GOK(1).AND.GOK(2) .AND. COSINE .LT. 0.D0)
            IF(FINISH) THEN
               IF (DOPRNT) WRITE(6,'(//10X,'' BOTH SYSTEMS ARE ON ''//  CIO
     &''THE SAME SIDE OF THE '',                                        CIO
     &''TRANSITION STATE -'',/10X,'' GEOMETRIES OF THE SYSTEMS'',
     &'' ON EACH SIDE OF THE T.S. ARE AS FOLLOWS'')')
               DO 190 I=1,NVAR
  190          XPARAM(I)=XSTORE(I)
               CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.,  CSTP(call)
     & *9999)                                                           CSTP(call)
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &            '' GEOMETRY ON ONE SIDE OF THE TRANSITION STATE'')')  CIO
               CALL WRITMO(TIME0,FUNCT1,*9999)                          CSTP(call)
            ENDIF
            IF (DOPRNT) WRITE(6,'(                                      CIO
     &                  ''  REACTANTS AND PRODUCTS SWAPPED AROUND'')')  CIO
            ONE=-1.D0
            EOLD=ESCF
            SUM=GOLD
            GOLD=GNORM
            I=1.7+ONE*0.5
            IF(GNORM.GT.10.D0)GOK(I)=.TRUE.
            GNORM=SUM
            DO 200 I=1,NUMAT
               DO 200 J=1,3
                  X=GEO(J,I)
                  GEO(J,I)=GEOA(J,I)
  200       GEOA(J,I)=X
            DO 210 I=1,NVAR
               X=XOLD(I)
               XOLD(I)=XPARAM(I)
  210       XPARAM(I)=X
C
C
C    SWAP AROUND THE DENSITY MATRICES.
C
            DO 220 I=1,LINEAR
               X=P1STOR(I)
               P1STOR(I)=P(I)
               P(I)=X
               X=P2STOR(I)
               P2STOR(I)=PA(I)
               PA(I)=X
               X=P3STOR(I)
               P3STOR(I)=PB(I)
               PB(I)=X
  220       CONTINUE
            IF(FINISH) GOTO 240
         ELSE
            ONE=1.D0
         ENDIF
  230 CONTINUE
  240 CONTINUE
      IF (DOPRNT) WRITE(6,'('' AT END OF REACTION'')')                  CIO
      GOLD=SQRT(ddot(nvar,grad,1,grad,1))
      CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.,*9999)     CSTP(call)
      GNORM=SQRT(ddot(nvar,grad,1,grad,1))
      DO 250 I=1,NVAR
  250 GROLD(I)=XPARAM(I)
      CALL WRITMO(TIME0,FUNCT1,*9999)                                   CSTP(call)
*
* THE GEOMETRIES HAVE (A) BEEN OPTIMIZED CORRECTLY, OR
*                     (B) BOTH ENDED UP ON THE SAME SIDE OF THE T.S.
*
*  TRANSITION STATE LIES BETWEEN THE TWO GEOMETRIES
*
      C1=GOLD/(GOLD+GNORM)
      C2=1.D0-C1
      IF (DOPRNT) THEN                                                  CIO
       WRITE(6,'('' BEST ESTIMATE GEOMETRY OF THE TRANSITION STATE'')')
       WRITE(6,'(//10X,'' C1='',F8.3,''C2='',F8.3)')C1,C2
      ENDIF                                                             CIO
      DO 260 I=1,NVAR
  260 XPARAM(I)=C1*GROLD(I)+C2*XOLD(I)
      STEP=0.D0
      CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.,*9999)     CSTP(call)
      CALL WRITMO(TIME0,FUNCT1,*9999)                                   CSTP(call)
      TIM=SECOND()-TIME0
      IF (DOPRNT) THEN                                                  CIO
       WRITE(6,'(///,'' TOTAL CPU TIME: '',F16.2,'' SECONDS'')') TIM
       WRITE(6,'(/,'' == MOPAC DONE =='')')
      ENDIF                                                             CIO
c     call timend
      return
  270 RETURN 1                                                          CSTP (stop)

 9999 RETURN 1                                                          CSTP
      ENTRY REACT1_INIT                                                 CSAV
                C1 = 0.0D0                                              CSAV
                C2 = 0.0D0                                              CSAV
                CA = 0.0D0                                              CSAV
             CONST = 0.0D0                                              CSAV
             COORD = 0.0D0                                              CSAV
              DELL = 0.0D0                                              CSAV
              EOLD = 0.0D0                                              CSAV
            FINISH = .FALSE.                                            CSAV
            FUNCT1 = 0.0D0                                              CSAV
               GOK = .FALSE.                                            CSAV
              GOLD = 0.0D0                                              CSAV
             GROLD = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
             IDUM1 = 0                                                  CSAV
             IDUM2 = 0                                                  CSAV
             IDUM3 = 0                                                  CSAV
             IDUM4 = 0                                                  CSAV
            IDUMMY = 0.0D0                                              CSAV
             ILOOP = 0                                                  CSAV
               INT = .FALSE.                                            CSAV
                IR = 0                                                  CSAV
                 J = 0                                                  CSAV
                JR = 0                                                  CSAV
                 K = 0                                                  CSAV
                 L = 0                                                  CSAV
            LINEAR = 0                                                  CSAV
            MAXSTP = 0                                                  CSAV
            NUMAT2 = 0                                                  CSAV
               ONE = 0.0D0                                              CSAV
                SA = 0.0D0                                              CSAV
             STEP0 = 0.0D0                                              CSAV
            STEPMX = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
              SUMM = 0.0D0                                              CSAV
              SUMX = 0.0D0                                              CSAV
              SUMY = 0.0D0                                              CSAV
              SUMZ = 0.0D0                                              CSAV
              SWAP = 0.0D0                                              CSAV
               TIM = 0.0D0                                              CSAV
             TIME0 = 0.0D0                                              CSAV
             TIME1 = 0.0D0                                              CSAV
             TIME2 = 0.0D0                                              CSAV
                 X = 0.0D0                                              CSAV
            XSTORE = 0.0D0                                              CSAV
               XYZ = .FALSE.                                            CSAV
      RETURN                                                            CSAV
      END
