      SUBROUTINE FMAT(FMATRX, TSCF, TDER, DELDIP, HEAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
***********************************************************************
*
*  VALUE CALCULATES THE SECOND-ORDER OF THE ENERGY WITH
*        RESPECT TO THE CARTESIAN COORDINATES I AND J AND PLACES IT
*        IN FMATRX
*
*  ON INPUT NATOMS  = NUMBER OF ATOMS IN THE SYSTEM.
*           XPARAM  = INTERNAL COORDINATES OF MOLECULE STORED LINEARLY
*
*  VARIABLES USED
*           COORDL  = ARRAY OF CARTESIAN COORDINATES, STORED LINEARLY.
*           I       = INDEX OF CARTESIAN COORDINATE.
*           J       = INDEX OF CARTESIAN COORDINATE.
*
*  ON OUTPUT FMATRX = SECOND DERIVATIVE OF THE ENERGY WITH RESPECT TO
*                    CARTESIAN COORDINATES I AND J.
***********************************************************************
      DIMENSION FMATRX(*), DELDIP(3,*)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY, DUMY(MAXPAR)
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY             3GL3092
      COMMON /DENSTY/ P(MPACK),PDUMY(2,MPACK)
C     COMMON /TIME  / TIME0
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /CORE  / CORE(107)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCR / FDUMMY(MAXHES),STORE(MAXPAR**2),                 3GL3092
     1                 EVECS(MAXPAR*MAXPAR),COLD(MAXPAR),GRAD(MAXPAR),  3GL3092
     2                 GROLD(MAXPAR),Q(NUMATM),DEL2(3),                 3GL3092
     3                 EIGS(MAXPAR),FCONST(MAXPAR),                     3GL3092
     4                 RDUMY(MAXPAR*(3*MAXPAR + NCHAIN + 16) + 29 +     3GL3092
     5                       NCHAIN - MAXPAR*(2*MAXPAR+5) - NUMATM -3)  3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,FDUMMY(MAXHES),STORE(MAXPAR**2)
C    1               ,EVECS(MAXPAR*MAXPAR),COLD(MAXPAR),GRAD(MAXPAR)
C    2               ,GROLD(MAXPAR),Q(NUMATM),DEL2(3)
C    3               ,EIGS(MAXPAR),FCONST(MAXPAR)
      COMMON /CMCOM/  ECMCG(NUMATM)                                     GDH1293
      COMMON /OPTCOM/ LDER                                              GDH1093
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      DIMENSION COORDL(MAXPAR)
      LOGICAL DEBUG, DERIV, RESTRT,FAIL, LDER , WAVDIP                  DJG0896
      CHARACTER SPACE*1, DOT*1, ZERO*1, NINE*1, CH*1
      EQUIVALENCE (COORD(1,1),COORDL(1))
       SAVE
      DATA SPACE,DOT,ZERO,NINE /' ','.','0','9'/
      DATA FACT/6.95125D-3/
C
C    FACT IS THE CONVERSION FACTOR FROM KCAL/MOLE TO ERGS
C
C SET UP CONSTANTS AND FLAGS
      NA(1)=99
C
C  SET UP THE VARIABLES IN XPARAM ANDLOC,THESE ARE IN CARTESIAN COORDINA
C
      NUMAT=0
      DO 10 I=1,NATOMS
         IF(LABELS(I).NE.99.AND.LABELS(I).NE.107) THEN
            NUMAT=NUMAT+1
            LABELS(NUMAT)=LABELS(I)
         ENDIF
   10 CONTINUE
      NATOMS=NUMAT
C
C   THIS IS A QUICK, IF CLUMSY, WAY TO CALCULATE NUMAT, AND TO REMOVE TH
C   DUMMY ATOMS FROM THE ARRAY LABELS.
C
      NVAR=0
      DO 20 I=1,NUMAT
         DO 20 J=1,3
            NVAR=NVAR+1
            LOC(1,NVAR)=I
            LOC(2,NVAR)=J
   20 CONTINUE
      LIN=(NVAR*(NVAR+1))/2
      DO 30 I=1,LIN
   30 FMATRX(I)=0.D0
      RESTRT =(IRESTA.NE.0)                                             DJG0995
      IF(INLLSQ.NE.0) RESTRT=.FALSE.                                    DJG0995
      DEBUG=IFMAT.NE.0                                                  DJG0995
      DERIV=LDER                                                        GDH0993
      WRITE(JOUT,'(//4X,''FIRST DERIVATIVES WILL BE USED IN THE''
     1,'' CALCULATION OF SECOND DERIVATIVES'')')
C     TIME=3600
      TIME=TDEF                                                         GL0492
      IF(ITLIMI.NE.0) THEN                                              DJG0995
         TIME=FITLIM                                                    DJG0995
         WRITE(JOUT,'(/10X,''TIME DEFINED FOR THIS STEP ='',F19.2,
     1    '' SECONDS'')')TIME
      ELSE
         WRITE(JOUT,'(/10X,''DEFAULT TIME OF'',F8.2,
     1    '' SECONDS ALLOCATED FOR THIS STEP'')')TIME
      ENDIF
      CALL PORCPU (TFLY)                                                GL0492
      TLEFT=TIME-TFLY+TIME0
      IF(RESTRT) THEN
         DO 60 I=1,NVAR
   60    COLD(I)=COORDL(I)
         ISTART = 0
         I=0
         CALL FORSAV(TOTIME,DELDIP,ISTART,I,FMATRX, COORD, NVAR,HEAT,
     1                EVECS,JSTART,FCONST)
         KOUNTF=(ISTART*(ISTART+1))/2
         ISTART=ISTART+1
         JSTART=JSTART+1
         CALL PORCPU (TIME2)                                            GL0492
         IF(ISTART.GT.NVAR) GOTO 160
      ELSE
         KOUNTF=0
         TOTIME=0.D0
         IF (TSCF.GT.0.D0)TLEFT=TLEFT-TSCF-TDER
         ISTART=1
      ENDIF
      CONST =60
      DELTA =0.5D0/CONST
C CALCULATE FMATRX
      IF(ISTART.GT.1) THEN
         ESTIME=(NVAR-ISTART+1)*TOTIME/(ISTART-1.D0)
      ELSE
         ESTIME=NVAR*(TSCF+TDER)*2.D0
      ENDIF
      IF(TSCF.GT.0)
     1WRITE(JOUT,'(/10X,''ESTIMATED TIME TO COMPLETE CALCULATION =''
     2,F9.2,'' SECONDS'')')ESTIME
      IF(RESTRT) THEN
         IF(ISTART.LE.NVAR)
     1    WRITE(JOUT,'(/10X,''STARTING AGAIN AT LINE'',18X,I4)')ISTART
         WRITE(JOUT,'(/10X,''TIME USED UP TO RESTART ='',F22.2)')TOTIME
      ENDIF
      LU=KOUNTF
      CALL PORCPU (TIME1)                                               GL0492
      NUMAT=NVAR/3
      DO 120 I=ISTART,NVAR
         CALL PORCPU (TIME2)                                            GL0492
         COORDL(I)=COORDL(I)+DELTA*0.5
         GROLD(1)=100.D0
         CALL COMPFG(COORDL,ESCF,FAIL,GROLD,.TRUE.)
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=6                                                       GDH1095
         RETURN                                                         GDH1095
         ENDIF                                                          GDH1095
         IF (IEXTCM.NE.0) THEN                                          DJG0995
C          WAVDIP TURNS ON OR OFF SOME PRINTING IN DIPOLE               DJG0896
           WAVDIP=.FALSE.                                               DJG0896
           DO 71 II=1,NUMAT
   71      Q(II)=ECMCG(II)                                              GDH1293
           WRITE(JOUT,'(/,''For the charges entered with EXTCM:'')')    GDH1293
           SUM = DIPOLE(P,Q,COORDL,DELDIP(1,I),WAVDIP)                  DJG0896
         ENDIF                                                          GDH1293
         IF (ICHMOD.EQ.1) THEN                                          DJG1094
           WAVDIP=.FALSE.                                               DJG0896
           CALL CHGMP1(P,Q)                                             DJG1094
         ELSE IF (ICHMOD.EQ.2) THEN                                     GDH0997
           WAVDIP=.FALSE.                                               GDH0997
           CALL CHGMP2(P,Q)                                             GDH0997
         ELSE                                                           DJG1094
           WAVDIP=.TRUE.                                                DJG0896
           CALL CHRGE (P,Q)
           DO 70 II=1,NUMAT                                             DL0397
   70      Q(II)=CORE(LABELS(II))-Q(II)                                 DL0397
         ENDIF                                                          DL0397
         SUM = DIPOLE(P,Q,COORDL,DELDIP(1,I),WAVDIP)                    DJG0896
         COORDL(I)=COORDL(I)-DELTA
         GRAD(1)=100.D0
         CALL COMPFG(COORDL,HEATAA,FAIL,GRAD,.TRUE.)
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=6                                                       GDH1095
         RETURN                                                         GDH1095
         ENDIF                                                          GDH1095
         IF (IEXTCM.NE.0) THEN                                          DJG0995
           WAVDIP=.FALSE.                                               DJG0896
           DO 79 II=1,NUMAT
   79      Q(II)=ECMCG(II)                                              GDH1293
           WRITE(JOUT,'(/,''For the charges entered with EXTCM:'')')    GDH1293
           SUM = DIPOLE(P,Q,COORDL,DEL2,WAVDIP)                         DJG0896
         ENDIF                                                          GDH1293
         IF (ICHMOD.EQ.1) THEN                                          DJG1094
           WAVDIP=.FALSE.                                               DJG0896
           CALL CHGMP1(P,Q)                                             DJG1094
         ELSE IF (ICHMOD.EQ.2) THEN                                     GDH0997
           WAVDIP=.FALSE.                                               GDH0997
           CALL CHGMP2(P,Q)                                             GDH0997
         ELSE                                                           DJG1094
           WAVDIP=.TRUE.                                                DJG0896
           CALL CHRGE (P,Q)
           DO 80 II=1,NUMAT                                             DL0397
   80      Q(II)=CORE(LABELS(II))-Q(II)                                 DL0397
         ENDIF                                                          DL0397
         SUM = DIPOLE(P,Q,COORDL,DEL2,WAVDIP)                           DJG0896
         DO 90 II=1,3
   90    DELDIP(II,I)=(DELDIP(II,I)-DEL2(II))*CONST
         LL=LU+1
         LU=LL+I-1
         L=0
         DO 100 KOUNTF=LL,LU
            L=L+1
            FMATRX(KOUNTF)=FMATRX(KOUNTF)+
     1         ((GROLD(L)-GRAD(L)))
     2          *CONST*FACT*0.5D0
  100    CONTINUE
         L=L-1
         DO 110 K=I,NVAR
            L=L+1
            KK=(K*(K-1))/2+I
            FMATRX(KK)=FMATRX(KK)+
     1         ((GROLD(L)-GRAD(L)))
     2          *CONST*FACT*0.5D0
  110    CONTINUE
         COORDL(I)=COORDL(I)+DELTA*0.5D0
         CALL PORCPU (TIME3)                                            GL0492
         TSTEP=TIME3-TIME2
         TOTIME= TOTIME+TSTEP
         TLEFT= TLEFT-TSTEP
         WRITE(JOUT,'('' STEP:'',I4,'' TIME ='',F9.2,'' SECS, INTEGRAL =
     1          '',F10.2,'' TIME LEFT:'',F10.2)')I,TSTEP,TOTIME,TLEFT
         IF(DERIV) THEN
            ESTIM = TOTIME/I
         ELSE
            ESTIM = TOTIME*2.D0/I
         ENDIF
         IF(I.NE.NVAR.AND.TLEFT-10.D0 .LT. ESTIM) THEN
            WRITE(JOUT,'(//10X,''- - - - -  TIME  LIMIT - - - - -'')')
            WRITE(JOUT,'(/10X,'' POINT REACHED ='',I4)')I
            WRITE(JOUT,'(/10X,'' RESTART USING KEY-WORD "RESTART"'')')
            WRITE(JOUT,'(10X,''ESTIMATED TIME FOR THE NEXT STEP ='',
     1            F8.2,'' SECONDS'')')ESTIM
            JSTART=1
            II=I
            CALL FORSAV(TOTIME,DELDIP,II,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,JSTART,FCONST)
         ENDIF
  120 CONTINUE
C#      CALL FORSAV(TOTIME,DELDIP,NVAR,NVAR,FMATRX, COORD,NVAR,HEAT,
C#     +                EVECS,JSTART,FCONST)
      IF(DERIV) GOTO 250
      WRITE(JOUT,'(//10X,'' STARTING TO CALCULATE FORCE CONSTANTS'',/)')
      CALL FRAME(FMATRX,NUMAT,0,EIGS)
      CALL HQRII(FMATRX,NVAR,NVAR,EIGS,EVECS)
      IF(DEBUG) THEN
         WRITE(JOUT,'(''   EIGENVECTORS FROM FIRST CALCULATION'')')
         CALL MATOUT(EVECS,EIGS,NVAR,NVAR,NVAR)
      ENDIF
      L=0
      NREAL=NVAR-6
      DO 140 I=1,NVAR
         DO 140 J=1,I
            L=L+1
            SUM=0.D0
            DO 130 K=1,NREAL
               K1=(K-1)*NVAR+I
               K2=(K-1)*NVAR+J
  130       SUM=SUM+EVECS(K1)*EIGS(K)*EVECS(K2)
  140 FMATRX(L)=SUM
      CALL FRAME(FMATRX,NUMAT,0,EIGS)
      CALL HQRII(FMATRX,NVAR,NVAR,EIGS,EVECS)
      JSTART=1
      DO 150 I=1,NVAR
  150 COLD(I)=COORDL(I)
  160 IF(DERIV) GOTO 250
      DELTA=0.025D0
      L=(JSTART-1)*NVAR
      DO 210 ILOOP=JSTART,NVAR
         J=L
         DO 170 I=1,NVAR
            J=J+1
  170    COORDL(I)=COLD(I)+EVECS(J)*DELTA
         CALL COMPFG(COORDL,HEATA,FAIL,GRAD,.FALSE.)
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=7                                                       GDH1095
         RETURN                                                         GDH1095
         ENDIF                                                          GDH1095
         HEATA=HEATA-HEAT
         J=L
         DO 180 I=1,NVAR
            J=J+1
  180    COORDL(I)=COLD(I)-EVECS(J)*DELTA
         CALL COMPFG(COORDL,HEATB,FAIL,GRAD,.FALSE.)
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=8                                                       GDH1095
         RETURN                                                         GDH1095
         ENDIF                                                          GDH1095
         HEATB=HEATB-HEAT
         J=L
         DO 190 I=1,NVAR
            J=J+1
  190    COORDL(I)=COLD(I)+EVECS(J)*DELTA*2
         CALL COMPFG(COORDL,HEATAA,FAIL,GRAD,.FALSE.)
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=9                                                       GDH1095
         RETURN                                                         GDH1095
         ENDIF                                                          GDH1095
         HEATAA=HEATAA-HEAT
         J=L
         DO 200 I=1,NVAR
            J=J+1
  200    COORDL(I)=COLD(I)-EVECS(J)*DELTA*2
         CALL COMPFG(COORDL,HEATBB,FAIL,GRAD,.FALSE.)
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=10                                                      GDH1095
         RETURN                                                         GDH1095
         ENDIF                                                          GDH1095
         HEATBB=HEATBB-HEAT
         SUM=( (HEATA+HEATB)*16 - (HEATAA+HEATBB) )/12.D0
     1/DELTA*FACT/DELTA*0.5D0
         FCONST(ILOOP)=SUM*0.5D0
         L=L+NVAR
         CALL PORCPU (TIME3)                                            GL0492
         TSTEP=TIME3-TIME2
         TIME2=TIME3
         TOTIME= TOTIME+TSTEP
         TLEFT= TLEFT-TSTEP
         WRITE(JOUT,'('' STEP:'',I4,'' TIME ='',F9.2,'' SECS, INTEGRAL =
     1'',F10.2,'' TIME LEFT:'',F10.2)')ILOOP,TSTEP,TOTIME,TLEFT
         ESTIM = TSTEP*5.D0
C
C    5.0 IS A SAFETY FACTOR
C
         IF(ILOOP.NE.NVAR.AND.TLEFT-10.D0 .LT. ESTIM) THEN
            WRITE(JOUT,'(//10X,''- - - - -  TIME  LIMIT - - - - -'')')
            WRITE(JOUT,'(/10X,'' POINT REACHED ='',I4)')ILOOP
            WRITE(JOUT,'(/10X,'' RESTART USING KEY-WORD "RESTART"'')')
            WRITE(JOUT,'(10X,''ESTIMATED TIME FOR THE NEXT STEP ='',
     1F8.2,'' SECONDS'')')ESTIM
            IFOR=ILOOP
            IX=NVAR+2
*
* VALUE OF IX IS NOT IMPORTANT. SHOULD NOT BE 0 OR NVAR
*
            CALL FORSAV(TOTIME,DELDIP,IX,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,IFOR,FCONST)
         ENDIF
  210 CONTINUE
      L=0
      DO 230 I=1,NVAR
         DO 230 J=1,I
            L=L+1
            SUM=0.D0
            DO 220 K=1,NVAR
               K1=(K-1)*NVAR+I
               K2=(K-1)*NVAR+J
  220       SUM=SUM+EVECS(K1)*FCONST(K)*EVECS(K2)
  230 FMATRX(L)=SUM*2.D0
      DO 240 I=1,NVAR
  240 COORDL(I)=COLD(I)
  250 CONTINUE
      IF(ISTART.LE.NVAR.AND.IISOTO.NE.0)                                DJG0995
     1CALL FORSAV(TOTIME,DELDIP,NVAR,NVAR,FMATRX, COORD,NVAR,HEAT,
     2                EVECS,ILOOP,FCONST)
      RETURN
      END
