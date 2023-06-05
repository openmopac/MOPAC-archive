      SUBROUTINE FMAT(FMATRX, NREAL, TSCF, TDER, DELDIP, HEAT,*)        CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION FMATRX(*), DELDIP(3,*),a(30,30)
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
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR)                 IR0394
      COMMON /DENSTY/ P(MPACK),PDUMY(2,MPACK)
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /TIMER / TIME0
      COMMON /CORE  / CORE(120)
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /SCRACH/ EVECS(MAXPAR*MAXPAR)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION COLD(MAXPAR), GRAD(MAXPAR),
     1GROLD(MAXPAR), COORDL(MAXPAR), Q(NUMATM), DEL2(3), G2OLD(MAXPAR)
     2, EIGS(MAXPAR), G2RAD(MAXPAR),
     3 FCONST(MAXPAR)
      CHARACTER*160 KEYWRD
      LOGICAL DEBUG, DERIV, RESTRT, PRNT, RESFIL, ANALYT, PRECIS
      CHARACTER SPACE*1, DOTT*1, ZERO*1, NINE*1, CH*1
      EQUIVALENCE (COORD(1,1),COORDL(1))
CSAV         SAVE                                                           GL0892
      DATA SPACE,DOTT,ZERO,NINE /' ','.','0','9'/
      DATA FACT/6.95125D-3/
      data angs/0.529177D0/
C
C    FACT IS THE CONVERSION FACTOR FROM KCAL/MOLE TO ERGS
C
C SET UP CONSTANTS AND FLAGS
      NA(1)=99
      KOUNTF=0
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
      PRNT   =(INDEX(KEYWRD,'IRC=') .EQ. 0)
      PRECIS =(INDEX(KEYWRD,'PRECIS') .NE. 0)
      ANALYT =((INDEX(KEYWRD,'ANALYT') .NE. 0).or.
     &         (INDEX(KEYWRD,'PMOV1')) .NE. 0)                          LF1010
      RESTRT =(INDEX(KEYWRD,'RESTART') .NE. 0)
      IF(INDEX(KEYWRD,'NLLSQ') .NE. 0) RESTRT=.FALSE.
      DEBUG =(INDEX(KEYWRD,'FMAT') .NE. 0)
      DERIV=(NCLOSE .EQ. NOPEN .AND. INDEX(KEYWRD,'C.I.') .EQ. 0)
      IF(DOPRNT.AND.PRNT)                                               CIO
     &    WRITE(6,'(//4X,''FIRST DERIVATIVES WILL BE USED IN THE''      CIO
     &          ,'' CALCULATION OF SECOND DERIVATIVES'')')              CIO
      TIME=MAXTIM
      I=INDEX(KEYWRD,' T=')
      IF(I.NE.0) THEN
         TIM=READA(KEYWRD,I)
         DO 40 J=I+3,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') TIM=TIM*60
               IF( CH .EQ. 'H') TIM=TIM*3600
               IF( CH .EQ. 'D') TIM=TIM*86400
               GOTO 50
            ENDIF
   40    CONTINUE
   50    TIME=TIM
         IF(DOPRNT.AND.PRNT)                                            CIO
     &        WRITE(6,'(/10X,''TIME DEFINED FOR THIS STEP ='',F19.2,    CIO
     &              '' SECONDS'')')TIME                                 CIO
      ELSE
         IF(DOPRNT.AND.PRNT)WRITE(6,'(/10X,''DEFAULT TIME OF'',F8.2,    CIO
     1    '' SECONDS ALLOCATED FOR THIS STEP'')')TIME                   CIO
      ENDIF
      TLEFT=TIME
      TLAST=TIME
      TDUMP=MAXDMP
      I=INDEX(KEYWRD,' DUMP')
      IF(I.NE.0) THEN
         TDUMP=READA(KEYWRD,I)
         DO 60 J=I+6,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') TDUMP=TDUMP*60
               IF( CH .EQ. 'H') TDUMP=TDUMP*3600
               IF( CH .EQ. 'D') TDUMP=TDUMP*86400
               GOTO 70
            ENDIF
   60    CONTINUE
   70    CONTINUE
      ENDIF
      RESFIL=.FALSE.
      IF(RESTRT) THEN
         DO 80 I=1,NVAR
   80    COLD(I)=COORDL(I)
         ISTART = 0
         I=0
         CALL FORSAV(TOTIME,DELDIP,ISTART,I,FMATRX, COORD, NVAR,HEAT,
     1                EVECS,JSTART,FCONST,*9999)                         CSTP(call)
         KOUNTF=(ISTART*(ISTART+1))/2
         ISTART=ISTART+1
         JSTART=JSTART+1
         TIME2 = SECOND()
         IF(ISTART.GT.NVAR) GOTO 200
      ELSE
         TOTIME=0.D0
         IF (TSCF.GT.0.D0)TLEFT=TLEFT-TSCF-TDER
         ISTART=1
      ENDIF
C CALCULATE FMATRX
      IF(ISTART.GT.1) THEN
         ESTIME=(NVAR-ISTART+1)*TOTIME/(ISTART-1.D0)
      ELSE
         ESTIME=NVAR*(TSCF+TDER)*2.D0
         IF (PRECIS) ESTIME=ESTIME*2.D0
      ENDIF
      IF(DOPRNT.AND.(TSCF.GT.0))                                        CIO
     1WRITE(6,'(/10X,''ESTIMATED TIME TO COMPLETE CALCULATION =''
     2,F9.2,'' SECONDS'')')ESTIME
      IF(RESTRT) THEN
        IF (DOPRNT) THEN                                                CIO
         IF(ISTART.LE.NVAR)
     1    WRITE(6,'(/10X,''STARTING AGAIN AT LINE'',18X,I4)')ISTART
         WRITE(6,'(/10X,''TIME USED UP TO RESTART ='',F22.2)')TOTIME
        ENDIF                                                           CIO
      ENDIF
      LU=KOUNTF
      TIME1 = SECOND()
      NUMAT=NVAR/3
      DO 160 I=ISTART,NVAR
         TIME2 = SECOND()
         DELTA = 1.D0/60.D0
         IF(PRECIS)THEN
C
C   DETERMINE A GOOD STEP SIZE
C
            G2OLD(1)=100.D0
            COORDL(I)=COORDL(I)+DELTA
            CALL COMPFG(COORDL, .TRUE., ESCF, .TRUE., G2OLD, .TRUE.,     CSTP(call)
     & *9999)                                                           CSTP(call)
            COORDL(I)=COORDL(I)-DELTA
            DELTA=DELTA*10.D0/SQRT(ddot(nvar,g2old,1,g2old,1))
C#         WRITE(6,'(A,F12.5)')' DELTA :',DELTA
            G2OLD(1)=100.D0
            COORDL(I)=COORDL(I)+DELTA
            CALL COMPFG(COORDL, .TRUE., ESCF, .TRUE., G2OLD, .TRUE.,     CSTP(call)
     & *9999)                                                           CSTP(call)
C#         WRITE(6,*)' GNORM:',SQRT(ddot(nvar,g2old,1,g2old,1))
            COORDL(I)=COORDL(I)-DELTA*2.D0
            G2RAD(1)=100.D0
            CALL COMPFG(COORDL, .TRUE., HEATAA, .TRUE., G2RAD, .TRUE.,   CSTP(call)
     & *9999)                                                           CSTP(call)
            COORDL(I)=COORDL(I)+DELTA
         ENDIF
         COORDL(I)=COORDL(I)+0.5D0*DELTA
         GROLD(1)=100.D0
         CALL COMPFG(COORDL, .TRUE., ESCF, .TRUE., GROLD, .TRUE.,*9999)  CSTP(call)
C#         WRITE(6,*)' GNORM:',SQRT(ddot(nvar,GROLD,1,GROLD,1))
         CALL CHRGE(P,Q,*9999)                                           CSTP(call)
         DO 90 II=1,NUMAT
   90    Q(II)=CORE(LABELS(II))-Q(II)
         SUM = DIPOLE(P,Q,COORDL,DELDIP(1,I))
         COORDL(I)=COORDL(I)-DELTA
         GRAD(1)=100.D0
         CALL COMPFG(COORDL, .TRUE., HEATAA, .TRUE., GRAD, .TRUE.,*9999) CSTP(call)
         COORDL(I)=COORDL(I)+DELTA*0.5D0
         CALL CHRGE(P,Q,*9999)                                           CSTP(call)
         DO 100 II=1,NUMAT
  100    Q(II)=CORE(LABELS(II))-Q(II)
         SUM = DIPOLE(P,Q,COORDL,DEL2)
         DO 110 II=1,3
  110    DELDIP(II,I)=(DELDIP(II,I)-DEL2(II))*0.5D0/DELTA
         LL=LU+1
         LU=LL+I-1
         L=0
c*AG     write(6,*) kountf,i,istart
         IF(PRECIS)THEN
            DO 120 KOUNTF=LL,LU
               L=L+1
               FMATRX(KOUNTF)=FMATRX(KOUNTF)+
     1         (8.D0*(GROLD(L)-GRAD(L))-(G2OLD(L)-G2RAD(L)))
     2          *0.25D0/DELTA*FACT/6.D0
c*AG        if (kountf .eq. 42 .or. kountf .eq. 45) then
c*AG        write(6,*)i,kountf,fmatrx(kountf)*2/fact
c*AG        write(6,*)l,grold(l),grad(l)
c*AG        write(6,*)l,g2old(l),g2rad(l)
c*AG        write (6,*) delta
c*AG        end if
  120       CONTINUE
            L=L-1
            DO 130 K=I,NVAR
               L=L+1
               KK=(K*(K-1))/2+I
               FMATRX(KK)=FMATRX(KK)+
     1         (8.D0*(GROLD(L)-GRAD(L))-(G2OLD(L)-G2RAD(L)))
     2          *0.25D0/DELTA*FACT/6.D0
c*AG        if (kk .eq. 42 .or. kk .eq. 45) then
c*AG        write(6,*) i,kk,fmatrx(kk)*2/fact
c*AG        write(6,*)l,grold(l),grad(l)
c*AG        write(6,*)l,g2old(l),g2rad(l)
c*AG        write (6,*) delta
c*AG        end if
  130       CONTINUE
         ELSE
            DO 140 KOUNTF=LL,LU
               L=L+1
               FMATRX(KOUNTF)=FMATRX(KOUNTF)+
     1         ((GROLD(L)-GRAD(L)))
     2          *0.25D0/DELTA*FACT
  140       CONTINUE
            L=L-1
            DO 150 K=I,NVAR
               L=L+1
               KK=(K*(K-1))/2+I
               FMATRX(KK)=FMATRX(KK)+
     1         ((GROLD(L)-GRAD(L)))
     2          *0.25D0/DELTA*FACT
  150       CONTINUE
         ENDIF
         TIME3 = SECOND()
         TSTEP=TIME3-TIME2
         TOTIME= TOTIME+TSTEP
         TLEFT= TLEFT-TSTEP
         IF(RESFIL)THEN
            IF (DOPRNT)                                                 CIO
     &WRITE(6,'('' STEP:'',I4,'' RESTART FILE WRITTEN, INTEGRAL ='',    CIO
     &F10.2,'' TIME LEFT:'',F10.2)')I,TOTIME,TLEFT                      CIO
            RESFIL=.FALSE.
         ELSE
            IF (DOPRNT)                                                 CIO
     &WRITE(6,'('' STEP:'',I4,'' TIME ='',F9.2,'' SECS, INTEGRAL ='',   CIO
     &F10.2,'' TIME LEFT:'',F10.2)')I,TSTEP,TOTIME,TLEFT                CIO
         ENDIF
         IF(DERIV) THEN
            ESTIM = TOTIME/I
         ELSE
            ESTIM = TOTIME*2.D0/I
         ENDIF
         IF(TLAST-TLEFT.GT.TDUMP)THEN
            TLAST=TLEFT
            RESFIL=.TRUE.
            JSTART=1
            II=I
            CALL FORSAV(TOTIME,DELDIP,II,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,JSTART,FCONST,*9999)                        CSTP(call)
         ENDIF
         IF(I.NE.NVAR.AND.TLEFT-10.D0 .LT. ESTIM) THEN
           IF (DOPRNT) THEN                                             CIO
            WRITE(6,'(//10X,''- - - - - - - TIME UP - - - - - - -'',//)'
     1)
            WRITE(6,'(/10X,'' POINT REACHED ='',I4)')I
            WRITE(6,'(/10X,'' RESTART USING KEY-WORD "RESTART"'')')
            WRITE(6,'(10X,''ESTIMATED TIME FOR THE NEXT STEP ='',F8.2,
     1'' SECONDS'')')ESTIM
           ENDIF                                                        CIO
            JSTART=1
            II=I
            CALL FORSAV(TOTIME,DELDIP,II,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,JSTART,FCONST,*9999)                        CSTP(call)
            IF(DOPRNT)                                                  CIO
     &            WRITE(6,'(//10X,''FORCE MATRIX WRITTEN TO DISK'')')   CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
  160 CONTINUE
c*AG     l = 1
c*AG     do 111 i=1,nvar
c*AG      do 112 k=1,i
c*AG       a(i,k) = fmatrx(l)
c*AG       l = l + 1
c*AG112   continue
c*AG     write(6,113) (a(i,k)*2/fact,k=1,i)
c*AG111 continue
c*AG113  format(/,15F8.3)
C#      CALL FORSAV(TOTIME,DELDIP,NVAR,NVAR,FMATRX, COORD,NVAR,HEAT,
C#     +                EVECS,JSTART,FCONST)
      IF(DERIV) GOTO 290
      IF (DOPRNT)                                                       CIO
     & WRITE(6,'(//10X,'' STARTING TO CALCULATE FORCE CONSTANTS'',/)')  CIO
      CALL FRAME(FMATRX,NUMAT,0,EIGS,*9999)                              CSTP(call)
      CALL RSP(FMATRX,NVAR,NVAR,EIGS,EVECS,*9999)                        CSTP(call)
      IF(DEBUG) THEN
         IF (DOPRNT)                                                    CIO
     & WRITE(6,'(''   EIGENVECTORS FROM FIRST CALCULATION'')')          CIO
         CALL MATOUT(EVECS,EIGS,NVAR,NVAR,NVAR,*9999)                    CSTP(call)
      ENDIF
      L=0
      DO 180 I=1,NVAR
         DO 180 J=1,I
            L=L+1
            SUM=0.D0
            DO 170 K=1,NREAL
               K1=(K-1)*NVAR+I
               K2=(K-1)*NVAR+J
  170       SUM=SUM+EVECS(K1)*EIGS(K)*EVECS(K2)
  180 FMATRX(L)=SUM
      CALL FRAME(FMATRX,NUMAT,0,EIGS,*9999)                              CSTP(call)
      CALL RSP(FMATRX,NVAR,NVAR,EIGS,EVECS,*9999)                        CSTP(call)
C     CALL MATOUT(EVECS,EIGS,NVAR,NVAR,NVAR)
      JSTART=1
      DO 190 I=1,NVAR
  190 COLD(I)=COORDL(I)
  200 IF(DERIV) GOTO 290
      L=(JSTART-1)*NVAR
      DO 250 ILOOP=JSTART,NVAR
C
C   MAKE THE STEP-SIZE ROUGHLY INVERSELY PROPORTIONAL TO THE EIGENVALUE
C
         IF(ILOOP.LE.NREAL)THEN
            DELTA=MAX(0.02D0,MIN(0.2D0,0.25D0/ABS(EIGS(ILOOP))))
         ELSEIF(ILOOP.LE.NREAL+3)THEN
            DELTA=0.2D0
         ELSE
            DELTA=0.1D0
         ENDIF
C#      WRITE(6,*)'DELTA:',DELTA
         J=L
         CALL COMPFG(COLD, .TRUE., HEAT, .TRUE., GRAD, .FALSE.,*9999)    CSTP(call)
         DO 210 I=1,NVAR
            J=J+1
  210    COORDL(I)=COLD(I)+EVECS(J)*DELTA
         CALL COMPFG(COORDL, .TRUE., HEATA, .TRUE., GRAD, .FALSE.,*9999) CSTP(call)
         HEATA=HEATA-HEAT
         J=L
         DO 220 I=1,NVAR
            J=J+1
  220    COORDL(I)=COLD(I)-EVECS(J)*DELTA
         CALL COMPFG(COORDL, .TRUE., HEATB, .TRUE., GRAD, .FALSE.,*9999) CSTP(call)
         HEATB=HEATB-HEAT
         J=L
         DO 230 I=1,NVAR
            J=J+1
  230    COORDL(I)=COLD(I)+EVECS(J)*DELTA*2
         CALL COMPFG(COORDL, .TRUE., HEATAA, .TRUE.,GRAD, .FALSE.,*9999) CSTP(call)
         HEATAA=HEATAA-HEAT
         J=L
         DO 240 I=1,NVAR
            J=J+1
  240    COORDL(I)=COLD(I)-EVECS(J)*DELTA*2
         CALL COMPFG(COORDL, .TRUE., HEATBB, .TRUE.,GRAD, .FALSE.,*9999) CSTP(call)
         HEATBB=HEATBB-HEAT
         SUM=( (HEATA+HEATB)*16 - (HEATAA+HEATBB) )/12.D0
     1/DELTA*FACT/DELTA*0.5D0
C#      WRITE(6,'(5F12.6)')HEATAA+HEAT,HEATA+HEAT,HEAT,HEATB+HEAT,
C#     +HEATBB+HEAT
C#      WRITE(6,'(5F12.6)')HEATAA,HEATA,0.D0,HEATB,HEATBB
         FCONST(ILOOP)=SUM*0.5D0
         L=L+NVAR
         TIME3 = SECOND()
         TSTEP=TIME3-TIME2
         IF(TSTEP.GT.1.D5)THEN
            TSTEP=TSTEP-1000000.D0
            TIME3=TIME3-1000000.D0
            TLEFT=-1.D0
         ENDIF
         TIME2=TIME3
         TOTIME= TOTIME+TSTEP
         TLEFT= MAX(-1.D0,TLEFT-TSTEP)
         IF (DOPRNT)                                                      CIO
     &   WRITE(6,'('' STEP:'',I4,'' TIME ='',F9.2,'' SECS, INTEGRAL ='',  CIO
     &   F10.2,'' TIME LEFT:'',F10.2)')ILOOP,TSTEP,TOTIME,TLEFT           CIO
         ESTIM = TSTEP*5.D0
C
C    5.0 IS A SAFETY FACTOR
C
         IF(TLAST-TLEFT.GT.TDUMP)THEN
            TLAST=TLEFT
            RESFIL=.TRUE.
            IFOR=ILOOP
            IX=NVAR+2
*
* VALUE OF IX IS NOT IMPORTANT. SHOULD NOT BE 0 OR NVAR
*
            CALL FORSAV(TOTIME,DELDIP,IX,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,IFOR,FCONST,*9999)                           CSTP(call)
         ENDIF
         IF(ILOOP.NE.NVAR.AND.TLEFT-10.D0 .LT. ESTIM) THEN
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//10X,''- - - - -  TIME  LIMIT - - - - -'')')
            WRITE(6,'(/10X,'' POINT REACHED ='',I4)')ILOOP
            WRITE(6,'(/10X,'' RESTART USING KEY-WORD "RESTART"'')')
            WRITE(6,'(10X,''ESTIMATED TIME FOR THE NEXT STEP ='',F8.2,
     1'' SECONDS'')')ESTIM
            ENDIF                                                       CIO
            IFOR=ILOOP
            IX=NVAR+2
*
* VALUE OF IX IS NOT IMPORTANT. SHOULD NOT BE 0 OR NVAR
*
            CALL FORSAV(TOTIME,DELDIP,IX,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,IFOR,FCONST,*9999)                           CSTP(call)
         ENDIF
  250 CONTINUE
      L=0
      DO 270 I=1,NVAR
         DO 270 J=1,I
            L=L+1
            SUM=0.D0
            DO 260 K=1,NVAR
               K1=(K-1)*NVAR+I
               K2=(K-1)*NVAR+J
  260       SUM=SUM+EVECS(K1)*FCONST(K)*EVECS(K2)
  270 FMATRX(L)=SUM*2.D0
      DO 280 I=1,NVAR
  280 COORDL(I)=COLD(I)
  290 CONTINUE
      DO 300 I=1,NUMAT
         IF(ATMASS(I).LT.1.D-20)THEN
            CALL FORSAV(TOTIME,DELDIP,NVAR,NVAR,FMATRX, COORD,NVAR,HEAT,
     1                EVECS,ILOOP,FCONST,*9999)                          CSTP(call)
            IF (DOPRNT) THEN                                             CIO
            WRITE(6,'(A)')' AT LEAST ONE ATOM HAS A ZERO MASS. A RESTART
     1'
            WRITE(6,'(A)')' FILE HAS BEEN WRITTEN AND THE JOB STOPPED'
            ENDIF                                                        CIO
      RETURN 1                                                           CSTP (stop)
         ENDIF
  300 CONTINUE
      IF(ISTART.LE.NVAR .AND. INDEX(KEYWRD,'ISOTOPE') .NE. 0)
     1CALL FORSAV(TOTIME,DELDIP,NVAR,NVAR,FMATRX, COORD,NVAR,HEAT,
     2                EVECS,ILOOP,FCONST,*9999)                          CSTP(call)
      RETURN
 9999 RETURN 1                                                          CSTP
      END
