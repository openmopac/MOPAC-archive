      SUBROUTINE NLLSQ(X,N,ESCF,GRAD) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
       EXTERNAL SDOT                                                    GL0492
      DIMENSION X(*),GRAD(*) 
************************************************************************ 
* 
*  NLLSQ IS A NON-DERIVATIVE, NONLINEAR LEAST-SQUARES MINIMIZER. IT USES 
*        BARTEL'S PROCEDURE TO MINIMISE A FUNCTION WHICH IS A SUM OF 
*        SQUARES. 
* 
*    ON INPUT N    = NUMBER OF UNKNOWNS 
*             X    = PARAMETERS OF FUNCTION TO BE MINIMIZED. 
* 
*    ON EXIT  X    = OPTIMISED PARAMETERS. 
* 
*    THE FUNCTION TO BE MINIMIZED IS THE SUM OF SQUARE OF RESIDALS 
*    RETURNED BY 'COMPFG'. 
*==> FOR PRESENT PURPOSE THE RESIDUALS ARE THE FIRST DERIVATIVES OF 
*    THE ENERGY ESCF. 
*    THE CALLING SEQUENCE OF COMPFG MUST BE THE FOLLOWING : 
*                  CALL COMPFG(XPARAM,ESCF,FAIL,EFS,.TRUE.) 
*                  SSQ=DOT(EFS,EFS,N) 
*    WHERE   EFS  IS A VECTOR WHICH  COMPFG  FILLS WITH THE N INDIVIDUAL 
*                 COMPONENTS OF THE ERROR FUNCTION AT THE POINT X 
*            SSQ  IS THE VALUE OF THE SUM OF THE  EFS  SQUARED 
*            FAIL IS A FLAG TURNED TO .TRUE. IF EFS IS NOT RELIABLE. 
*    IN THIS FORMULATION OF NLLSQ M AND N ARE THE SAME. 
*    THE PRECISE DEFINITIONS OF THESE TWO QUANTITIES IS: 
* 
*     N = NUMBER OF PARAMETERS TO BE OPTIMIZED. 
*     M = NUMBER OF REFERENCE FUNCTIONS. M MUST BE GREATER THEN, OR 
*         EQUAL TO, N 
************************************************************************ 
C     Q = ORTHOGONAL MATRIX   (M BY M) 
C     R = RIGHT-TRIANGULAR MATRIX   (M BY N) 
C     MXCNT(1) = MAX ALLOW OVERALL FUN EVALS 
C     MXCNT(2) = MAX ALLOW NO OF FNC EVALS PER LIN SEARCH 
C     TOLS1 = RELATIVE TOLERANCE ON X OVERALL 
C     TOLS2 = ABSOLUTE TOLERANCE ON X OVERALL 
C     TOLS5 = RELATIVE TOLERANCE ON X FOR LINEAR SEARCHES 
C     TOLS6 = ABSOLUTE TOLERANCE ON X FOR LINEAR SEARCHES 
C     NRST = NUMBER OF CYCLES BETWEEN SIDESTEPS 
C     ********** 
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
     1       /MESAGE/ IFLEP, ISCF                                       DJG0995
     3       /LAST  / LAST 
     4       /PRECI / SCFCV,SCFTOL,EG(9),KDUM(MAXPAR) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / ICYC,IRST,JRST,NDOUBL,M,NCOUNT,                  3GL3092 
     1                 IDUMMY(19 + 2*NCHAIN - 6)                        3GL3092
      COMMON /OPTMCR / Q(MAXPAR,MAXPAR),EFSLST(MAXPAR),                 3GL3092
     1                 R(MAXPAR,MAXPAR),XLAST(MAXPAR),ALF,SSQ,PN,       3GL3092
     2                 Y(MAXPAR),EFS(MAXPAR),P(MAXPAR),                 3GL3092
     3                RDUMY(MAXHES+MAXPAR*(MAXPAR+NCHAIN+11)+NCHAIN+26) 3GL3092 
      LOGICAL RESTRT, FAIL 
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1 
      SAVE
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/ 
C     ********** 
C     READ KEYWORDS AND OVERWRITE STANDARD OPTIONS 
C     ********** 
      RESTRT=(IRESTA.NE.0)                                              DJG0995
C     LENGTH OF /OPTIM/ TO BE SAVED FOR RESTART ACCORDING TO 
C     /OPTIM/ I1(MAXPAR,LEN1),I2(LEN2). 
C     LEN1=(2*MAXPAR+2)*2 
C     LEN2=           3*2 + 8 
C
C  LEN1, LEN2, AND LEN3 HAVE BEEN REDEFINED TO REPRESENT THE LENGTH OF THE 
C  COMMON BLOCKS OPTMCR, OPTMCI, AND OPTMCL IN SVOPTS (NEW VERSION OF SAVOPT) 
C
      LEN1=MAXPAR*(2*MAXPAR+5)+3                                        GL0492
      LEN2=6                                                            GL0492
      LEN3=0                                                            GL0492
      MXCYCL=100 
      IF (ICYCLE.NE.0) MXCYCL=IICYCL                                    DJG0995
      IF (IPRINT.NE.0) IMP=IIPRIN                                       DJG0995
      GNORM=1.D0 
      IF(IPRECI.NE.0) GNORM=GNORM*0.1D0                                 DJG0995
      IF (IGNORM.NE.0) GNORM=FIGNOR                                     DJG0995
      TOLS1=1.D-12 
      TOLS2=1.D-10 
      TOLS5=1.D-6 
      TOLS6=1.D-3 
      NRST=4 
      YMAXST=1.D0 
C     TLEFT=3600 
      TLEFT=TDEF                                                        GL0492
      IF(ITLIMI.NE.0) THEN                                              DJG0995
         TIM=FITLIM                                                     DJG0995
         TLEFT=TIM                                                      DJG0995
      ENDIF 
      SUM=TLEFT 
      CALL PORCPU (TFLY)                                                GL0492
      TLEFT=TLEFT-TFLY+TIME0 
C     ********** 
C     SET UP COUNTERS AND SWITCHES 
C     ********** 
      IFLEP=11                                                          DJG0995
      M=N 
      NDOUBL=N 
      LAST=0 
      NTO=N/6 
      IFRTL=0 
      NREM=N-(NTO*6) 
      IREPET=0 
      NSST=0 
      IF(IXSO.EQ.0) IXSO=N 
      NP1 = N+1 
      NP2 = N+2 
      ICYC = 0 
      IRST = 0 
      JRST = 1 
      EPS =TOLS5 
      T = TOLS6 
C     ********** 
C     GET STARTING-POINT FUNCTION VALUE 
C     SET UP ESTIMATE OF INITIAL LINE STEP 
C     ********** 
      IF(RESTRT) THEN 
         CALL SVOPTS(LEN1,LEN2,LEN3,.TRUE.)                             GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         MXCYCL=MXCYCL+ICYC 
         CALL SCOPY (N,XLAST,1,X,1) 
         CALL PORCPU (TIME1)                                            GL0492
         GOTO 80 
      ENDIF 
      CALL COMPFG(X,ESCF,FAIL,EFSLST,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      SSQ=DOT(EFSLST,EFSLST,N) 
      RMS=SQRT(SSQ/DBLE(N))                                             GCL0393
      GNORM=MAX(GNORM,SQRT(DBLE(N))*(EG(1)+EG(2)+EG(3)))                GCL0393
      WRITE(JOUT,'('' MINIMIZATION OF THE GRADIENT NORM BY NLLSQ ...'' 
     ./''    MAXIMUM NUMBER OF CYCLES         ='',I5 
     ./''    ALLOWED TIME                     ='',F8.2,'' SECONDS'' 
     ./''    INITIAL RMS GRADIENT             ='',F8.2 
     ./''    CV THRESHOLD ON THE RMS GRADIENT ='',F8.2 
     ./''    NUMBER OF OPTIMIZED VARIABLES    ='',I5 
     .)') MXCYCL,SUM,RMS,GNORM,N 
      IF(FAIL) THEN
          ISTOP=1                                                       GDH1095
          IWHERE=147                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NCOUNT = 1 
      DO 50 I=1,M 
         DO 30 J=1,N 
   30    R(I,J) = 0.0D0 
         DO 40 J=1,M 
   40    Q(J,I) = 0.0D0 
      R(I,I)=1.D0 
   50 Q(I,I)=1.D0 
      TEMP = DOT(X,X,N) 
      ALF = 100.0D0*(EPS*SQRT(TEMP)+T) 
C     ********** 
C     MAIN LOOP 
C     ********** 
      CALL PORCPU (TIME1)                                               GL0492
   80 CONTINUE 
C     ********** 
C     UPDATE COUNTERS AND TEST FOR PRINTING THIS CYCLE 
C     ********** 
      IFRTL=IFRTL+1 
      ICYC = ICYC+1 
      IRST = IRST+1 
C     ********** 
C     SET  PRT,  THE LEVENBERG-MARQUARDT PARAMETER. 
C     ********** 
      PRT = SQRT(SSQ) 
C     ********** 
C     IF A SIDESTEP IS TO BE TAKEN, GO TO 31 
C     ********** 
      IF (IRST .GE. NRST)  GO TO 230 
C     ********** 
C     SOLVE THE SYSTEM    Q*R*P = -EFSLST    IN THE LEAST-SQUARES SENSE 
C     ********** 
      NSST=0 
      DO 100 I=1,M 
  100 EFS(I) = -DOT(Q(1,I),EFSLST,M) 
      DO 110 J=1,N 
         JJ = NP1-J 
         DO 110 I=1,J 
            II = NP2-I 
  110 R(II,JJ) = R(I,J) 
      DO 180 I=1,N 
         I1 = I+1 
         Y(I) = PRT 
         EFSSS=0.0D0 
         IF (I.LT.N) THEN 
            DO 120 J=I1,N 
  120       Y(J) = 0.0D0 
         ENDIF 
         DO 170 J=I,N 
            II = NP2-J 
            JJ = NP1-J 
            IF (ABS(Y(J)) .GE. ABS(R(II,JJ))) THEN 
               TEMP = Y(J)*SQRT(1.0D0+(R(II,JJ)/Y(J))**2) 
            ELSE 
               TEMP = R(II,JJ)*SQRT(1.0D0+(Y(J)/R(II,JJ))**2) 
            ENDIF 
            SIN = R(II,JJ)/TEMP 
            COS = Y(J)/TEMP 
            R(II,JJ) = TEMP 
            TEMP = EFS(J) 
            EFS(J)=SIN*TEMP+COS*EFSSS 
            EFSSS=SIN*EFSSS-COS*TEMP 
            IF (J .GE. N)  GO TO 180 
            J1 = J+1 
            DO 160 K=J1,N 
               JJ = NP1-K 
               TEMP = R(II,JJ) 
               R(II,JJ) = SIN*TEMP+COS*Y(K) 
  160       Y(K) = SIN*Y(K)-COS*TEMP 
  170    CONTINUE 
  180 CONTINUE 
      P(N) = EFS(N)/R(2,1) 
      DO 210 I=N,1,-1 
         TEMP = EFS(I) 
         K = I+1 
         II = NP2-I 
         DO 200 J=K,N 
         JJ = NP1-J 
  200    TEMP = TEMP-R(II,JJ)*P(J) 
         JJ = NP1-I 
  210 P(I) = TEMP/R(II,JJ) 
      GO TO 250 
C     ********** 
C     SIDESTEP SECTION 
C     ********** 
  230 JRST = JRST+1 
      NSST=NSST+1 
      IF(NSST.GE.IXSO) GO TO 670 
      IF (JRST .GT. N)  JRST=2 
      IRST = 0 
C     ********** 
C     PRODUCTION OF A VECTOR ORTHOGONAL TO THE LAST P-VECTOR 
C     ********** 
      WORK = PN*(ABS(P(1))+PN) 
      TEMP = P(JRST) 
      P(1) = TEMP*(P(1)+SIGN(PN,P(1))) 
      DO 240 I=2,N 
  240 P(I) = TEMP*P(I) 
      P(JRST) = P(JRST)-WORK 
C     ********** 
C     COMPUTE NORM AND NORM-SQUARE OF THE P-VECTOR 
C     ********** 
  250 PNLAST = PN 
      PN=0.D0 
      PN2 = 0.0D0 
      DO 260 I=1,N 
         PN=PN+ABS(P(I)) 
  260 PN2 = PN2+P(I)**2 
      IF(PN.LT.1.D-20) THEN 
         WRITE(JOUT,'('' SYSTEM DOES NOT APPEAR TO BE OPTIMIZABLE.'',/ 
     1,'' THIS CAN HAPPEN IF (A) IT WAS OPTIMIZED TO BEGIN WITH'',/ 
     2,'' OR                 (B) IT IS NEITHER A GROUND NOR A'', 
     3'' TRANSITION STATE'')') 
         CALL GEOUT 
          ISTOP=1                                                       GDH1095
          IWHERE=148                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF 
      IF(PN2.LT.1.D-20)PN2=1.D-20 
      PN = SQRT(PN2) 
      IF(ALF.GT.1.D20)ALF=1.D20 
      IF(ICYC .GT. 1) THEN 
         ALF=ALF*1.D-20*PNLAST/PN 
         IF(ALF.GT.1.D10)        ALF=1.D10 
         ALF=ALF*1.D20 
      ENDIF 
      TTMP=ALF*PN 
      IF(TTMP.LT.0.0001D0) ALF=0.001D0/PN 
      CALL SCOPY (N,X,1,EFS,1) 
C     ********** 
C     PERFORM LINE-MINIMIZATION FROM POINT X IN DIRECTION P OR -P 
C     ********** 
      SSQLST = SSQ 
      CALL SCOPY (N,X,1,XLAST,1) 
      CALL LOCMIN(M,X,N,P,EFS,IERR,ESCF) 
      IF(SSQLST .LT. SSQ ) THEN 
         IF(IERR .EQ. 0)      SSQ=SSQLST 
         CALL SCOPY (N,XLAST,1,X,1) 
         IRST=NRST 
         PN=PNLAST 
         TIME2=TIME1 
         CALL PORCPU (TIME1)                                            GL0492
         TCYCLE=TIME1-TIME2 
         TLEFT=TLEFT-TCYCLE 
         IF(TLEFT .GT. TCYCLE*2) GO TO 80 
         GOTO 630 
      ENDIF 
      IREPET=0 
C     ********** 
C     PRODUCE THE VECTOR   R*P 
C     ********** 
      DO 300 I=1,N 
  300 Y(I) = SDOT(N-I+1,R(I,I),MAXPAR,P(I),1) 
C     ********** 
C     PRODUCE THE VECTOR ... 
C                  Y  =    (EFS-EFSLST-ALF*Q*R*P)/(ALF*(NORMSQUARE(P)) 
C     COMPUTE NORM OF THIS VECTOR AS WELL 
C     ********** 
      WORK = ALF*PN2 
      YN = 0.0D0 
      DO 310 I=1,M 
         TEMP = (EFS(I)-EFSLST(I)-ALF*SDOT(N,Q(I,1),MAXPAR,Y,1)) 
         EFSLST(I) = EFS(I) 
         YN = YN+TEMP**2 
  310 EFS(I) = TEMP/WORK 
      YN = SQRT(YN)/WORK 
C     ********** 
C     THE BROYDEN UPDATE   NEW MATRIX = OLD MATRIX + Y*(P-TRANS) 
C     HAS BEEN FORMED.  IT IS NOW NECESSARY TO UPDATE THE  QR DECOMP. 
C     FIRST LET    Y = (Q-TRANS)*Y. 
C     ********** 
      DO 320 I=1,M 
  320 Y(I) = DOT(Q(1,I),EFS,M) 
C     ********** 
C     REDUCE THE VECTOR Y TO A MULTIPLE OF THE FIRST UNIT VECTOR USING 
C     A HOUSEHOLDER TRANSFORMATION FOR COMPONENTS N+1 THROUGH M AND 
C     ELEMENTARY ROTATIONS FOR THE FIRST N+1 COMPONENTS.  APPLY ALL 
C     TRANSFORMATIONS TRANSPOSED ON THE RIGHT TO THE MATRIX Q, AND 
C     APPLY THE ROTATIONS ON THE LEFT TO THE MATRIX R. 
C     THIS GIVES    (Q*(V-TRANS))*((V*R) + (V*Y)*(P-TRANS)),    WHERE 
C     V IS THE COMPOSITE OF THE TRANSFORMATIONS.  THE MATRIX 
C     ((V*R) + (V*Y)*(P-TRANS))    IS UPPER HESSENBERG. 
C     ********** 
      IF (M .LE. NP1)  GO TO 410 
C 
C THE NEXT THREE LINES WERE INSERTED TO TRY TO GET ROUND OVERFLOW BUGS. 
C 
      CONST=1.D-12 
      DO 330 I=NP1,M 
  330 CONST=MAX(ABS(Y(NP1)),CONST) 
      YTAIL = 0.0D0 
      DO 340 I=NP1,M 
  340 YTAIL = YTAIL+(Y(I)/CONST)**2 
      YTAIL = SQRT(YTAIL)*CONST 
      BET = (1.0D25/YTAIL)/(YTAIL+ABS(Y(NP1))) 
      Y(NP1) = SIGN (YTAIL+ABS(Y(NP1)),Y(NP1)) 
      DO 400 I=1,M 
         TMP = 0.0D0 
         DO 380 J=NP1,M 
  380    TMP = TMP+Q(I,J)*Y(J)*1.D-25 
         TMP = BET*TMP 
         DO 390 J=NP1,M 
  390    Q(I,J) = Q(I,J)-TMP*Y(J) 
  400 CONTINUE 
      Y(NP1) = YTAIL 
      I = NP1 
      GO TO 420 
  410 CONTINUE 
      I = M 
  420 CONTINUE 
  430 J = I 
      I = I-1 
      IF (I.LE.0)  GO TO 500 
      IF (Y(J).EQ.0.D0)  GO TO 430 
      IF (ABS(Y(I)) .GE. ABS(Y(J))) THEN 
         TEMP = ABS(Y(I))*SQRT(1.0D0+(Y(J)/Y(I))**2) 
      ELSE 
         TEMP = ABS(Y(J))*SQRT(1.0D0+(Y(I)/Y(J))**2) 
      ENDIF 
      COS = Y(I)/TEMP 
      SIN = Y(J)/TEMP 
      Y(I) = TEMP 
      DO 480 K=1,M 
         TEMP = COS*Q(K,I)+SIN*Q(K,J) 
         WORK = -SIN*Q(K,I)+COS*Q(K,J) 
         Q(K,I) = TEMP 
  480 Q(K,J) = WORK 
      IF (I .GT. N)  GO TO 430 
      R(J,I) = -SIN*R(I,I) 
      R(I,I) = COS*R(I,I) 
      IF (J .GT. N)  GO TO 430 
      DO 490 K=J,N 
         TEMP = COS*R(I,K)+SIN*R(J,K) 
         WORK = -SIN*R(I,K)+COS*R(J,K) 
         R(I,K) = TEMP 
  490 R(J,K) = WORK 
      GO TO 430 
C     ********** 
C     REDUCE THE UPPER-HESSENBERG MATRIX TO UPPER-TRIANGULAR FORM 
C     USING ELEMENTARY ROTATIONS.  APPLY THE SAME ROTATIONS, TRANSPOSED, 
C     ON THE RIGHT TO THE MATRIX  Q. 
C     ********** 
  500 CALL SAXPY (N,YN,P,1,R,MAXPAR) 
      JEND = NP1 
      IF (M .EQ. N)  JEND=N 
      DO 530 J=2,JEND 
         I = J-1 
         IF (R(J,I).EQ.0.D0)  GO TO 530 
         IF (ABS(R(I,I)) .GE. ABS(R(J,I))) THEN 
            TEMP = ABS(R(I,I))*SQRT(1.0D0+(R(J,I)/R(I,I))**2) 
         ELSE 
            TEMP = ABS(R(J,I))*SQRT(1.0D0+(R(I,I)/R(J,I))**2) 
         ENDIF 
         COS = R(I,I)/TEMP 
         SIN = R(J,I)/TEMP 
         R(I,I) = TEMP 
         IF (J .LE. N) THEN 
            DO 510 K=J,N 
            TEMP = COS*R(I,K)+SIN*R(J,K) 
            WORK = -SIN*R(I,K)+COS*R(J,K) 
            R(I,K) = TEMP 
  510       R(J,K) = WORK 
         ENDIF 
         DO 520 K=1,M 
         TEMP = COS*Q(K,I)+SIN*Q(K,J) 
         WORK = -SIN*Q(K,I)+COS*Q(K,J) 
         Q(K,I) = TEMP 
  520    Q(K,J) = WORK 
  530 CONTINUE 
C     ********** 
C     CHECK THE STOPPING CRITERIA 
C     ********** 
      TEMP = DOT(X,X,N) 
      TOLX = TOLS1*SQRT(TEMP)+TOLS2 
      IF (SQRT(ALF*PN2) .LE. TOLX)  GO TO 650 
      IF(SSQ.GE.GNORM*SQRT(DBLE(N))) GO TO 610                          GCL0393
C***** 
C     The stopping criterion is that no individual gradient be 
C     greater than 2.5*GNORM i.e 2.5 times the standard deviation 
C***** 
      DO 600 I=1,N 
         IF(ABS(EFSLST(I)).GE.2.5D0*GNORM) GO TO 610 
  600 CONTINUE 
      RMS=SSQ/SQRT(DBLE(N))                                             GCL0393
      GO TO 660 
  610 CONTINUE 
      IF (ICYC .GE. MXCYCL)  THEN 
         IFLEP=12                                                       DJG0995
         GOTO 880 
      ENDIF 
      TIME2=TIME1 
      CALL PORCPU (TIME1)                                               GL0492
      TCYCLE=TIME1-TIME2 
      TLEFT=TLEFT-TCYCLE 
      IF (IMP.GT.0) WRITE(JOUT,1000)ICYC,TCYCLE,TLEFT,SQRT(SSQ),ESCF 
      IF(TLEFT .GT. TCYCLE*2) GO TO 80 
  630 CALL SCOPY (N,X,1,XLAST,1) 
      CALL SVOPTS(LEN1,LEN2,LEN3,.FALSE.)                               GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
          ISTOP=1                                                       GDH1095
          IWHERE=149                                                    GDH1095
          RETURN                                                        GDH1095
  650 WRITE (JOUT,1020)  NCOUNT 
      GOTO 880 
  660 WRITE (JOUT,1030)  NCOUNT,SSQ,RMS 
      GOTO 880 
  670 CONTINUE 
      WRITE(JOUT,1010) IXSO 
      GOTO 880 
  880 LAST=1 
      CALL COMPFG(X,ESCF,FAIL,GRAD,.TRUE.) 
      RETURN 
 1000 FORMAT(' CYCLE:',I5,' TIME:',F7.2,' TIME LEFT:',F9.1, 
     1' GRAD.:',F10.3,' HEAT:',G14.7) 
 1010 FORMAT(1H ,5X,'ATTEMPT TO GO DOWNHILL IS UNSUCCESSFUL AFTER',I5,5X 
     1,'ORTHOGONAL SEARCHES') 
 1020 FORMAT('0TEST ON X SATISFIED, NUMBER OF FUNCTION CALLS = ',I5) 
 1030 FORMAT('0TEST ON SSQ SATISFIED, NUMBER OF FUNCTION CALLS = ',I5 
     ./' SSQ =',F8.3,5X,'RMS =',F8.3) 
      END 
