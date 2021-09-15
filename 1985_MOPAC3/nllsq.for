      SUBROUTINE NLLSQ(X,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /KEYWRD/ KEYWRD
      CHARACTER*80 KEYWRD
      DIMENSION X(*)
      COMMON /MESAGE/ IFLEPO,IITER
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
*    THE FUNCTION TO BE MINIMIZED IS "COMPFG". COMPFG MUST HAVE THE
*    CALLING SEQUENCE
*                  CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,EFS,.TRUE.)
*                  SSQ=DOT(EFS,EFS,N)
*    WHERE   EFS  IS A VECTOR WHICH  COMPFG  FILLS WITH THE N INDIVIDUAL
*                 COMPONENTS OF THE ERROR FUNCTION AT THE POINT X
*            SSQ IS THE VALUE OF THE SUM OF THE  EFS  SQUARED.
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
C     IPRINT = PRINT SWITCH
C     NRST = NUMBER OF CYCLES BETWEEN SIDESTEPS
C     **********
      COMMON /TIME  / TIME0
      COMMON /NLLSQI/ NCOUNT
      DIMENSION EFSLST(MAXPAR), XLAST(MAXPAR), Q(MAXPAR,MAXPAR),
     1 R(MAXPAR,MAXPAR),
     2 Y(MAXPAR), EFS(MAXPAR), P(MAXPAR)
      COMMON /LAST  / LAST
      COMMON /NLLCOM/DDDUM,EFSLST,Q,R,XLAST,
     1               IIIUM,IDUMY(2*MAXHES-19-MAXPAR*4)
      DIMENSION IIIUM(7),DDDUM(6)
      LOGICAL MIDDLE, SCF1
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1
      EQUIVALENCE ( IIIUM(2), ICYC),(IIIUM(3), IRST),
     1(IIIUM(4),JRST),
     2(DDDUM(2),ALF), (DDDUM(3),SSQ),(DDDUM(4), PN)
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/
      MIDDLE=(INDEX(KEYWRD,'RESTART') .NE. 0)
      SCF1=(INDEX(KEYWRD,'1SCF') .NE. 0)
      IFLEPO=10
C*
      M=N
C*
      LAST=0
      MXCYCL=100
      I=INDEX(KEYWRD,'CYCLES=')
      IF (I.NE.0) THEN
         MXCYCL=READA(KEYWRD,I)
         WRITE(6,'(/10X,''NUMBER OF CYCLES TO BE RUN ='',I5)')MXCYCL
      ENDIF
      TOLS1=1.D-12
      TOLS2=1.D-10
      TOLS5=1.D-6
      TOLS6=1.D-3
      IPRINT=-1
      NRST=4
      YMAXST=1.D0
      TLEFT=28200
      I=INDEX(KEYWRD,' T=')
      IF(I.NE.0) THEN
         TIM=READA(KEYWRD,I)
         DO 10 J=I+3,80
            CH=KEYWRD(J:J)
            IF( CH .NE. CHDOT .AND. (CH .LT. ZERO .OR. CH .GT. NINE)) TH
     1EN
               IF( CH .EQ. 'M') TIM=TIM*60
               GOTO 20
            ENDIF
   10    CONTINUE
   20    TLEFT=TIM
      ENDIF
      TLEFT=TLEFT-SECOND()+TIME0
C     **********
C     SET UP COUNTERS AND SWITCHES
C     **********
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
      IF(MIDDLE) THEN
         CALL PARSAV(0,N,M)
         CLOSE(13)
         NCOUNT=IIIUM(5)
         MXCYCL=MXCYCL+ICYC
         DO 30 I=1,N
   30    X(I)=XLAST(I)
         TIME1=SECOND()
         GOTO 80
      ENDIF
      CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFSLST,.TRUE.)
      IF( SCF1 ) GOTO 880
      SSQ=DOT(EFSLST,EFSLST,N)
      NCOUNT = 1
   40 CONTINUE
      DO 60 I=1,M
         DO 50 J=1,N
            R(I,J) = 0.0D0
            IF (I .EQ. J)  R(I,J)=1.0D0
   50    CONTINUE
         DO 60 J=I,M
            Q(I,J) = 0.0D0
            Q(J,I) = 0.0D0
            IF (I .EQ. J)  Q(I,I)=1.0D0
   60 CONTINUE
      TEMP = 0.0D0
      DO 70 I=1,N
   70 TEMP = TEMP+X(I)**2
      ALF = 100.0D0*(EPS*SQRT(TEMP)+T)
C     **********
C     MAIN LOOP
C     **********
      TIME1=SECOND()
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
         TEMP = 0.0D0
         DO 90 J=1,M
   90    TEMP = TEMP-Q(J,I)*EFSLST(J)
  100 EFS(I) = TEMP
      DO 110 J=1,N
         JJ = NP1-J
         DO 110 I=1,J
            II = NP2-I
  110 R(II,JJ) = R(I,J)
      DO 180 I=1,N
         I1 = I+1
         Y(I) = PRT
         EFSSS=0.0D0
         IF (I .GE. N)  GO TO 130
         DO 120 J=I1,N
  120    Y(J) = 0.0D0
  130    CONTINUE
         DO 170 J=I,N
            II = NP2-J
            JJ = NP1-J
            IF (ABS(Y(J)) .LT. ABS(R(II,JJ)))  GO TO 140
            TEMP = Y(J)*SQRT(1.0D0+(R(II,JJ)/Y(J))**2)
            GO TO 150
  140       TEMP = R(II,JJ)*SQRT(1.0D0+(Y(J)/R(II,JJ))**2)
  150       CONTINUE
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
      I = N
  190 I = I-1
      IF (I)  220,220,200
  200 TEMP = EFS(I)
      K = I+1
      II = NP2-I
      DO 210 J=K,N
         JJ = NP1-J
  210 TEMP = TEMP-R(II,JJ)*P(J)
      JJ = NP1-I
      P(I) = TEMP/R(II,JJ)
      GO TO 190
  220 CONTINUE
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
         WRITE(6,'('' SYSTEM DOES NOT APPEAR TO BE OPTIMIZABLE.'',/
     1,'' THIS CAN HAPPEN IF (A) IT WAS OPTIMIZED TO BEGIN WITH'',/
     2,'' OR                 (B) IT IS NEITHER A GROUND NOR A'',
     3'' TRANSITION STATE'')')
         CALL GEOUT
         STOP
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
C     **********
C     PRINTING SECTION
C     **********
C#      WRITE(6,501)TLEFT,ICYC,SSQ
      DO 270 I=1,N
         EFS(I)=X(I)
  270 CONTINUE
C     **********
C     PERFORM LINE-MINIMIZATION FROM POINT X IN DIRECTION P OR -P
C     **********
      SSQLST = SSQ
      DO 280 I=1,N
         EFS(I)=0.D0
  280 XLAST(I)=X(I)
      CALL LOCMIN(M,X,N,P,SSQ,ALF,EFS,IERR,ESCF)
      IF(SSQLST .LT. SSQ ) THEN
         IF(IERR .EQ. 0)      SSQ=SSQLST
         DO 290 I=1,N
  290    X(I)=XLAST(I)
         IRST=NRST
         PN=PNLAST
         TIME2=TIME1
         TIME1=SECOND()
         TCYCLE=TIME1-TIME2
         TLEFT=TLEFT-TCYCLE
         IF(TLEFT .GT. TCYCLE*2) GO TO 80
         GOTO 630
      ENDIF
      IREPET=0
C     **********
C     PRODUCE THE VECTOR   R*P
C     **********
      DO 310 I=1,N
         TEMP = 0.0D0
         DO 300 J=I,N
  300    TEMP = TEMP+R(I,J)*P(J)
  310 Y(I) = TEMP
C     **********
C     PRODUCE THE VECTOR ...
C                  Y  =    (EFS-EFSLST-ALF*Q*R*P)/(ALF*(NORMSQUARE(P))
C     COMPUTE NORM OF THIS VECTOR AS WELL
C     **********
      WORK = ALF*PN2
      YN = 0.0D0
      DO 330 I=1,M
         TEMP = 0.0D0
         DO 320 J=1,N
  320    TEMP = TEMP+Q(I,J)*Y(J)
         TEMP = (EFS(I)-EFSLST(I)-ALF*TEMP)
         EFSLST(I) = EFS(I)
         YN = YN+TEMP**2
  330 EFS(I) = TEMP/WORK
      YN = SQRT(YN)/WORK
C     **********
C     THE BROYDEN UPDATE   NEW MATRIX = OLD MATRIX + Y*(P-TRANS)
C     HAS BEEN FORMED.  IT IS NOW NECESSARY TO UPDATE THE  QR DECOMP.
C     FIRST LET    Y = (Q-TRANS)*Y.
C     **********
      DO 350 I=1,M
         TEMP = 0.0D0
         DO 340 J=1,M
  340    TEMP = TEMP+Q(J,I)*EFS(J)
  350 Y(I) = TEMP
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
      DO 360 I=NP1,M
  360 CONST=MAX(ABS(Y(NP1)),CONST)
      YTAIL = 0.0D0
      DO 370 I=NP1,M
  370 YTAIL = YTAIL+(Y(I)/CONST)**2
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
      IF (I)  500,500,440
  440 IF (Y(J))  450,430,450
  450 IF (ABS(Y(I)) .LT. ABS(Y(J)))  GO TO 460
      TEMP = ABS(Y(I))*SQRT(1.0D0+(Y(J)/Y(I))**2)
      GO TO 470
  460 TEMP = ABS(Y(J))*SQRT(1.0D0+(Y(I)/Y(J))**2)
  470 COS = Y(I)/TEMP
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
  500 CONTINUE
C     **********
C     REDUCE THE UPPER-HESSENBERG MATRIX TO UPPER-TRIANGULAR FORM
C     USING ELEMENTARY ROTATIONS.  APPLY THE SAME ROTATIONS, TRANSPOSED,
C     ON THE RIGHT TO THE MATRIX  Q.
C     **********
      DO 510 K=1,N
  510 R(1,K) = R(1,K)+YN*P(K)
      JEND = NP1
      IF (M .EQ. N)  JEND=N
      DO 580 J=2,JEND
         I = J-1
         IF (R(J,I))  520,580,520
  520    IF (ABS(R(I,I)) .LT. ABS(R(J,I)))  GO TO 530
         TEMP = ABS(R(I,I))*SQRT(1.0D0+(R(J,I)/R(I,I))**2)
         GO TO 540
  530    TEMP = ABS(R(J,I))*SQRT(1.0D0+(R(I,I)/R(J,I))**2)
  540    COS = R(I,I)/TEMP
         SIN = R(J,I)/TEMP
         R(I,I) = TEMP
         IF (J .GT. N)  GO TO 560
         DO 550 K=J,N
            TEMP = COS*R(I,K)+SIN*R(J,K)
            WORK = -SIN*R(I,K)+COS*R(J,K)
            R(I,K) = TEMP
  550    R(J,K) = WORK
  560    DO 570 K=1,M
            TEMP = COS*Q(K,I)+SIN*Q(K,J)
            WORK = -SIN*Q(K,I)+COS*Q(K,J)
            Q(K,I) = TEMP
  570    Q(K,J) = WORK
  580 CONTINUE
C     **********
C     CHECK THE STOPPING CRITERIA
C     **********
      TEMP = 0.0D0
      DO 590 I=1,N
  590 TEMP = TEMP+X(I)**2
      TOLX = TOLS1*SQRT(TEMP)+TOLS2
      IF (SQRT(ALF*PN2) .LE. TOLX)  GO TO 650
      IF(SSQ.GE.2.D0*N) GO TO 610
      DO 600 I=1,N
C*****
C     The stopping criterion is that no individual gradient be
C         greater than 0.2
C*****
         IF(ABS(EFSLST(I)).GE.0.2D0) GO TO 610
  600 CONTINUE
      WRITE(6,690) SSQ
      GO TO 660
  610 CONTINUE
      IF (ICYC .GE. MXCYCL)  THEN
         IFLEPO=12
         GOTO 880
      ENDIF
      TIME2=TIME1
      TIME1=SECOND()
      TCYCLE=TIME1-TIME2
      TLEFT=TLEFT-TCYCLE
      WRITE(6,620)ICYC,TCYCLE,TLEFT,SQRT(SSQ),ESCF
  620 FORMAT(' CYCLE:',I5,' TIME:',F7.2,' TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
      IF(TLEFT .GT. TCYCLE*2) GO TO 80
  630 IIIUM(5)=NCOUNT
      DO 640 I=1,N
  640 XLAST(I)=X(I)
      CALL PARSAV(1,N,M)
      STOP
  650 WRITE (6,770)  NCOUNT
      GOTO 880
  660 WRITE (6,780)  NCOUNT
      GOTO 880
  670 CONTINUE
      WRITE(6,680) IXSO
  680 FORMAT(1H ,5X,'ATTEMPT TO GO DOWNHILL IS UNSUCCESSFUL AFTER',I5,5X
     1,'ORTHOGONAL SEARCHES')
      GOTO 880
  690 FORMAT(1H ,'SSQ =',F15.7)
  700 FORMAT(1H ,3X,'ALF =',E12.4)
  710 FORMAT(1H ,3X,'NCOUNT =',I5)
  720 FORMAT(3X,'TIME LEFT:',F7.1,' CYCLE',I5,3X,'GNORM SQUARED IS'
     1,F13.5)
  730 FORMAT(4(5X,'X(',I2,') = ',E15.8))
  740 FORMAT(4(5X,'P(',I2,') = ',E15.8))
  750 FORMAT(5X,'R-MATRIX DIAGONAL ENTRIES ...')
  760 FORMAT(6E13.3)
  770 FORMAT('0TEST ON X SATISFIED, NUMBER OF FUNCTION CALLS = ',I5)
  780 FORMAT('0TEST ON SSQ SATISFIED, NUMBER OF FUNCTION CALLS = ',I5)
  790 FORMAT(' ///// NEXT CYCLE IS A SIDE-STEP ALONG THE ',I2,
     1  '-TH NORMAL TO P')
  800 FORMAT('0ALLOWED NUMBER OF FUNCTION CALLS EXCEEDED.'/
     1  ' NUMBER OF FUNCTION CALLS WAS ',I5)
  810 FORMAT('  L.-M. PARAMETER = ',E15.7,
     1  '   SUMSQUARES CHANGE = ',E15.7)
  820 FORMAT(1H )
  830 FORMAT(1H )
  840 FORMAT(1H ,3X,'I',7X,I2,9(10X,I2))
  850 FORMAT(1H ,1X,'X(I)',1X,F10.5,2X,9(F10.5,2X))
  860 FORMAT(1H ,1X,'G(I)',1X,F10.5,2X,9(F10.5,2X))
  870 FORMAT(1H ,1X,'P(I)',1X,F10.5,2X,9(F10.5,2X))
  880 LAST=1
      CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFSLST,.TRUE.)
      RETURN
      END
