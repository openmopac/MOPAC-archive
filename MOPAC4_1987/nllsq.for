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
*        BARTEL'S PROCEDURE TO MINIMIZE A FUNCTION WHICH IS A SUM OF
*        SQUARES.
*
*    ON INPUT N    = NUMBER OF UNKNOWNS
*             X    = PARAMETERS OF FUNCTION TO BE MINIMIZED.
*
*    ON EXIT  X    = OPTIMIZED PARAMETERS.
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
      COMMON /NUMSCF/ NSCF
      DIMENSION Y(MAXPAR), EFS(MAXPAR), P(MAXPAR)
      COMMON /LAST  / LAST
      COMMON /NLLCOM/DDDUM(6),EFSLST(MAXPAR),Q(MAXPAR,MAXPAR),
     1               R(MAXPAR,MAXPAR),XLAST(MAXPAR),
     2               IIIUM(7),IDUMY(2*MAXPAR*MAXPAR-19-MAXPAR*4)
      LOGICAL MIDDLE, SCF1, RESFIL
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
      I=INDEX(KEYWRD,'CYCLES')
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
      TLEFT=MAXTIM
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
      TLAST=TLEFT
      TDUMP=MAXDMP
      I=INDEX(KEYWRD,' DUMP')
      IF(I.NE.0) THEN
         TDUMP=READA(KEYWRD,I)
         DO 30 J=I+7,80
            CH=KEYWRD(J:J)
            IF( CH .NE. CHDOT .AND. (CH .LT. ZERO .OR. CH .GT. NINE))
     1 THEN
               IF( CH .EQ. 'M') TDUMP=TDUMP*60
               GOTO 40
            ENDIF
   30    CONTINUE
   40    CONTINUE
      ENDIF
      RESFIL=.FALSE.
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
         NSCF=IIIUM(1)
         CLOSE(13)
         NCOUNT=IIIUM(5)
         MXCYCL=MXCYCL+ICYC
         DO 50 I=1,N
   50    X(I)=XLAST(I)
         TIME1=SECOND()
         GOTO 100
      ENDIF
      CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFSLST,.TRUE.)
      IF( SCF1 ) GOTO 920
      SSQ=DOT(EFSLST,EFSLST,N)
      NCOUNT = 1
   60 CONTINUE
      DO 80 I=1,M
         DO 70 J=1,N
            R(I,J) = 0.0D0
            IF (I .EQ. J)  R(I,J)=1.0D0
   70    CONTINUE
         DO 80 J=I,M
            Q(I,J) = 0.0D0
            Q(J,I) = 0.0D0
            IF (I .EQ. J)  Q(I,I)=1.0D0
   80 CONTINUE
      TEMP = 0.0D0
      DO 90 I=1,N
   90 TEMP = TEMP+X(I)**2
      ALF = 100.0D0*(EPS*SQRT(TEMP)+T)
C     **********
C     MAIN LOOP
C     **********
      TIME1=SECOND()
  100 CONTINUE
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
      IF (IRST .GE. NRST)  GO TO 250
C     **********
C     SOLVE THE SYSTEM    Q*R*P = -EFSLST    IN THE LEAST-SQUARES SENSE
C     **********
      NSST=0
      DO 120 I=1,M
         TEMP = 0.0D0
         DO 110 J=1,M
  110    TEMP = TEMP-Q(J,I)*EFSLST(J)
  120 EFS(I) = TEMP
      DO 130 J=1,N
         JJ = NP1-J
         DO 130 I=1,J
            II = NP2-I
  130 R(II,JJ) = R(I,J)
      DO 200 I=1,N
         I1 = I+1
         Y(I) = PRT
         EFSSS=0.0D0
         IF (I .GE. N)  GO TO 150
         DO 140 J=I1,N
  140    Y(J) = 0.0D0
  150    CONTINUE
         DO 190 J=I,N
            II = NP2-J
            JJ = NP1-J
            IF (ABS(Y(J)) .LT. ABS(R(II,JJ)))  GO TO 160
            TEMP = Y(J)*SQRT(1.0D0+(R(II,JJ)/Y(J))**2)
            GO TO 170
  160       TEMP = R(II,JJ)*SQRT(1.0D0+(Y(J)/R(II,JJ))**2)
  170       CONTINUE
            SIN = R(II,JJ)/TEMP
            COS = Y(J)/TEMP
            R(II,JJ) = TEMP
            TEMP = EFS(J)
            EFS(J)=SIN*TEMP+COS*EFSSS
            EFSSS=SIN*EFSSS-COS*TEMP
            IF (J .GE. N)  GO TO 200
            J1 = J+1
            DO 180 K=J1,N
               JJ = NP1-K
               TEMP = R(II,JJ)
               R(II,JJ) = SIN*TEMP+COS*Y(K)
  180       Y(K) = SIN*Y(K)-COS*TEMP
  190    CONTINUE
  200 CONTINUE
      P(N) = EFS(N)/R(2,1)
      I = N
  210 I = I-1
      IF (I)  240,240,220
  220 TEMP = EFS(I)
      K = I+1
      II = NP2-I
      DO 230 J=K,N
         JJ = NP1-J
  230 TEMP = TEMP-R(II,JJ)*P(J)
      JJ = NP1-I
      P(I) = TEMP/R(II,JJ)
      GO TO 210
  240 CONTINUE
      GO TO 270
C     **********
C     SIDESTEP SECTION
C     **********
  250 JRST = JRST+1
      NSST=NSST+1
      IF(NSST.GE.IXSO) GO TO 710
      IF (JRST .GT. N)  JRST=2
      IRST = 0
C     **********
C     PRODUCTION OF A VECTOR ORTHOGONAL TO THE LAST P-VECTOR
C     **********
      WORK = PN*(ABS(P(1))+PN)
      TEMP = P(JRST)
      P(1) = TEMP*(P(1)+SIGN(PN,P(1)))
      DO 260 I=2,N
  260 P(I) = TEMP*P(I)
      P(JRST) = P(JRST)-WORK
C     **********
C     COMPUTE NORM AND NORM-SQUARE OF THE P-VECTOR
C     **********
  270 PNLAST = PN
      PN=0.D0
      PN2 = 0.0D0
      DO 280 I=1,N
         PN=PN+ABS(P(I))
  280 PN2 = PN2+P(I)**2
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
      DO 290 I=1,N
         EFS(I)=X(I)
  290 CONTINUE
C     **********
C     PERFORM LINE-MINIMIZATION FROM POINT X IN DIRECTION P OR -P
C     **********
      SSQLST = SSQ
      DO 300 I=1,N
         EFS(I)=0.D0
  300 XLAST(I)=X(I)
      CALL LOCMIN(M,X,N,P,SSQ,ALF,EFS,IERR,ESCF)
      IF(SSQLST .LT. SSQ ) THEN
         IF(IERR .EQ. 0)      SSQ=SSQLST
         DO 310 I=1,N
  310    X(I)=XLAST(I)
         IRST=NRST
         PN=PNLAST
         TIME2=TIME1
         TIME1=SECOND()
         TCYCLE=TIME1-TIME2
         TLEFT=TLEFT-TCYCLE
         IF(TLEFT .GT. TCYCLE*2) GO TO 100
         GOTO 670
      ENDIF
      IREPET=0
C     **********
C     PRODUCE THE VECTOR   R*P
C     **********
      DO 330 I=1,N
         TEMP = 0.0D0
         DO 320 J=I,N
  320    TEMP = TEMP+R(I,J)*P(J)
  330 Y(I) = TEMP
C     **********
C     PRODUCE THE VECTOR ...
C                  Y  =    (EFS-EFSLST-ALF*Q*R*P)/(ALF*(NORMSQUARE(P))
C     COMPUTE NORM OF THIS VECTOR AS WELL
C     **********
      WORK = ALF*PN2
      YN = 0.0D0
      DO 350 I=1,M
         TEMP = 0.0D0
         DO 340 J=1,N
  340    TEMP = TEMP+Q(I,J)*Y(J)
         TEMP = (EFS(I)-EFSLST(I)-ALF*TEMP)
         EFSLST(I) = EFS(I)
         YN = YN+TEMP**2
  350 EFS(I) = TEMP/WORK
      YN = SQRT(YN)/WORK
C     **********
C     THE BROYDEN UPDATE   NEW MATRIX = OLD MATRIX + Y*(P-TRANS)
C     HAS BEEN FORMED.  IT IS NOW NECESSARY TO UPDATE THE  QR DECOMP.
C     FIRST LET    Y = (Q-TRANS)*Y.
C     **********
      DO 370 I=1,M
         TEMP = 0.0D0
         DO 360 J=1,M
  360    TEMP = TEMP+Q(J,I)*EFS(J)
  370 Y(I) = TEMP
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
      IF (M .LE. NP1)  GO TO 430
C
C THE NEXT THREE LINES WERE INSERTED TO TRY TO GET ROUND OVERFLOW BUGS.
C
      CONST=1.D-12
      DO 380 I=NP1,M
  380 CONST=MAX(ABS(Y(NP1)),CONST)
      YTAIL = 0.0D0
      DO 390 I=NP1,M
  390 YTAIL = YTAIL+(Y(I)/CONST)**2
      YTAIL = SQRT(YTAIL)*CONST
      BET = (1.0D25/YTAIL)/(YTAIL+ABS(Y(NP1)))
      Y(NP1) = SIGN (YTAIL+ABS(Y(NP1)),Y(NP1))
      DO 420 I=1,M
         TMP = 0.0D0
         DO 400 J=NP1,M
  400    TMP = TMP+Q(I,J)*Y(J)*1.D-25
         TMP = BET*TMP
         DO 410 J=NP1,M
  410    Q(I,J) = Q(I,J)-TMP*Y(J)
  420 CONTINUE
      Y(NP1) = YTAIL
      I = NP1
      GO TO 440
  430 CONTINUE
      I = M
  440 CONTINUE
  450 J = I
      I = I-1
      IF (I)  520,520,460
  460 IF (Y(J))  470,450,470
  470 IF (ABS(Y(I)) .LT. ABS(Y(J)))  GO TO 480
      TEMP = ABS(Y(I))*SQRT(1.0D0+(Y(J)/Y(I))**2)
      GO TO 490
  480 TEMP = ABS(Y(J))*SQRT(1.0D0+(Y(I)/Y(J))**2)
  490 COS = Y(I)/TEMP
      SIN = Y(J)/TEMP
      Y(I) = TEMP
      DO 500 K=1,M
         TEMP = COS*Q(K,I)+SIN*Q(K,J)
         WORK = -SIN*Q(K,I)+COS*Q(K,J)
         Q(K,I) = TEMP
  500 Q(K,J) = WORK
      IF (I .GT. N)  GO TO 450
      R(J,I) = -SIN*R(I,I)
      R(I,I) = COS*R(I,I)
      IF (J .GT. N)  GO TO 450
      DO 510 K=J,N
         TEMP = COS*R(I,K)+SIN*R(J,K)
         WORK = -SIN*R(I,K)+COS*R(J,K)
         R(I,K) = TEMP
  510 R(J,K) = WORK
      GO TO 450
  520 CONTINUE
C     **********
C     REDUCE THE UPPER-HESSENBERG MATRIX TO UPPER-TRIANGULAR FORM
C     USING ELEMENTARY ROTATIONS.  APPLY THE SAME ROTATIONS, TRANSPOSED,
C     ON THE RIGHT TO THE MATRIX  Q.
C     **********
      DO 530 K=1,N
  530 R(1,K) = R(1,K)+YN*P(K)
      JEND = NP1
      IF (M .EQ. N)  JEND=N
      DO 600 J=2,JEND
         I = J-1
         IF (R(J,I))  540,600,540
  540    IF (ABS(R(I,I)) .LT. ABS(R(J,I)))  GO TO 550
         TEMP = ABS(R(I,I))*SQRT(1.0D0+(R(J,I)/R(I,I))**2)
         GO TO 560
  550    TEMP = ABS(R(J,I))*SQRT(1.0D0+(R(I,I)/R(J,I))**2)
  560    COS = R(I,I)/TEMP
         SIN = R(J,I)/TEMP
         R(I,I) = TEMP
         IF (J .GT. N)  GO TO 580
         DO 570 K=J,N
            TEMP = COS*R(I,K)+SIN*R(J,K)
            WORK = -SIN*R(I,K)+COS*R(J,K)
            R(I,K) = TEMP
  570    R(J,K) = WORK
  580    DO 590 K=1,M
            TEMP = COS*Q(K,I)+SIN*Q(K,J)
            WORK = -SIN*Q(K,I)+COS*Q(K,J)
            Q(K,I) = TEMP
  590    Q(K,J) = WORK
  600 CONTINUE
C     **********
C     CHECK THE STOPPING CRITERIA
C     **********
      TEMP = 0.0D0
      DO 610 I=1,N
  610 TEMP = TEMP+X(I)**2
      TOLX = TOLS1*SQRT(TEMP)+TOLS2
      IF (SQRT(ALF*PN2) .LE. TOLX)  GO TO 690
      IF(SSQ.GE.2.D0*N) GO TO 630
      DO 620 I=1,N
C*****
C     The stopping criterion is that no individual gradient be
C         greater than 0.2
C*****
         IF(ABS(EFSLST(I)).GE.0.2D0) GO TO 630
  620 CONTINUE
      WRITE(6,730) SSQ
      GO TO 700
  630 CONTINUE
      IF (ICYC .GE. MXCYCL)  THEN
         IFLEPO=12
         GOTO 920
      ENDIF
      TIME2=TIME1
      TIME1=SECOND()
      TCYCLE=TIME1-TIME2
      TLEFT=TLEFT-TCYCLE
      IF(RESFIL)THEN
         WRITE(6,640)TLEFT,SQRT(SSQ),ESCF
  640    FORMAT('  RESTART FILE WRITTEN,  TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
         RESFIL=.FALSE.
      ELSE
         WRITE(6,650)ICYC,TCYCLE,TLEFT,SQRT(SSQ),ESCF
  650    FORMAT(' CYCLE:',I5,' TIME:',F6.1,' TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
      ENDIF
      IF(TLAST-TLEFT.GT.TDUMP)THEN
         TLAST=TLEFT
         RESFIL=.TRUE.
         DO 660 I=1,N
  660    XLAST(I)=X(I)
         IIIUM(1)=NSCF
         CALL PARSAV(2,N,M)
      ENDIF
      IF(TLEFT .GT. TCYCLE*2) GO TO 100
  670 IIIUM(5)=NCOUNT
      DO 680 I=1,N
  680 XLAST(I)=X(I)
      IIIUM(1)=NSCF
      CALL PARSAV(1,N,M)
      STOP
  690 WRITE (6,810)  NCOUNT
      GOTO 920
  700 WRITE (6,820)  NCOUNT
      GOTO 920
  710 CONTINUE
      WRITE(6,720) IXSO
  720 FORMAT(1H ,5X,'ATTEMPT TO GO DOWNHILL IS UNSUCCESSFUL AFTER',I5,5X
     1,'ORTHOGONAL SEARCHES')
      GOTO 920
  730 FORMAT(1H ,'SSQ =',F15.7)
  740 FORMAT(1H ,3X,'ALF =',E12.4)
  750 FORMAT(1H ,3X,'NCOUNT =',I5)
  760 FORMAT(3X,'TIME LEFT:',F7.1,' CYCLE',I5,3X,'GNORM SQUARED IS'
     1,F13.5)
  770 FORMAT(4(5X,'X(',I2,') = ',E15.8))
  780 FORMAT(4(5X,'P(',I2,') = ',E15.8))
  790 FORMAT(5X,'R-MATRIX DIAGONAL ENTRIES ...')
  800 FORMAT(6E13.3)
  810 FORMAT('0TEST ON X SATISFIED, NUMBER OF FUNCTION CALLS = ',I5)
  820 FORMAT('0TEST ON SSQ SATISFIED, NUMBER OF FUNCTION CALLS = ',I5)
  830 FORMAT(' ///// NEXT CYCLE IS A SIDE-STEP ALONG THE ',I2,
     1  '-TH NORMAL TO P')
  840 FORMAT('0ALLOWED NUMBER OF FUNCTION CALLS EXCEEDED.'/
     1  ' NUMBER OF FUNCTION CALLS WAS ',I5)
  850 FORMAT('  L.-M. PARAMETER = ',E15.7,
     1  '   SUMSQUARES CHANGE = ',E15.7)
  860 FORMAT(1H )
  870 FORMAT(1H )
  880 FORMAT(1H ,3X,'I',7X,I2,9(10X,I2))
  890 FORMAT(1H ,1X,'X(I)',1X,F10.5,2X,9(F10.5,2X))
  900 FORMAT(1H ,1X,'G(I)',1X,F10.5,2X,9(F10.5,2X))
  910 FORMAT(1H ,1X,'P(I)',1X,F10.5,2X,9(F10.5,2X))
  920 LAST=1
      CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFSLST,.TRUE.)
      RETURN
      END
