      SUBROUTINE LOCMIN(M,X,N,P,EFS,ITRAP,ESCF) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION X(*), P(*), EFS(*) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IDUM(5), NCOUNT, IDUMMY(19 + 2*NCHAIN - 6)       3GL3092
      COMMON /OPTMCR / AREA(MAXPAR*(2*MAXPAR+2)),ALF,SSQ,PN,            3GL3092
     1                 WORK1(3*MAXPAR),XSTOR(MAXPAR),GSTOR(MAXPAR),     3GL3092
     2                 PHI(3),VT(3),                                    3GL3092 
     3                RDUMY(MAXHES  + MAXPAR*(3*MAXPAR+NCHAIN+16) +     3GL3092
     4                      29 + NCHAIN - MAXPAR*(2*MAXPAR + 7) - 9)    3GL3092
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      INTEGER LEFT,RIGHT,CENTER 
      LOGICAL DEBUG, LOWER, FAIL 
       SAVE
************************************************************************ 
* 
*    LOCMIN IS CALLED BY NLLSQ ONLY. IT IS A LINE-SEARCH PROCEDURE FOR 
*    LOCATING A MINIMUM OF THE GRADIENT NORM OF THE FUNCTION COMPFG. 
*    SEE NLLSQ FOR MORE DETAILS. 
* 
************************************************************************ 
      IF (ICONTR(8).EQ.1) THEN                                          GDH0195
         ICONTR(8)=2                                                    GDH0195
         XMAXM=1.D9 
C 
C THE ABOVE LINE IS TO TRY TO PREVENT OVERFLOW IN NLLSQ 
C 
         EPS=1.D-5 
         DEBUG=ILOCMI.NE.0                                              DJG0995
         TEE=1.D-2 
         YMAXST=0.005D0 
         XCRIT=0.0002D0 
         MXCNT2=30 
      ENDIF 
      XMAXM=1.D-11 
      DO 10 I=1,N 
   10 XMAXM=MAX(XMAXM,ABS(P(I))) 
      XMINM=XMAXM 
      XMAXM=YMAXST/XMAXM 
      FIN = SSQ 
      LOWER = .FALSE. 
      T=ALF 
      PHI(1) = SSQ 
      VT(1) = 0.0D0 
      VT(2) = T/4.0D0 
      IF(VT(2).GT.XMAXM) VT(2)=XMAXM 
      T = VT(2) 
      CALL SAXPY (N,T,P,1,X,1) 
      CALL COMPFG(X,ESCF,FAIL,EFS,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=17                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
      PHI(2)=DOT(EFS,EFS,N) 
      CALL EXCHNG(PHI(2),SQSTOR,ENERGY,ESTOR,X,XSTOR,T,ALFS,N) 
      CALL SCOPY (N,EFS,1,GSTOR,1) 
      IF (PHI(1) .LE. PHI(2)) THEN 
         VT(3) = -VT(2) 
         LEFT = 3 
         CENTER = 1 
         RIGHT = 2 
      ELSE 
         VT(3)=2.0D0*VT(2) 
         LEFT = 1 
         CENTER = 2 
         RIGHT = 3 
      ENDIF 
      TLAST = VT(3) 
      T = TLAST-T 
      CALL SAXPY (N,T,P,1,X,1) 
      FLAST=PHI(2) 
      CALL COMPFG(X,ESCF,FAIL,EFS,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=18                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
      F=DOT(EFS,EFS,N) 
      IF(F.LT.SQSTOR) CALL EXCHNG(F,SQSTOR,ENERGY,ESTOR,X, 
     1XSTOR,T,ALFS,N) 
      CALL SCOPY (M,EFS,1,GSTOR,1) 
      IF(F.LT.FIN) LOWER = .TRUE. 
      NCOUNT = NCOUNT+2 
      PHI(3) = F 
      IF (DEBUG.OR.IMP.GT.1) 
     .        WRITE (JOUT,1000) VT(1),PHI(1),VT(2),PHI(2),VT(3),PHI(3) 
      MXCT=MXCNT2 
      DO 500 ICTR=3,MXCT 
         XMAXM=XMAXM*3.D0 
         ALPHA = VT(2) - VT(3) 
         BETA = VT(3) - VT(1) 
         GAMMA = VT(1)-VT(2) 
         IF(ALPHA.EQ.0.D0)ALPHA=1.D-20 
         IF(BETA.EQ.0.D0)BETA=1.D-20 
         IF(GAMMA.EQ.0.D0)GAMMA=1.D-20 
         ABG =-(PHI(1)*ALPHA+PHI(2)*BETA+PHI(3)*GAMMA)/ALPHA 
         ABG=ABG/BETA 
         ABG=ABG/GAMMA 
         ALPHA=ABG 
         BETA = ((PHI(1)-PHI(2))/GAMMA)-ALPHA*(VT(1)+VT(2)) 
         IF (ALPHA.LE.0.D0) THEN 
            IF (PHI(RIGHT) .LE. PHI(LEFT)) THEN 
               T = 3.0D0*VT(RIGHT)-2.0D0*VT(CENTER) 
            ELSE 
               T = 3.0D0*VT(LEFT)-2.0D0*VT(CENTER) 
            ENDIF 
            S=T-TLAST 
            T=S+TLAST 
         ELSE 
            T = -BETA/(2.0D0*ALPHA) 
            S=T-TLAST 
            IF (S) 20,600,30 
   20       AMDIS=VT(LEFT)-TLAST-XMAXM 
            GO TO 40 
   30       AMDIS=VT(RIGHT)-TLAST+XMAXM 
   40       IF(ABS(S).GT.ABS(AMDIS)) S=AMDIS 
            T=S+TLAST 
         ENDIF 
         IF(ICTR.GT.3.AND.ABS(S*XMINM).LT.XCRIT) THEN 
            IF(DEBUG.OR.IMP.GT.2) 
     .      WRITE(JOUT,'('' EXIT DUE TO SMALL PROJECTED STEP'')') 
            GO TO 600 
         ENDIF 
         T=S+TLAST 
         CALL SAXPY (N,S,P,1,X,1) 
         FLAST=F 
         CALL COMPFG(X,ESCF,FAIL,EFS,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=19                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
         F=DOT(EFS,EFS,N) 
         IF(F.LT.SQSTOR) CALL EXCHNG(F,SQSTOR,ENERGY,ESTOR,X,XSTOR, 
     1T,ALFS,N) 
         CALL SCOPY (M,EFS,1,GSTOR,1) 
         IF(F.LT.FIN) LOWER = .TRUE. 
         NCOUNT = NCOUNT+1 
         IF (DEBUG.OR.IMP.GT.1) 
     .    WRITE (JOUT,1010) VT(LEFT),PHI(LEFT),VT(CENTER),PHI(CENTER), 
     .  VT(RIGHT),PHI(RIGHT),T,F 
C 
C    TEST FOR EXCITED STATES AND POTHOLES 
C 
         ITRAP=0 
         IF(ABS(VT(CENTER)).GT.1.D-10) GOTO 50 
         IF(ABS(T)/(ABS(VT(LEFT))+1.D-15).GT.0.3333) GOTO 50 
         IF(2.5D0*F-PHI(RIGHT)-PHI(LEFT).LT.0.5D0*PHI(CENTER)) GOTO 50 
C 
C   WE ARE STUCK ON A FALSE MINIMUM 
C 
         ITRAP=1 
         GOTO 600 
  50    CONTINUE 
* 
* NOW FOR THE MAIN STOPPING TESTS.  LOCMIN WILL STOP IF:- 
*     THE ERROR FUNCTION HAS BEEN REDUCED, AND 
*     THE RATE OF DROP OF THE ERROR FUNCTION IS LESS THAN 0.5% PER STEP 
*     AND 
*     (A) THE RATIO OF THE PROPOSED STEP TO THE TOTAL STEP IS LESS THAN 
*         EPS,   OR 
*     (B) THE LAST DROP IN ERROR FUNCTION WAS LESS THAN 5%OFTHETOTALDROP 
*         DURING THIS CALL TO LOCMIN. 
* 
         IF(DEBUG)WRITE(JOUT,'('' F/FLAST'',F13.6)')F/FLAST 
         IF( LOWER  .AND. F/FLAST .GT. 0.995D0) THEN 
            IF((ABS(T-TLAST).LE.EPS*ABS(T+TLAST)+TEE)) THEN 
               IF(DEBUG.OR.IMP.GT.1) 
     1       WRITE(JOUT,'('' EXIT AS STEP IS ABSOLUTELY SMALL '')') 
               GO TO 600 
            ENDIF 
            SUM=MIN(ABS(F-PHI(1)),ABS(F-PHI(2)),ABS(F-PHI(3))) 
            SUM2=(FIN-SQSTOR)*0.05D0 
            IF(SUM .LT. SUM2) THEN 
               IF(DEBUG.OR.IMP.GT.1) 
     1        WRITE(JOUT,'('' EXIT DUE TO HAVING REACHED BOTTOM'')') 
               GOTO 600 
            ENDIF 
         ENDIF 
         TLAST = T 
         IF ((T .GT. VT(RIGHT)) .OR. (T .GT. VT(CENTER) .AND. F .LT. 
     1  PHI(CENTER)) .OR. (T .GT. VT(LEFT) .AND. T .LT. VT(CENTER) .AND. 
     2  F .GT. PHI(CENTER))) THEN 
            VT (LEFT) =T 
            PHI(LEFT) =F 
         ELSE 
            VT (RIGHT)=T 
            PHI(RIGHT)=F 
         ENDIF 
         IF (VT(CENTER) .GE. VT(RIGHT)) THEN 
            I = CENTER 
            CENTER = RIGHT 
            RIGHT = I 
         ENDIF 
         IF (VT(LEFT ) .GE. VT(CENTER)) THEN 
            I = LEFT 
            LEFT = CENTER 
            CENTER = I 
         ENDIF 
         IF (VT(CENTER) .GE. VT(RIGHT)) THEN 
            I = CENTER 
            CENTER = RIGHT 
            RIGHT = I 
         ENDIF 
  500 CONTINUE 
C 
  600 CALL EXCHNG(SQSTOR,F,ESTOR,ENERGY,XSTOR,X,ALFS,T,N) 
      CALL SCOPY (M,GSTOR,1,EFS,1) 
      SSQ=(F) 
      ALF=T 
      IF (T.LT.0.D0) THEN 
         T = -T 
         DO 610 I=1,N 
  610    P(I) = -P(I) 
      ENDIF 
      ALF=T 
      RETURN 
 1000 FORMAT(' ---LOCMIN'/5X,'LEFT   ...',2F19.6/5X,'CENTER ...', 
     1  2F19.6/5X,'RIGHT  ...',2F19.6/' ') 
 1010 FORMAT(5X,'LEFT   ...',2F19.6/5X,'CENTER ...',2F19.6/5X, 
     1  'RIGHT  ...',2F19.6/5X,'NEW    ...',2F19.6/' ') 
      END 
