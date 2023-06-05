      SUBROUTINE LOCMIN(M,X,N,P,SSQ,ALF,EFS,ITRAP,ESCF,*)               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /NLLSQI/ NCOUNT
      COMMON /KEYWRD/ CONTRL
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION X(*), P(*), EFS(*)
      DIMENSION CONST(MAXPAR), XSTOR(MAXPAR), GSTOR(MAXPAR)
      DIMENSION PHI(3),VT(3)
      INTEGER LEFT,RIGHT,CENTER
      CHARACTER*160 KEYWRD
      CHARACTER*80 CONTRL
      LOGICAL DEBUG, LOWER
         SAVE                                                           GL0892
      DATA ICALCN /0/
      DATA CONST/MAXPAR*1.D0/
************************************************************************
*
*    LOCMIN IS CALLED BY NLLSQ ONLY. IT IS A LINE-SEARCH PROCEDURE FOR
*    LOCATING A MINIMUM IN THE FUNCTION SPACE OF COMPFG.  SEE NLLSQ
*    FOR MORE DETAILS
*
************************************************************************
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
         XMAXM=1.D9
         SCALE=1.D0
         KEYWRD=CONTRL
C
C THE ABOVE LINE IS TO TRY TO PREVENT OVERFLOW IN NLLSQ
C
         EPS=1.D-5
         DEBUG=(INDEX(KEYWRD,'LINMIN') .NE. 0)
         TEE=1.D-2
         YMAXST=0.005D0
         XCRIT=0.0002D0
         MXCNT2=30
         IPRINT=0
         IF(DEBUG)IPRINT=-1
      ENDIF
      XMAXM=1.D-11
      DO 10 I=1,N
   10 XMAXM=MAX(XMAXM,ABS(P(I)))
      XMINM=XMAXM*SCALE
      XMAXM=YMAXST/XMAXM/SCALE
      FIN = SSQ
      LOWER = .FALSE.
      T=ALF
      PHI(1) = SSQ
      VT(1) = 0.0D0
      VT(2) = T/4.0D0
      IF(VT(2).GT.XMAXM) VT(2)=XMAXM
      T = VT(2)
      DO 20 I=1,N
   20 X(I) = X(I)+T*P(I)*CONST(I)*SCALE
      CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFS,.TRUE.,*9999)                 CSTP(call)
      phi(2) = ddot(n,efs,1,efs,1)
      sqstor = phi(2)
      estor = energy
      alfs = t
      do 25 iexch = 1,n
      xstor(iexch) = x(iexch)
25    continue
      DO 30 I=1,M
   30 GSTOR(I)=EFS(I)
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
      DO 40 I=1,N
   40 X(I) = X(I)+T*P(I)*CONST(I)*SCALE
      FLAST=PHI(2)
      CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFS,.TRUE.,*9999)                 CSTP(call)
      f = ddot(n,efs,1,efs,1)
      if(f .lt. sqstor) then
        sqstor = f
        estor = energy
        alfs = t
        do 45 iexch = 1,n
        xstor(iexch) = x(iexch)
45      continue
      endif
      DO 50 I=1,M
   50 GSTOR(I)=EFS(I)
      IF(F.LT.FIN) LOWER = .TRUE.
      NCOUNT = NCOUNT+2
      PHI(3) = F
      IF (IPRINT) 60,70,70
   60 IF (DOPRNT) WRITE (6,310) VT(1),PHI(1),VT(2),PHI(2),VT(3),PHI(3)  CIO
   70 MXCT=MXCNT2
      DO 250 ICTR=3,MXCT
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
         IF (ALPHA)  80,80,110
   80    IF (PHI(RIGHT) .GT. PHI(LEFT))  GO TO 90
         T = 3.0D0*VT(RIGHT)-2.0D0*VT(CENTER)
         GO TO 100
   90    T = 3.0D0*VT(LEFT)-2.0D0*VT(CENTER)
  100    S=T-TLAST
         T=S+TLAST
         GO TO 150
  110    T = -BETA/(2.0D0*ALPHA)
         S=T-TLAST
         IF (S) 120,260,130
  120    AMDIS=VT(LEFT)-TLAST-XMAXM
         GO TO 140
  130    AMDIS=VT(RIGHT)-TLAST+XMAXM
  140    IF(ABS(S).GT.ABS(AMDIS)) S=AMDIS
         T=S+TLAST
  150    CONTINUE
         IF(ICTR.GT.3.AND.ABS(S*XMINM).LT.XCRIT) THEN
            IF( DOPRNT.AND.DEBUG )                                      CIO
     1    WRITE(6,'('' EXIT DUE TO SMALL PROJECTED STEP'')')
            GO TO 260
         ENDIF
         T=S+TLAST
         DO 160 I=1,N
  160    X(I) = X(I)+S*P(I)*CONST(I)*SCALE
         FLAST=F
         CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFS,.TRUE.,*9999)              CSTP(call)
      f = ddot(n,efs,1,efs,1)
      if(f .lt. sqstor) then
        sqstor = f
        estor = energy
        alfs = t
        do 165 iexch = 1,n
        xstor(iexch) = x(iexch)
165     continue
      endif
         DO 170 I=1,M
  170    GSTOR(I)=EFS(I)
         IF(F.LT.FIN) LOWER = .TRUE.
         NCOUNT = NCOUNT+1
         IF (IPRINT) 180,190,190
  180    IF (DOPRNT) WRITE (6,320) VT(LEFT),PHI(LEFT),VT(CENTER),       CIO
     &                     PHI(CENTER),VT(RIGHT),PHI(RIGHT),T,F         CIO
  190    CONTINUE
C
C    TEST FOR EXCITED STATES AND POTHOLES
C
         ITRAP=0
         IF(ABS(VT(CENTER)).GT.1.D-10) GOTO 200
         IF(ABS(T)/(ABS(VT(LEFT))+1.D-15).GT.0.3333) GOTO 200
         IF(2.5D0*F-PHI(RIGHT)-PHI(LEFT).LT.0.5D0*PHI(CENTER)) GOTO 200
C
C   WE ARE STUCK ON A FALSE MINIMUM
C
         ITRAP=1
         GOTO 260
  200    CONTINUE
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
         IF(DOPRNT.AND.DEBUG)WRITE(6,'('' F/FLAST'',F13.6)')F/FLAST     CIO
         IF( LOWER  .AND. F/FLAST .GT. 0.995D0) THEN
            IF((ABS(T-TLAST).LE.EPS*ABS(T+TLAST)+TEE)) THEN
               IF( DOPRNT.AND.DEBUG )                                   CIO
     1       WRITE(6,'('' EXIT AS STEP IS ABSOLUTELY SMALL '')')
               GO TO 260
            ENDIF
            SUM=MIN(ABS(F-PHI(1)),ABS(F-PHI(2)),ABS(F-PHI(3)))
            SUM2=(FIN-SQSTOR)*0.05D0
            IF(SUM .LT. SUM2) THEN
               IF( DOPRNT.AND.DEBUG )                                   CIO
     1        WRITE(6,'('' EXIT DUE TO HAVING REACHED BOTTOM'')')
               GOTO 260
            ENDIF
         ENDIF
         TLAST = T
         IF ((T .GT. VT(RIGHT)) .OR. (T .GT. VT(CENTER) .AND. F .LT.
     1  PHI(CENTER)) .OR. (T .GT. VT(LEFT) .AND. T .LT. VT(CENTER) .AND.
     2  F .GT. PHI(CENTER)))  GO TO 210
         VT(RIGHT) = T
         PHI(RIGHT) = F
         GO TO 220
  210    VT(LEFT) = T
         PHI(LEFT) = F
  220    IF (VT(CENTER) .LT. VT(RIGHT))  GO TO 230
         I = CENTER
         CENTER = RIGHT
         RIGHT = I
  230    IF (VT(LEFT) .LT. VT(CENTER))  GO TO 240
         I = LEFT
         LEFT = CENTER
         CENTER = I
  240    IF (VT(CENTER) .LT. VT(RIGHT))  GO TO 250
         I = CENTER
         CENTER = RIGHT
         RIGHT = I
  250 CONTINUE
  260 CONTINUE
      f = sqstor
      energy = estor
      t = alfs
      do 265 iexch = 1,n
      x(iexch) = xstor(iexch)
265   continue
      DO 270 I=1,M
  270 EFS(I)=GSTOR(I)
      SSQ=(F)
      ALF=T
      IF (T) 280,300,300
  280 T = -T
      DO 290 I=1,N
  290 P(I) = -P(I)
  300 CONTINUE
      ALF=T
  310 FORMAT(' ---LOCMIN'/5X,'LEFT   ...',2F19.6/5X,'CENTER ...',
     1  2F19.6/5X,'RIGHT  ...',2F19.6/' ')
  320 FORMAT(5X,'LEFT   ...',2F19.6/5X,'CENTER ...',2F19.6/5X,
     1  'RIGHT  ...',2F19.6/5X,'NEW    ...',2F19.6/' ')
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY LOCMIN_INIT                                                 CSAV
               ABG = 0.0D0                                              CSAV
              ALFS = 0.0D0                                              CSAV
             ALPHA = 0.0D0                                              CSAV
             AMDIS = 0.0D0                                              CSAV
              BETA = 0.0D0                                              CSAV
            CENTER = 0.0D0                                              CSAV
             CONST = 1.0D0                                              CSAV
                 D = 0.0D0                                              CSAV
                D9 = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
            ENERGY = 0.0D0                                              CSAV
               EPS = 0.0D0                                              CSAV
             ESTOR = 0.0D0                                              CSAV
                 F = 0.0D0                                              CSAV
               FIN = 0.0D0                                              CSAV
             FLAST = 0.0D0                                              CSAV
             GAMMA = 0.0D0                                              CSAV
             GSTOR = 0                                                  CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
              ICTR = 0                                                  CSAV
             IEXCH = 0                                                  CSAV
            IPRINT = 0                                                  CSAV
              LEFT = 0                                                  CSAV
             LOWER = .FALSE.                                            CSAV
            MXCNT2 = 0                                                  CSAV
              MXCT = 0                                                  CSAV
               PHI = 0.0D0                                              CSAV
             RIGHT = 0.0D0                                              CSAV
                 S = 0.0D0                                              CSAV
             SCALE = 0.0D0                                              CSAV
            SQSTOR = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
              SUM2 = 0.0D0                                              CSAV
                 T = 0.0D0                                              CSAV
               TEE = 0.0D0                                              CSAV
             TLAST = 0.0D0                                              CSAV
                VT = 0.0D0                                              CSAV
             XCRIT = 0.0D0                                              CSAV
             XMAXM = 0.0D0                                              CSAV
             XMINM = 0.0D0                                              CSAV
             XSTOR = 0.0D0                                              CSAV
            YMAXST = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
