      SUBROUTINE SEARCH(XPARAM,ALPHA,NVAR,GMIN,OK) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION XPARAM(*) 
************************************************************************ 
* 
* SEARCH PERFORMS A LINE SEARCH FOR POWSQ. IT MINIMISES THE NORM OF 
*        THE GRADIENT VECTOR IN THE DIRECTION SIG. 
* 
* ON INPUT  XPARAM = CURRENT POINT IN NVAR DIMENSIONAL SPACE. 
*           ALPHA  = INITIAL STEP SIZE ALONG SIG. 
*           SIG    = SEARCH DIRECTION VECTOR IN /OPTIM/. 
*           NVAR   = NUMBER OF PARAMETERS IN SIG (& XPARAM) 
*           GMIN   = NOT DEFINED. 
*           GMIN1  = GRADIENT VECTOR AT CURRENT POINT. 
* 
* ON OUTPUT XPARAM = NEW CURRENT POINT (MINIMUM IN DIRECTION SIG). 
*           ALPHA  = OPTIMUM STEP SIZE FOUND 
*           GMIN   = GRADIENT NORM AT MINIMUM. 
*           GMIN1  = GRADIENT VECTOR AT MINIMUM. 
*           OK     =.TRUE. IF IMPROVEMENT IN GRADIENT NORM. 
*           GNEXT1 = INTERMEDIATE GRADIENT ALONG SEARCH. WILL BE USED 
*                    TO UPDATE THE HESSIAN MATRIX IN 'POWSQ'. 
************************************************************************ 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IDUM, LOOP, IDUMMY(17 + 2*NCHAIN)                3GL3092
      COMMON /OPTMCR / HESS(MAXPAR,MAXPAR),PMAT(MAXHES),                3GL3092
     1                 BMAT(MAXPAR,MAXPAR),GMIN1(MAXPAR),GMIDUM,        3GL3092 
     2                 SIG(MAXPAR),XBEST(MAXPAR),GNEXT,AMIN,            3GL3092 
     3                 ANEXT,PVEC(MAXPAR*MAXPAR),EIG(MAXPAR),P(MAXPAR), 3GL3092 
     4                 Q(MAXPAR),WORK(MAXPAR),GNEXT1(MAXPAR),           3GL3092 
     5                 GRAD(MAXPAR),XREF(MAXPAR),GREF(MAXPAR),          3GL3092 
     6                 XMIN1(MAXPAR),                                   3GL3092 
     7                 RDUMY(MAXPAR*(NCHAIN + 4) + NCHAIN + 25)         3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,HESS(MAXPAR,MAXPAR),PMAT(MAXHES) 
C    1               ,BMAT(MAXPAR,MAXPAR),GMIN1(MAXPAR),GMIDUM 
C    2               ,IDUM,LOOP,SIG(MAXPAR),XBEST(MAXPAR),GNEXT,AMIN 
C    3               ,ANEXT,PVEC(MAXPAR*MAXPAR),EIG(MAXPAR),P(MAXPAR) 
C    4               ,Q(MAXPAR),WORK(MAXPAR),GNEXT1(MAXPAR) 
C    5               ,GRAD(MAXPAR),XREF(MAXPAR),GREF(MAXPAR) 
C    6               ,XMIN1(MAXPAR) 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      LOGICAL DEBUG, FAIL, OK 
      SAVE
      IF (ICONTR(13).EQ.1) THEN                                         GDH0195
         ICONTR(13)=2                                                   GDH0195
C 
C    TOLG   = CRITERION FOR EXIT BY RELATIVE CHANGE IN GRADIENT. 
C 
         DEBUG=(ISEARC.NE.0)                                            DJG0995
         LOOKS=0 
         TINY=0.1D0 
         TOLERG=0.02D0 
         G=100.D0 
         ALPHA=0.1D0 
      ENDIF 
      OK=.FALSE. 
      DO 10 I=1,NVAR 
         GREF(I)  =GMIN1(I) 
         GNEXT1(I)=GMIN1(I) 
         XMIN1(I) =XPARAM(I) 
   10 XREF(I)  =XPARAM(I) 
      IF(ABS(ALPHA) .GT. 0.2)ALPHA=SIGN(0.2D0,ALPHA) 
      IF(DEBUG) THEN 
         WRITE(JOUT,'('' SEARCH DIRECTION VECTOR'')') 
         WRITE(JOUT,'(6F12.6)')(SIG(I),I=1,NVAR) 
         WRITE(JOUT,'('' INITIAL GRADIENT VECTOR'')') 
         WRITE(JOUT,'(6F12.6)')(GMIN1(I),I=1,NVAR) 
      ENDIF 
      GB=DOT(GMIN1,GREF,NVAR) 
      IF(DEBUG) WRITE(JOUT,'('' GRADIENT AT START OF SEARCH:'',F16.6)') 
     1SQRT(GB) 
      GSTORE=GB 
      AMIN=0.D0 
      GMINN=1.D9 
C 
C 
      TA=0.D0 
      GA=GB 
      GB=1.D9 
      ITRYS=0 
      GOTO 30 
   20 SUM=GA/(GA-GB) 
      ITRYS=ITRYS+1 
      IF(ABS(SUM) .GT. 3.D0) SUM=SIGN(3.D0,SUM) 
      ALPHA=(TB-TA)*SUM+TA 
C 
C         XPARAM IS THE GEOMETRY OF THE PREDICTED MINIMUM ALONG THE LINE 
C 
   30 DO 40 I=1,NVAR 
   40 XPARAM(I)=XREF(I)+ALPHA*SIG(I) 
C 
C         CALCULATE GRADIENT NORM AND GRADIENTS AT THE PREDICTED MINIMUM 
C 
      CALL COMPFG (XPARAM,FUNCT,FAIL, GRAD, .TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=27                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
      LOOKS=LOOKS+1 
C 
C          G IS THE PROJECTION OF THE GRADIENT ALONG SIG. 
C 
      G=DOT(GREF,GRAD,NVAR) 
      GTOT=SQRT(DOT(GRAD,GRAD,NVAR)) 
      IF(DEBUG.OR.IMP.GT.1) 
     1WRITE(JOUT,'('' LOOKS'',I3,'' ALPHA ='',F12.6,'' GRADIENT'',F12.3, 
     2'' G  ='',F16.6)') LOOKS,ALPHA,GTOT,G 
      IF(GTOT .LT. GMINN) THEN 
         GMINN=GTOT 
         IF(ABS(AMIN-ALPHA) .GT.1.D-2) THEN 
* 
* WE CAN MOVE ANEXT TO A POINT NEAR, BUT NOT TOO NEAR, AMIN, SO THAT THE 
* SECOND DERIVATIVESWILLBEREALISTIC(D2E/DX2=(GNEXT1-GMIN1)/(ANEXT-AMIN)) 
* 
            ANEXT=AMIN 
            CALL SCOPY (NVAR,GMIN1,1,GNEXT1,1) 
         ENDIF 
         AMIN=ALPHA 
         CALL SCOPY (NVAR,GRAD,1,GMIN1,1) 
         IF (GMINN.LT.GMIN) THEN 
            CALL SCOPY (NVAR,XPARAM,1,XMIN1,1) 
            GMIN=GMINN 
         ENDIF 
      ENDIF 
      IF(ITRYS .GT. 8) GOTO 50 
      IF (ABS(G/GSTORE).LT.TINY .OR. ABS(G) .LT. TOLERG) GO TO 50 
      IF(ABS(G) .LT. MAX(ABS(GA),ABS(GB)) .OR. 
     1     GA*GB .GT. 0.D0 .AND. G*GA .LT. 0.D0) THEN 
C   G IS AN IMPROVEMENT ON GA OR GB. 
         OK=.TRUE. 
         IF(ABS(GB) .LT. ABS(GA))THEN 
            TA=ALPHA 
            GA=G 
            GO TO 20 
         ELSE 
            TB=ALPHA 
            GB=G 
            GO TO 20 
         ENDIF 
      ELSE 
         IF(IMP.GT.0.AND..NOT.OK) 
     .   WRITE(JOUT,'(//10X,'' FAILED IN SEARCH, SEARCH CONTINUING'')') 
      ENDIF 
   50 GMINN=SQRT(DOT(GMIN1,GMIN1,NVAR)) 
      GMIN=GMINN 
      CALL SCOPY (NVAR,XMIN1,1,XPARAM,1) 
      IF(DEBUG.OR.IMP.GT.3) THEN 
         WRITE(JOUT,'('' AT EXIT FROM SEARCH'')') 
         WRITE(JOUT,'('' XPARAM'',6F12.6)')(XPARAM(I),I=1,NVAR) 
         WRITE(JOUT,'('' GNEXT1'',6F12.6)')(GNEXT1(I),I=1,NVAR) 
         WRITE(JOUT,'('' GMIN1 '',6F12.6)')(GMIN1 (I),I=1,NVAR) 
         WRITE(JOUT,'('' AMIN, ANEXT,GMIN'',4F12.6)')AMIN,ANEXT,GMIN 
      ENDIF 
      RETURN 
      END 
