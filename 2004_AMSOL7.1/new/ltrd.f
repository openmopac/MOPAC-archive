      SUBROUTINE LTRD(IND,X,F,G,N) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
C-------------------- 
C     MAIN ROUTINE FOR OPTIMIZATION BY NEWTON-LIKE METHOD : 
C      - MINIMIZATION OF THE ENERGY         IND=1 ==>(IND=5 OR 7) 
C                OR 
C      - MINIMIZATION OF THE GRADIENT NORM  IND=2 ==>(IND=6 OR 8) . 
C------------ 
C     THE HESSIAN IS CALCULATED BY AN USUAL FINITE DIFFERENCE FORMULA 
C     ACCORDING TO THE NATURE OF THE GRADIENT AND IOPT1 OPTION :        DJG0995
C      - NUMERICAL DERIVATION OF AN ANALYTICAL GRADIENT IF IND =5,6 
C                OR 
C      - DIRECT USE OF THE ENERGY IF IND =7,8 . 
C                         IN EACH CASE ... 
C     THE LENGTH OF THE INCREMENT IS UPDATED TO INSURE A GIVEN PRECISION 
C                ON DIAGONAL ELEMENTS OF THE HESSIAN, 
C     AND A STATISTICAL ERROR ON THE EIGENVALUES IS PROVIDED. 
C------------ 
C     THE ONE-DIMENSIONAL DIRECTION OF OPTIMISATION IS SELECTED 
C     AMONG THE FOLLOWING POSSIBILITIES : 
C      - STEEPEST DESCENT ( INSURING STABILITY ) 
C      - FULL NEWTON      ( INSURING QUADRATIC TERMINATION ) 
C      - EIGENVECTOR ASSOCIATED TO A NEGATIVE EIGENVALUE 
C                         ( INSURING STABILITY NEAR AN INFLEXION ) 
C------------ 
C     THE ONE-DIMENSIONAL OPTIMISATION PROCEEDES BY UP TO 3 DEGREE 
C     POLYNOMIAL EXTRAPOLATION AND TRY TO AVOID SOME USUAL PITFALL 
C     OF MOST SIMILAR PROCEDURE... 
C--------------------- 
C     REQUIRED SUBROUTINES : 
C        LDATA  : GENERAL OPTIONS 
C        POINT1 : HESSIAN MATRIX (ANALYTICAL-NUMERICAL) 
C        POINT3 : HESSIAN MATRIX (NUMERICAL -NUMERICAL) 
C        FINDS  : SELECT THE OPTIMISATION DIRECTION S 
C        STAT   : MAIN SUBROUTINE FOR OPTIMISATION IN S DIRECTION 
C        STAT1,SELECT,STATUS : MONODIMENSIONAL OPTIMISATION 
C        DOT,SUPDOT,DIAGIV,CARDAN : MATHEMATICAL PACKAGE 
C        COMPFG : ENERGY AND GRADIENT 
C        SAVOPT : SAVE/RESTART ROUTINE (THIS SUBROUTINE HAS BEEN REFORMULATED 
C                 AND REMOVED)                                          GL0492
C        SVOPTS : SAVE/RESTART ROUTINE (REFORMULATED VERSION OF SAVOPT)
C---------------------- 
C        THE COMMON/OPTIM/        INCLUDES THE WHOLE DATA REQUIRED, 
C                  /PRECI/ PROVIDES THE ESTIMATED ERRORS ON 
C        THE ENERGY (SCFCV) , THE GRADIENT COMPONENTS (EG(3)) , 
C        AN ESTIMATE OF THE DIAGONAL CURVATURES (ESTIM(3)) , 
C        THE MAXIMUM STEP LENGTH ARE DEFINED HERE. 
C        THE SUBROUTINE COMPFG MUST RETURN ALSO THE FLAG "FAIL" : 
C                 FAIL = .T.  : SCF CONVERGED 
C                      = .F.    OTHERWISE. 
C        NOTE...THIS STRUCTURE ALLOWS TO OVERLAY THIS BRANCH (LDATA, 
C        POINT1&3,FINDS,STAT1,SELECT,STATUS,DOT,SUPDOT,DIAGIV,CARDAN) 
C        WITH THE ENERGY AND GRADIENT SECTION (COMPFG). 
C        MOREOVER,ONLY THIS SUBROUTINE MUST BE MODIFIED FOR 
C        IMPLEMENTATION IN ANOTHER PACKAGE. 
C---------------------- 
C        LABORATOIRE DE CHIMIE STRUCTURALE 
C                    FACULTE DES SCIENCES 
C                    AVENUE DE L'UNIVERSITE 
C                                              PAU (64000)-FRANCE- 
C        REFERENCE : D.LIOTARD THESIS PAU (1979) 
C 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / I1,I2,IROUTE,ITYP,ICURV,IOPT1,NOPT,              DJG0995
     1                 ISAVE,ISAV2, IDUMMY(19 + 2*NCHAIN - 9)           3GL3092 
      COMMON /OPTMCL / LFINAL, LBIS(2), LDUMMY(NCHAIN + 4 - 3)          GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),P(MAXPAR*MAXPAR),      3GL3092
     1                 STEP(MAXPAR),SEUIL(2),ESTIM2(3),PMIN(3),         3GL3092
     2                 PMAX2(3),EG2(3),EPS1,ERROR,SAVE1,SAVE2,FCUR1,    3GL3092
     3                 FCUR2,SAV1(MAXPAR,2),SAV2(3),                    3GL3092
     4                RDUMY(MAXPAR*(2*MAXPAR+NCHAIN+12) + NCHAIN + 6)   3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /DENSTY/ PT(MPACK),PA(MPACK),PB(MPACK) 
      COMMON /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR) 
      COMMON /MESAGE/ IFLEP, ISCF                                       DJG0995
      COMMON /SCFOK / FAIL 
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /OPTCOM/ LDER                                              GDH1093
      DIMENSION PREF(MPACK),PREFA(MPACK),PREFB(MPACK) 
      DIMENSION X(1),G(1),S(1) 
      EQUIVALENCE (S(1),HNEW(1)) 
      LOGICAL FAIL,LFINAL,REST,UHF, LDER 
      LOGICAL LBIS, LDUMMY                                              3GL3092
       SAVE
C     ... 
C     BEGIN INITIALIZATION 
C     ... 
      LINEAR=NORBS*(NORBS+1)/2 
      UHF=NALPHA.NE.0 
      FAIL=.FALSE. 
C     TLEFT=3600. 
      TLEFT=TDEF                                                        GL0492
      ITEST = MAXPAR                                                    03GCL93
      IF(ITLIMI.NE.0) TLEFT=FITLIM                                      DJG0995
      IND=IND+4 
      IF (LDER) IND=IND+2
      IOPT1=1                                                           DJG0995 
      EPS=1.D0 
      LIMIT=5 
      IF(IPRECI.NE.0) THEN                                              DJG0995
         IOPT1=2                                                        DJG0995 
         EPS=EPS*0.1D0 
         LIMIT=LIMIT*3 
      ENDIF 
      IF(IPRINT.NE.0)IMP=IIPRIN                                         DJG0995
      IF(ICYCLE.NE.0) LIMIT=IICYCL                                      DJG0995
      IF(IGNORM.NE.0) EPS=ABS(FIGNOR)                                   DJG0995
      REST=IRESTA.NE.0                                                  DJG0995
C     FOR SAVE/RESTART : EQUIVALENCED /OPTIM/ I1(MAXPAR,LEN1),I2(LEN2) 
C     LEN1=2*(MAXPAR+4) 
C     LEN2=2*(MAXHES+23) + 16 
C
C  LEN1, LEN2, AND LEN3 HAVE BEEN REDEFINED TO REPRESENT THE LENGTH OF THE 
C  COMMON BLOCKS OPTMCR, OPTMCI, AND OPTMCL IN SVOPTS (NEW VERSION OF SAVOPT) 
C
      LEN1=MAXHES+MAXPAR*(MAXPAR+4)+23                                  GL0492
      LEN2=9                                                            GL0492
      LEN3=3                                                            GL0492
C     ... 
C     COMPLETE INITIALIZATION, OR RESTART 
C     ... 
      IF ( REST ) THEN 
         CALL SVOPTS(LEN1,LEN2,LEN3,.TRUE.)                             GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         CALL SCOPY (N,SAV1(1,1),1,X,1) 
         CALL SCOPY (N,SAV1(1,2),1,G,1) 
         F=     SAV2(1) 
         SCFCV= SAV2(2) 
         SCFTOL=SAV2(3) 
         ITERAT=ISAV2 
         READ (JDEN) (PREFA(I),I=1,LINEAR) 
         IF (UHF) READ (JDEN) (PREFB(I),I=1,LINEAR) 
         REWIND JDEN
         IF (UHF) THEN 
            DO 2 I=1,LINEAR 
    2       PREF(I)=PREFA(I)+PREFB(I) 
            ELSE 
            DO 3 I=1,LINEAR 
    3       PREF(I)=PREFA(I)*2.D0 
         ENDIF 
      ELSE IF (ITEST .LT. 20) THEN                                       03GCL93
              WRITE(JOUT,130) 
          ISTOP=1                                                       GDH1095
          IWHERE=135                                                    GDH1095
          RETURN                                                        GDH1095
           ELSE 
              CALL COMPFG (X,F,FAIL,GNEW,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
              PMAX2(1)=0.015D0 
              PMIN (1)=0.001D0 
              PMAX2(2)=0.045D0 
              PMIN (2)=0.01D0 
              PMAX2(3)=0.061D0 
              PMIN (3)=0.01D0 
              DO 4 I=1,3 
              EG2(I)=EG(I) 
    4         ESTIM2(I)=ESTIM(I)*0.5D0 
              CALL LDATA (NOPT,IOPT1,ITERAT,N,LIMIT,EPS,IND)            DJG0995
              IF (IFLEP.NE.12) IFLEP=13                                 GDH0597
              AK=0.D0 
              ITERAT=0 
      ENDIF 
C     ... 
C     BEGINNING OF ITERATIVE LOOP 
C     ... 
      IF ( REST ) GO TO (21,32,34,41),ISAVE 
   10 ITERAT=ITERAT+1 
      IF(FAIL) GO TO 50 
      CALL SCOPY (LINEAR,PT,1,PREF,1) 
      CALL SCOPY (LINEAR,PA,1,PREFA,1) 
      IF (UHF) CALL SCOPY (LINEAR,PB,1,PREFB,1) 
C     ... 
C     NEW HESSIAN EVALUATION 
C     ---------------------- 
      IF(IND.GT.6) GO TO 30 
C 
C     ANALYTICAL- NUMERICAL METHODS 
      CALL POINT1(P,X,F,N,EPS,ITERAT,LIMIT,G) 
   20 CALL PORCPU (TFLY)                                                GL0492
      IF(0.9D0*TLEFT.LT.(TFLY-TIME0)) THEN 
         ISAVE=1 
         GO TO 70 
      ENDIF 
      CALL SCOPY (LINEAR,PREF,1,PT,1) 
      CALL SCOPY (LINEAR,PREFA,1,PA,1) 
      IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
   21 CALL COMPFG (X,FCUR1,FAIL,G,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(FAIL) GO TO 61 
      IF (IOPT1.EQ.2) THEN                                              DJG0995
         X(I1)=SAVE1-STEP(I1) 
         LCI=N*(I1-1)+1 
         CALL SCOPY (LINEAR,PREF,1,PT,1) 
         CALL SCOPY (LINEAR,PREFA,1,PA,1) 
         IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
         CALL COMPFG(X,FCUR2,FAIL,P(LCI),.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IF(FAIL) GO TO 62 
      ENDIF 
      CALL POINT2(P,X,F,N,EPS,ITERAT,LIMIT,G) 
      GO TO (20,40),IROUTE 
      GO TO 60 
C 
C     TWICE NUMERICAL METHODS 
   30 CALL POINT3(P,X,F,N,EPS,ITERAT,LIMIT,G) 
   31 X(I1)=SAVE1+STEP(I1) 
      CALL PORCPU (TFLY)                                                GL0492
      IF (0.99D0*TLEFT.LT.TFLY-TIME0) THEN 
         ISAVE=2 
         GO TO 70 
      ENDIF 
      CALL SCOPY (LINEAR,PREF,1,PT,1) 
      CALL SCOPY (LINEAR,PREFA,1,PA,1) 
      IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
   32 CALL COMPFG (X,FCUR1,FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(FAIL) GO TO 64 
      X(I1)=SAVE1-STEP(I1) 
      CALL SCOPY (LINEAR,PREF,1,PT,1) 
      CALL SCOPY (LINEAR,PREFA,1,PA,1) 
      IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
      CALL COMPFG (X,FCUR2,FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(FAIL) GO TO 65 
      CALL POINT4(P,X,F,N,EPS,ITERAT,LIMIT,G) 
      GO TO (31,33,40) IROUTE 
      GO TO 60 
   33 X(I1)=SAVE1+STEP(I1) 
      X(I2)=SAVE2+STEP(I2) 
      CALL PORCPU (TFLY)                                                GL0492
      IF (TLEFT.LT.TFLY-TIME0) THEN 
         ISAVE=3 
         GO TO 70 
      ENDIF 
      CALL SCOPY (LINEAR,PREF,1,PT,1) 
      CALL SCOPY (LINEAR,PREFA,1,PA,1) 
      IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
   34 CALL COMPFG(X,FCUR1,FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) GO TO 64 
      IF(IOPT1.EQ.2) THEN                                               DJG0995
         X(I1)=SAVE1-STEP(I1) 
         X(I2)=SAVE2-STEP(I2) 
         CALL SCOPY  (LINEAR,PREF,1,PT,1) 
         CALL SCOPY  (LINEAR,PREFA,1,PA,1) 
         IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
         CALL COMPFG (X,FCUR2,FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IF (FAIL) GO TO 65 
      ENDIF 
      CALL POINT5(P,X,F,N,EPS,ITERAT,LIMIT,G) 
      GO TO (31,33,40),IROUTE 
      GO TO 60 
C     ... 
C     CONVERGENCE : ITERAT.GE.LIMIT  OR  RMS GRADIENT OK 
C     -------------------------------------------------- 
   40 RMS=SQRT(DOT(GNEW,GNEW,N)/DBLE(N))                                GCL0393
      IF(ITERAT.GE.LIMIT.OR.RMS.LT.EPS) GO TO 50 
C     ... 
C     SELECT THE 1-D DIRECTION OF OPTIMIZATION 
C     ---------------------------------------- 
      CALL PORCPU (TFLY)                                                GL0492
      IF (0.8D0*TLEFT.LT.TFLY-TIME0) THEN 
         ISAVE=4 
         GO TO 70 
      ENDIF 
   41 CALL FINDS (P,X,F,N,ICURV,G,NOPT,AK) 
C     ... 
C     1-D OPTIMIZATION (ON ENERGY OR GRADIENT NORM) 
C     --------------------------------------------- 
C Former STAT routine : renamed to avoid clash with the libc C library  IR1294
      CALL AMSTAT(AK,N,X,F,EPS)                                         IR1294
C      CALL STAT(AK,N,X,F,EPS)
      GO TO 10 
C     ... 
C     END OF ITERATIVE LOOP 
C     ... 
   50 IMP=IMP0 
C     RESTORE THE BEST GEOMETRY AND RELATED DATA EVERYWHERE 
      CALL COMPFG (X,F,FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      CALL SCOPY (N,GNEW,1,G,1) 
      IF(RMS.GT.EPS) THEN 
C        OPTIMIZATION NOT COMPLETED. 
         IND=1 
         IFLEP=12                                                       DJG0995
      ELSE 
C        OPTIMIZATION COMPLETED. 
         IF(MOD(IND,2).EQ.0)THEN 
            IFLEP=11                                                    DJG0995
         ELSE 
            IFLEP=10                                                    DJG0995
         ENDIF 
         IND=0 
C        BACK TRANSFORM THE HESSIAN ONTO CARTESIAN COORDINATES, 
C        AND COMPUTE CONTRIBUTIONS TO ZERO POINT ENERGY, IF POSSIBLE. 
         CALL ZPE 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF 
      RETURN 
C     ... 
C     ERRORS SECTION 
C     ... 
   60 WRITE (JOUT,120) IROUTE 
      FAIL=.TRUE. 
      GO TO 50 
   61 INTNL=1 
      GO TO 63 
   62 INTNL=2 
   63 WRITE(JOUT,100) INTNL,I1,STEP(I1) 
      RETURN 
   64 INTNL=1 
      GO TO 66 
   65 INTNL=2 
   66 WRITE(JOUT,110) INTNL,I1,I2,STEP(I1),STEP(I2) 
      RETURN 
C     ... 
C     SAVE SECTION 
C     ... 
   70 CALL SCOPY (N,X,1,SAV1(1,1),1) 
      CALL SCOPY (N,G,1,SAV1(1,2),1) 
      SAV2(1)=F 
      SAV2(2)=SCFCV 
      SAV2(3)=SCFTOL 
      ISAV2=ITERAT 
      CALL SCOPY (LINEAR,PREFA,1,PA,1) 
      IF (UHF) CALL SCOPY (LINEAR,PREFB,1,PB,1) 
      CALL SVOPTS (LEN1,LEN2,LEN3,.FALSE.)                              GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
  100 FORMAT(' AT POINT',I2,' OF HESSIAN ROW',I3,' WITH STEP',1PD8.1) 
  110 FORMAT(' AT POINT',I2,5X,2I4,' OF HESSIAN WITH STEPS',1P,2D8.1) 
  120 FORMAT(' ABNORMAL VALUE OF IROUTE =',I5,' IN LTRD . STOP') 
  130 FORMAT(' THE ORGANIZATION OF THE COMMON /OPTIM/ IN ''LTRD'' NEEDS 
     .THE VALUE OF'/' THE PARAMETER ''MAXPAR'' TO EXCEED 19. SORRY]') 
      RETURN                                                            GDH0495
      END 
