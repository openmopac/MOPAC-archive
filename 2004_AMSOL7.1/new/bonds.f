      SUBROUTINE BONDS(P,LFLAG)                                         CC8-91
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C     PARAMETER (NATMS2=MAXPAR*MAXPAR-MAXORB*MAXORB) 
      PARAMETER (NATMS2=MAXPAR*MAXPAR)                                  GCL0393
      DIMENSION P(*)
      COMMON /HBORDS/ HBORD(NUMATM),HB(NUMATM)                          CC8-91
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA, 
     2                NCLOSE,NOPEN,NDUMY
C     COMMON /SCRACH/ B(MAXORB,MAXORB), BONDAB(NATMS2) 
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ B(MAXORB,MAXORB), BONDAB(NATMS2),                 3GL3092
     1                DUM3(6*MAXPAR**2+1-MAXORB*MAXORB-NATMS2)          GCL0393
C
C*********************************************************************** 
C 
C   CALCULATES, AND PRINTOUT IF NEEDED 
C     ----------------------- 
C 
C   CALCULATES, AND PRINTS, THE BOND INDICES AND VALENCIES OF ATOMS
C
C  FOR REFERENCE, SEE "BOND INDICES AND VALENCY", J. C. S. DALTON,
C  ARMSTRONG, D.R., PERKINS, P.G., AND STEWART, J.J.P., 838 (1973)
C
C   ON INPUT
C            P = DENSITY MATRIX, LOWER HALF TRIANGLE, PACKED.
C            P   IS NOT ALTERED BY BONDS.
C
C***********************************************************************
      LOGICAL LFLAG                                                     CC8-91
       SAVE
      K=0
      DO 20 I=1,NORBS
         DO 20 J=1,I
            K=K+1
            B(I,J)=P(K)
   20 B(J,I)=P(K)
      IJ = 0
      DO 60 I=1,NUMAT
         IF (.NOT.LFLAG) HBORD(I)=0.0                                   CC8-91
         L=NFIRST(I)
         LL=NLAST(I)
         DO 40 J=1,I
            IJ = IJ + 1
            K=NFIRST(J)
            KK=NLAST(J)
            X=0.0
            DO 30 IL=L,LL
               DO 30 IH=K,KK
   30       X=X+B(IL,IH)*B(IL,IH)
   40    BONDAB(IJ)=X
         X=-BONDAB(IJ)
         DO 50 J=L,LL
   50    X=X+2.D0*B(J,J)
         BONDAB(IJ)=X
   60 CONTINUE
      IF(LFLAG) THEN                                                    CC8-91
      WRITE(JOUT,10)
   10 FORMAT(/20X,'Bond orders and valencies')                          GL0592
C  10 FORMAT(//20X,'BOND ORDERS AND VALENCIES',//)
        CALL VECPRT( BONDAB, NUMAT)
      ELSE                                                              CC8-91
        IJ=0                                                            CC8-91
        DO 70 I=1,NUMAT                                                 CC8-91
          IF(NAT(I).EQ.1) GO TO 70                                      CC8-91
                DO 80 J=1,I-1                                           CC8-91
                        IF(NAT(J).NE.1) GO TO 80                        CC8-91
                        IJ=(I*(I-1))/2+J                                CC8-91
                        HBORD(I)=HBORD(I)+BONDAB(IJ)                    CC8-91
80              CONTINUE                                                CC8-91
                DO 90 K=I+1,NUMAT                                       CC8-91
                        IF(NAT(K).NE.1) GO TO 90                        CC8-91
                        IJ=(K*(K-1))/2+I                                CC8-91
                        HBORD(I)=HBORD(I)+BONDAB(IJ)                    CC8-91
90              CONTINUE                                                CC8-91
70      CONTINUE                                                        CC8-91
      ENDIF                                                             CC8-91
      RETURN
      END
