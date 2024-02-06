      SUBROUTINE VECWRT (A,NUMB,IW) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION  A(*) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
C********************************************************************** 
C 
C  VECWRT OUTPUTS A LOWER-HALF TRIANGLE OF A SQUARE MATRIX TO 
C         CHANNEL IW, THE LOWER-HALF TRIANGLE BEING STORED IN 
C         PACKED FORM IN THE ARRAY "A" 
C 
C ON INPUT: 
C      A      = ARRAY TO BE PRINTED 
C      NUMB   = SIZE OF ARRAY TO BE PRINTED 
C      IW     = CHANNEL NUMBER FOR OUTPUT 
C(REF) NUMAT  = NUMBER OF ATOMS IN THE MOLECULE (THIS IS NEEDED TO 
C               DECIDE IF AN ATOMIC ARRAY OR ATOMIC ORBITAL ARRAY IS 
C               TO BE PRINTED 
C(REF) NAT    = LIST OF ATOMIC NUMBERS 
C(REF) NFIRST = LIST OF ORBITAL COUNTERS 
C(REF) NLAST  = LIST OF ORBITAL COUNTERS 
C 
C  NONE OF THE ARGUMENTS ARE ALTERED BY THE CALL OF VECPRT 
C 
C********************************************************************* 
      DIMENSION NATOM(MAXORB) 
      CHARACTER * 6 LINE(21) 
      CHARACTER*2 ELEMNT(107),ATORBS(9), ITEXT(MAXORB),JTEXT(MAXORB) 
      SAVE
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/ 
      DATA ELEMNT/' H','He', 
     2 'Li','Be',' B',' C',' N',' O',' F','Ne', 
     3 'Na','Mg','Al','Si',' P',' S','Cl','Ar', 
     4 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu', 
     4 'Zn','Ga','Ge','As','Se','Br','Kr', 
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag', 
     5 'Cd','In','Sn','Sb','Te',' I','Xe', 
     6 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy', 
     6 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt', 
     6 'Au','Hg','Tl','Pb','Bi','Po','At','Rn', 
     7 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX', 
     8 'Fm','Md','No','++',' +','--',' -','  '/ 
      IF(NUMAT.EQ.NUMB) THEN 
C 
C    PRINT OVER ATOM COUNT 
C 
          DO 10 I=1,NUMAT 
              ITEXT(I)='  ' 
              JTEXT(I)=ELEMNT(NAT(I)) 
              NATOM(I)=I 
  10          CONTINUE 
          ELSE 
          IF (NLAST(NUMAT) .EQ. NUMB) THEN 
              DO 30 I=1,NUMAT 
                  JLO=NFIRST(I) 
                  JHI=NLAST(I) 
                  L=NAT(I) 
                  K=0 
                  DO 20 J=JLO,JHI 
                      K=K+1 
                      ITEXT(J)=ATORBS(K) 
                      JTEXT(J)=ELEMNT(L) 
                      NATOM(J)=I 
  20              CONTINUE 
  30          CONTINUE 
              ELSE 
              NUMB=ABS(NUMB) 
              DO 40 I=1,NUMB 
                  ITEXT(I) = '  ' 
                  JTEXT(I) = '  ' 
  40              NATOM(I)=I 
              ENDIF 
          END IF 
      NUMB=ABS(NUMB) 
      DO 50 I=1,21 
  50       LINE(I)='------' 
      LIMIT=(NUMB*(NUMB+1))/2 
      KK=8 
      NA=1 
  60  LL=0 
      M=MIN((NUMB+1-NA),6)                                              GCL0393
      MA=2*M+1 
      M=NA+M-1 
      WRITE(IW,100)(ITEXT(I),JTEXT(I),NATOM(I),I=NA,M) 
      WRITE(IW,110) (LINE(K),K=1,MA) 
      DO 80 I=NA,NUMB 
         LL=LL+1 
         K=(I*(I-1))/2 
         L=MIN((K+M),(K+I))                                             GCL0393
         K=K+NA 
         IF ((KK+LL).LE.50) GO TO 70 
C#         WRITE (IW,120) 
         WRITE (IW,100) (ITEXT(N),JTEXT(N),NATOM(N),N=NA,M) 
         WRITE (IW,110) (LINE(N),N=1,MA) 
         KK=4 
         LL=0 
  70     WRITE (IW,130) ITEXT(I),JTEXT(I),NATOM(I),(A(N),N=K,L) 
  80  CONTINUE 
      IF (L.GE.LIMIT) GO TO 90 
      KK=KK+LL+4 
      NA=M+1 
      IF ((KK+NUMB+1-NA).LE.50) GO TO 60 
      KK=4 
C#      WRITE (IW,120) 
      GO TO 60 
  90  RETURN 
C 
 100  FORMAT (1H0/9X,10(2X,A2,1X,A2,I3,1X)) 
 110  FORMAT (1H ,21A6) 
 120  FORMAT (1H1) 
 130  FORMAT (1H ,A2,1X,A2,I3,10F11.6) 
C 
      END 
