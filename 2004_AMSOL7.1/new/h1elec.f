      SUBROUTINE H1ELEC(NI,NJ,XI,XJ,SMAT) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'KEYS.i'                                                  DJG0995
      DIMENSION XI(3),XJ(3),SMAT(9,9), BI(9), BJ(9) 
C*********************************************************************** 
C 
C  H1ELEC FORMS THE ONE-ELECTRON MATRIX BETWEEN TWO ATOMS. 
C 
C   ON INPUT    NI   = ATOMIC NO. OF FIRST ATOM. 
C               NJ   = ATOMIC NO. OF SECOND ATOM. 
C               XI   = COORDINATES OF FIRST ATOM. 
C               XJ   = COORDINATES OF SECOND ATOM. 
C 
C   ON OUTPUT   SMAT = MATRIX OF ONE-ELECTRON INTERACTIONS. 
C 
C*********************************************************************** 
      COMMON /BETAS / BETAS(107),BETAP(107),BETAD(107) 
      COMMON /BETA3 / BETA3(153) 
      COMMON /EULER / TVEC(3,3), ID 
      COMMON /VSIPS / VS(107),VP(107),VD(107) 
      COMMON /NATORB/ NATORB(107) 
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U,KDUM(6) 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION SBITS(9,9), LIMS(3,2), XJUC(3) 
      LOGICAL FIRST
      EQUIVALENCE (L1L,LIMS(1,1)) 
       SAVE
      IF (ICONTR(37).EQ.1) THEN                                         GDH0195
         ICONTR(37)=2                                                   GDH0195
         FIRST=.TRUE.                                                   GDH0195
         ITYPE=1                                                        GDH0195
      ENDIF                                                             GDH0195
      IF(ID.EQ.0) THEN 
C         PRINT *, 'I AM BEFORE DIAT', XI, XJ
         CALL DIAT(NI,NJ,XI,XJ,SMAT) 
      ELSE 
         IF (FIRST) THEN  
            FIRST=.FALSE.
            DO 10 I=1,ID 
               LIMS(I,1)=-1 
   10       LIMS(I,2)= 1 
            DO 20 I=ID+1,3 
               LIMS(I,1)=0 
   20       LIMS(I,2)=0 
         ENDIF 
         DO 30 I=1,9 
            DO 30 J=1,9 
   30    SMAT(I,J)=0 
         DO 60 I=L1L,L1U 
            DO 60 J=L2L,L2U 
               DO 60 K=L3L,L3U 
                  DO 40 L=1,3 
   40             XJUC(L)=XJ(L)+TVEC(L,1)*I+TVEC(L,2)*J+TVEC(L,3)*K 
                  CALL DIAT(NI,NJ,XI,XJUC,SBITS) 
                  DO 50 L=1,9 
                     DO 50 M=1,9 
   50             SMAT(L,M)=SMAT(L,M)+SBITS(L,M) 
   60    CONTINUE 
      ENDIF 
   70 GOTO (80,90,100) ITYPE 
   80 IF(IMINDO.NE.0) THEN                                              DJG0995
         ITYPE=2 
      ELSE 
         ITYPE=3 
      ENDIF 
      GOTO 70 
   90 CONTINUE 
      II=MAX(NI,NJ) 
      NBOND=(II*(II-1))/2+NI+NJ-II 
      IF(NBOND.GT.153)GOTO 110 
      BI(1)=BETA3(NBOND)*VS(NI) 
      BI(2)=BETA3(NBOND)*VP(NI) 
      BI(3)=BI(2) 
      BI(4)=BI(2) 
      BJ(1)=BETA3(NBOND)*VS(NJ) 
      BJ(2)=BETA3(NBOND)*VP(NJ) 
      BJ(3)=BJ(2) 
      BJ(4)=BJ(2) 
      GOTO 110 
  100 CONTINUE 
      BI(1)=BETAS(NI)*0.5D0 
      BI(2)=BETAP(NI)*0.5D0 
      BI(3)=BI(2) 
      BI(4)=BI(2) 
      BI(5)=BETAD(NI)*0.5D0 
      BI(6)=BI(5) 
      BI(7)=BI(5) 
      BI(8)=BI(5) 
      BI(9)=BI(5) 
      BJ(1)=BETAS(NJ)*0.5D0 
      BJ(2)=BETAP(NJ)*0.5D0 
      BJ(3)=BJ(2) 
      BJ(4)=BJ(2) 
      BJ(5)=BETAD(NJ)*0.5D0 
      BJ(6)=BJ(5) 
      BJ(7)=BJ(5) 
      BJ(8)=BJ(5) 
      BJ(9)=BJ(5) 
  110 CONTINUE 
      NORBI=NATORB(NI) 
      NORBJ=NATORB(NJ) 
      IF(NORBI.EQ.9.OR.NORBJ.EQ.9) THEN 
          DO 120 J=1,NORBJ 
          DO 120 I=1,NORBI 
     1           -2.0D0*SMAT(I,J)*((BI(I)*BJ(J))**0.50D0)
  120     SMAT(I,J)=-2.0D0*SMAT(I,J)*((BI(I)*BJ(J))**0.50D0) 
      ELSE 
          DO 130 J=1,NORBJ 
          DO 130 I=1,NORBI 
  130     SMAT(I,J)=SMAT(I,J)*(BI(I)+BJ(J)) 
      ENDIF 
C 
C    In the calculation of the one-electron terms the geometric mean 
C    of the two beta values is being used if one of the atoms 
C    contains d-orbitals. 
      RETURN 
      END 
