      SUBROUTINE H1ELEC(NI,NJ,XI,XJ,SMAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XI(3),XJ(3),SMAT(9,9), BI(9), BJ(9)
C****************************************************************************
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
C****************************************************************************
      COMMON /BETAS / BETAS(54),BETAP(54),BETAD(54)
      COMMON /BETA3 / BETA3(153)
      COMMON /KEYWRD/ KEYWRD
      COMMON /VSIPS / VS(54),VP(54),VD(54)
      COMMON /NATORB/ NATORB(54)
      CHARACTER*80 KEYWRD
      DATA ITYPE /1/
      CALL DIAT(NI,NJ,XI,XJ,SMAT)      
  10  GOTO (100,200,300) ITYPE
  100 IF(INDEX(KEYWRD,'MINDO3') .NE. 0) THEN
      ITYPE=2
      ELSE
      ITYPE=3
      ENDIF
      GOTO 10
  200 CONTINUE
      II=MAX(NI,NJ)
      NBOND=(II*(II-1))/2+NI+NJ-II
      BI(1)=BETA3(NBOND)*VS(NI)
      BI(2)=BETA3(NBOND)*VP(NI)
      BI(3)=BI(2)
      BI(4)=BI(2)
      BJ(1)=BETA3(NBOND)*VS(NJ)
      BJ(2)=BETA3(NBOND)*VP(NJ)
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      GOTO 400
  300 CONTINUE
      BI(1)=BETAS(NI)*0.5D0
      BI(2)=BETAP(NI)*0.5D0
      BI(3)=BI(2)
      BI(4)=BI(2)
      BJ(1)=BETAS(NJ)*0.5D0
      BJ(2)=BETAP(NJ)*0.5D0
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
  400 CONTINUE
      NORBI=NATORB(NI)
      NORBJ=NATORB(NJ)
          DO 1 J=1,NORBJ
          DO 1 I=1,NORBI
   1      SMAT(I,J)=SMAT(I,J)*(BI(I)+BJ(J))
      RETURN
      END

      
