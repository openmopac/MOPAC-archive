      SUBROUTINE MULLIK(C,CBETA,UHF,H,F,NORBS,VECS,STORE,*)             CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
      LOGICAL UHF
      DIMENSION C(*), CBETA(*), H(*), VECS(*), STORE(*), F(*)
**********************************************************************
*
*  MULLIK DOES A MULLIKEN POPULATION ANALYSIS
* ON INPUT     C      =  SQUARE ARRAY OF EIGENVECTORS.
*              H      =  PACKED ARRAY OF ONE-ELECTRON MATRIX
*              F      =  WORKSTORE OF SIZE AT LEAST NORBS*NORBS
*              VECS   =  WORKSTORE OF SIZE AT LEAST NORBS*NORBS
*              STORE  =  WORKSTORE OF SIZE AT LEAST (NORBS*(NORBS+1))/2
*
**********************************************************************
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NDUMMY,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
     4       /KEYWRD/ KEYWRD
     5       /BETAS / BETAS(120),BETAP(120),BETAD(120)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /EXPONT/ ZS(120),ZP(120),ZD(120)
      COMMON /SRPL/ ISSRP                                               IR0594
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      CHARACTER*4 BETF                                                  IR0594
      CHARACTER*1 BETL                                                  IR0594
      CHARACTER*5 BETTYP                                                IR0594
      LOGICAL GRAPH,ISSRP                                               IR0594
CSTP  Return variable from subroutine GETBET, formerly a function:
      DOUBLE PRECISION  RGETBT                                          CSTP
      CHARACTER KEYWRD*160
**********************************************************************
*
*  FIRST, RE-CALCULATE THE OVERLAP MATRIX
*
**********************************************************************
      DIMENSION EIGS(MAXORB), IFACT(MAXORB), XYZ(3,NUMATM)
CSAV         SAVE                                                           GL0892
      GRAPH=(INDEX(KEYWRD,'GRAPH').NE.0)
      DO 10 I=1,NORBS
   10 IFACT(I)=(I*(I-1))/2
      IFACT(NORBS+1)=(NORBS*(NORBS+1))/2
C
C  START of special SRP BETA handling (Ivan Rossi - May '94)
C
      IF(ISSRP) THEN
         DO 15 I=1,NUMAT
            IF=NFIRST(I)
            IL=NLAST(I)
            IM1=I-1
            BETF= 'BETS'
            DO 15 K=IF,IL
               II=(K*(K-1))/2
               DO 13 J=1,IM1
                  JF=NFIRST(J)
                  JL=NLAST(J)
                  BETL='S'
                  DO 12 JJ=JF,JL
                     IJ=II+JJ
                     BETTYP=BETF//BETL
                     IF( BETTYP .eq. 'BETPS') THEN
                         CALL GETBET(NAT(J),NAT(I),'BETSP',RGETBT,*9999) CSTP(call)
                          H(IJ)=H(IJ)/RGETBT                            CSTP
CSTP                      H(IJ)=H(IJ)/GETBET(NAT(J), NAT(I),'BETSP')
                     ELSE
                          CALL GETBET(NAT(I),NAT(J),BETTYP,RGETBT,*9999)CSTP CSTP(call)
                          H(IJ)=H(IJ)/RGETBT                            CSTP
CSTP                      H(IJ)=H(IJ)/GETBET(NAT(I), NAT(J), BETTYP)
                     ENDIF
                     STORE(IJ)=H(IJ)
   12             BETL='P'
   13          CONTINUE
               DO 14 JJ=IF,K
                  IJ=II+JJ
                  STORE(IJ)=0.D0
   14          H(IJ)=0.D0
   15    BETF='BETP'
      ELSE
C
C ...build Density matrix in the standard way (END of SRP BETA handling
C
         DO 50 I=1,NUMAT
            IF=NFIRST(I)
            IL=NLAST(I)
            IM1=I-1
            BI=BETAS(NAT(I))
            DO 50 K=IF,IL
               II=(K*(K-1))/2
               DO 30 J=1,IM1
                  JF=NFIRST(J)
                  JL=NLAST(J)
                  BJ=BETAS(NAT(J))
                  DO 20 JJ=JF,JL
                     IJ=II+JJ
                     H(IJ)=2.D0*H(IJ)/(BI+BJ)     +1.D-14
C  THE  +1.D-14 IS TO PREVENT POSSIBLE ERRORS IN THE DIAGONALIZATION.
                     STORE(IJ)=H(IJ)
   20             BJ=BETAP(NAT(J))
   30          CONTINUE
               DO 40 JJ=IF,K
                  IJ=II+JJ
                  STORE(IJ)=0.D0
   40          H(IJ)=0.D0
   50    BI=BETAP(NAT(I))
      ENDIF                                                             IR0594
      DO 60 I=1,NORBS
         STORE(IFACT(I+1))=1.D0
   60 H(IFACT(I+1))=1.D0
      CALL RSP(H,NORBS,NORBS,EIGS,VECS,*9999)                            CSTP(call)
      DO 70 I=1,NORBS
   70 EIGS(I)=1.D0/SQRT(ABS(EIGS(I)))
      IJ=0
      DO 90 I=1,NORBS
         DO 90 J=1,I
            IJ=IJ+1
            SUM=0.D0
            DO 80 K=1,NORBS
   80       SUM=SUM+VECS(I+(K-1)*NORBS)*EIGS(K)
     1                *VECS(J+(K-1)*NORBS)
            F(I+(J-1)*NORBS)=SUM
   90 F(J+(I-1)*NORBS)=SUM
      IF (GRAPH) THEN
         CALL GMETRY(GEO,XYZ,*9999)                                      CSTP(call)
*
* WRITE TO DISK THE FOLLOWING DATA FOR GRAPHICS CALCULATION, IN ORDER:
*
*      NUMBER OF ATOMS, ORBITAL, ELECTRONS
*      ALL ATOMIC COORDINATES
*      ORBITAL COUNTERS
*      ORBITAL EXPONENTS, S, P, AND D, AND ATOMIC NUMBERS
*      EIGENVECTORS (M.O.S NOT RE-NORMALIZED)
*      INVERSE-SQUARE ROOT OF THE OVERLAP MATRIX.
*
         OPEN(UNIT=13,FILE='FOR013',FORM='UNFORMATTED',                 No CIO
     &                    STATUS='NEW')                                 No CIO
         WRITE(13)NUMAT,NORBS,NELECS,((XYZ(I,J),J=1,NUMAT),I=1,3)       No CIO
         WRITE(13)(NLAST(I),NFIRST(I),I=1,NUMAT)                        No CIO
         WRITE(13)(ZS(NAT(I)),I=1,NUMAT),(ZP(NAT(I)),I=1,NUMAT),        No CIO
     1         (ZD(NAT(I)),I=1,NUMAT),(NAT(I),I=1,NUMAT)                No CIO
         LINEAR=NORBS*NORBS                                             No CIO
         WRITE(13)(C(I),I=1,LINEAR)                                     No CIO
         WRITE(13)(F(I),I=1,LINEAR)                                     No CIO
         RETURN
      ENDIF
*
* OTHERWISE PERFORM MULLIKEN ANALYSIS
*
C
C      SUBSTITUTING  CRAY MXM WITH BLAS LEVEL 3 DGEMM                     IR0394
C      call mxm(c,norbs,f,norbs,vecs,norbs)
C
      call dgemm('n','n', norbs, norbs, norbs, 1.0d0,
     1            c, norbs, f, norbs, 0.0d0, vecs, norbs,*9999)          CSTP(call)

      I=-1
      CALL DENSIT(VECS,NORBS,NORBS,NCLOSE,NOPEN,FRACT,C,2,*9999)         CSTP(call)
      LINEAR=(NORBS*(NORBS+1))/2
      DO 100 I=1,LINEAR
  100 C(I)=C(I)*STORE(I)
      SUMM=0.D0
      DO 130 I=1,NORBS
         SUM=0
         DO 110 J=1,I
  110    SUM=SUM+C(IFACT(I)+J)
         DO 120 J=I+1,NORBS
  120    SUM=SUM+C(IFACT(J)+I)
         SUMM=SUMM+SUM
  130 C(IFACT(I+1))=SUM
      CALL VECPRT(C,NORBS,*9999)                                        CSTP(call)
      RETURN
 9999 RETURN 1                                                          CSTP
      END
