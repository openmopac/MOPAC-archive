      SUBROUTINE PULAY(F,P,N,FPPF,FOCK,EMAT,LFOCK,NFOCK,MSIZE,START,PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*), P(*), FPPF(*), FOCK(*)
      LOGICAL START
**************************************************************************
*
*   PULAY USES DR. PETER PULAY'S METHOD FOR CONVERGENCE. 
*         A MATHEMATICAL DESCRIPTION CAN BE FOUND IN 
*         "P. PULAY, CHEM. PHYS. LETT., 73  393, (1980)."
*
* ARGUMENTS:-
*          ON INPUT F      = FOCK MATRIX, PACKED, LOWER HALF TRIANGLE.
*                   P      = DENSITY MATRIX, PACKED, LOWER HALF TRIANGLE.
*                   N      = NUMBER OF ORBITALS.
*                   FPPF   = WORKSTORE OF SIZE MSIZE, CONTENTS WILL BE
*                            OVERWRITTEN.
*                   FOCK   =      "       "              "         "
*                   EMAT   = WORKSTORE OF AT LEAST 15**2 ELEMENTS.
*                   START  = LOGICAL, = TRUE TO START PULAY.
*                   PL     = UNDEFINED ELEMENT.
*       ON OUTPUT   F      = "BEST" FOCK MATRIX, = LINEAR COMBINATION
*                            OF KNOWN FOCK MATRICES.
*                   START  = FALSE
*                   PL     = MEASURE OF NON-SELF-CONSISTENCY
*                          = [F*P] = F*P - P*F.
*
**************************************************************************
      COMMON /KEYWRD/ KEYWRD
      DIMENSION EMAT(20,20), EVEC(1000), COEFFS(20)
      CHARACTER*80 KEYWRD
      LOGICAL FIRST, DEBUG
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
          FIRST=.FALSE.
          MAXLIM=6
          DEBUG=(INDEX(KEYWRD,'DEBUGPULAY') .NE.0)
      ENDIF
      IF(START) THEN
          LINEAR=(N*(N+1))/2
          MFOCK=MSIZE/LINEAR
          IF(MFOCK.GT.MAXLIM)MFOCK=MAXLIM
          IF(DEBUG)
     +    WRITE(6,'('' MAXIMUM SIZE:'',I5)')MFOCK
          NFOCK=1
          LFOCK=1
          START=.FALSE.
      ELSE
          IF(NFOCK.LT.MFOCK)      NFOCK=NFOCK+1
          IF(LFOCK.NE.MFOCK)THEN
              LFOCK=LFOCK+1
              ELSE
              LFOCK=1
          ENDIF
      ENDIF
      LBASE=(LFOCK-1)*LINEAR
*
*   FIRST, STORE FOCK MATRIX FOR FUTURE REFERENCE.
*
      DO 9 I=1,LINEAR
   9     FOCK((I-1)*MFOCK+LFOCK)=F(I)
*
*   NOW FORM /FOCK*DENSITY-DENSITY*FOCK/, AND STORE THIS IN FPPF
*
      CALL MAMULT(P,F,FPPF(LBASE+1),N,0.D0)
      CALL MAMULT(F,P,FPPF(LBASE+1),N,-1.D0)
*
*   FPPF NOW CONTAINS THE RESULT OF FP - PF.
*
      NFOCK1=NFOCK+1
      DO 10 I=1,NFOCK
          EMAT(NFOCK1,I)=-1.D0
          EMAT(I,NFOCK1)=-1.D0
          EMAT(LFOCK,I)=DOT(FPPF((I-1)*LINEAR+1),FPPF(LBASE+1),LINEAR)
  10      EMAT(I,LFOCK)=EMAT(LFOCK,I)
      PL=EMAT(LFOCK,LFOCK)/LINEAR
      EMAT(NFOCK1,NFOCK1)=0.D0
      CONST=1.D0/EMAT(LFOCK,LFOCK)
C#     + *1.D4
      DO 14 I=1,NFOCK
      DO 14 J=1,NFOCK
  14  EMAT(I,J)=EMAT(I,J)*CONST
      IF(DEBUG) THEN
          WRITE(6,'('' EMAT'')')
          DO 66 I=1,NFOCK1
  66      WRITE(6,'(6E13.6)')(EMAT(J,I),J=1,NFOCK1)
      ENDIF
      L=0
      DO 20 I=1,NFOCK1
          DO 20 J=1,NFOCK1
              L=L+1
  20          EVEC(L)=EMAT(I,J)
      CONST=1.D0/CONST
      DO 15 I=1,NFOCK
      DO 15 J=1,NFOCK
  15  EMAT(I,J)=EMAT(I,J)*CONST
*********************************************************************
*   THE MATRIX EMAT SHOULD HAVE FORM
*
*      |<E(1)*E(1)>  <E(1)*E(2)> ...   -1.0|
*      |<E(2)*E(1)>  <E(2)*E(2)> ...   -1.0|
*      |<E(3)*E(1)>  <E(3)*E(2)> ...   -1.0|
*      |<E(4)*E(1)>  <E(4)*E(2)> ...   -1.0|
*      |     .            .      ...     . |
*      |   -1.0         -1.0     ...    0. |
*
*   WHERE <E(I)*E(J)> IS THE SCALAR PRODUCT OF [F*P] FOR ITERATION I
*   TIMES [F*P] FOR ITERATION J. 
*
*********************************************************************
      CALL OSINV(EVEC,NFOCK1,D)
C#      WRITE(6,'(''   D:'',E16.5)')D
      IF(ABS(D).LT.1.D-6)THEN
          START=.TRUE.
          RETURN
      ENDIF
      IF(NFOCK.LT.2) RETURN
      IL=NFOCK*NFOCK1
      DO 29 I=1,NFOCK
  29  COEFFS(I)=-EVEC(I+IL)
      IF(DEBUG) THEN
          WRITE(6,'('' EVEC'')')
          WRITE(6,'(6F12.6)')(COEFFS(I),I=1,NFOCK)
          WRITE(6,'(''    LAGRANGIAN MULTIPLIER (ERROR) =''
     +             ,F13.6)')EVEC(NFOCK1*NFOCK1)
      ENDIF
      DO 30 I=1,LINEAR
          SUM=0
          L=0
          II=(I-1)*MFOCK
          DO 40 J=1,NFOCK
  40          SUM=SUM+COEFFS(J)*FOCK(J+II)
  30      F(I)=SUM
C#      WRITE(6,'('' AFTER RE-INVERSION '')')
C#      CALL OSINV(EVEC,NFOCK1,D)
C#      DO 87 I=1,NFOCK1
C#      IL=(I-1)*NFOCK1+1
C#      IU=IL+NFOCK1-1
C#  87  WRITE(6,'(6E10.3)')(EVEC(J),J=IL,IU)
      RETURN
      END
