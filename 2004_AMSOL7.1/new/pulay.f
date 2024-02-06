      SUBROUTINE PULAY(F,P,N,FPPF,FOCK,EMAT,LFOCK,NFOCK,MSIZE,START,PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
************************************************************************
*
*   PULAY USES DR. PETER PULAY'S METHOD FOR CONVERGENCE.
*         A MATHEMATICAL DESCRIPTION CAN BE FOUND IN
*         "P. PULAY, J. COMP. CHEM. 3, 556 (1982).
*
* ARGUMENTS:-
*         ON INPUT F      = FOCK MATRIX, PACKED, LOWER HALF TRIANGLE.
*                  P      = DENSITY MATRIX, PACKED, LOWER HALF TRIANGLE.
*                  N      = NUMBER OF ORBITALS.
*                  FPPF   = WORKSTORE OF SIZE MSIZE, CONTENTS WILL BE
*                           OVERWRITTEN.
*                  FOCK   =      "       "              "         "
*                  EMAT   = WORKSTORE OF AT LEAST 15**2 ELEMENTS.
*                  START  = LOGICAL, = TRUE TO START PULAY.
*                  PL     = UNDEFINED ELEMENT.
*      ON OUTPUT   F      = "BEST" FOCK MATRIX, = LINEAR COMBINATION
*                           OF KNOWN FOCK MATRICES.
*                  START  = FALSE
*                  PL     = MEASURE OF NON-SELF-CONSISTENCY
*                         = \[325]F,P\[345] = F*P - P*F.
*
************************************************************************
C     COMMON /SCRACH/ WORK(1)
      COMMON /SCRCHR/ WORK1(MORB2), WORK2(MORB2), WORK3(MORB2),         3GL3092
     1                DUM3(6*MAXPAR**2+1-3*MORB2)                       GCL0393
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
C
C
      DIMENSION EMAT(7,7), EVEC(49), COEFFS(7)
      DIMENSION F(*), P(*), FPPF(*), FOCK(*)
      LOGICAL DEBUG, START
      SAVE
      IF (ICONTR(12).EQ.1) THEN                                         GDH0195
         ICONTR(12)=2                                                   GDH0195
         MAXLIM=6
         DEBUG=(IDEBUP.NE.0)                                            DJG0995
         START=.TRUE.
      ENDIF
      IF(START) THEN
         LINEAR=(N*(N+1))/2
         MFOCK=MIN(MSIZE/LINEAR,MAXLIM)
         IF(DEBUG) WRITE(JOUT,
     .    '('' MAXIMUM SIZE IN PULAY:'',I5)')MFOCK
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
      CALL SCOPY (LINEAR,F,1,FOCK(LBASE+1),1)
*
*   NOW FORM /FOCK*DENSITY-DENSITY*FOCK/, AND STORE THIS IN FPPF
*
C     SCALAR VERSION
C     CRAY VERSION
      CALL MAMULT(F,P,N,WORK1,WORK2,WORK3,FPPF(LBASE+1))                3GL3092
*
*   FPPF NOW CONTAINS THE RESULT OF FP - PF.
*   UPDATE SYMMETRIC MATRIX EMAT.
*
      NFOCK1=NFOCK+1
      CALL MXM (FPPF(LBASE+1),1,FPPF,LINEAR,EVEC,NFOCK)
      DO 10 I=1,NFOCK
      EMAT(LFOCK,I)=EVEC(I)
   10 EMAT(I,LFOCK)=EMAT(LFOCK,I)
      PL=EMAT(LFOCK,LFOCK)/LINEAR
      CONST=1.D0/(EMAT(LFOCK,LFOCK)+1D-38)
      L=0
      DO 30 I=1,NFOCK
         DO 20 J=1,NFOCK
            L=L+1
   20    EVEC(L)=EMAT(I,J)*CONST
         L=L+1
   30    EVEC(L)=-1.D0
      DO 40 I=1,NFOCK
         L=L+1
   40    EVEC(L)=-1.D0
      L=L+1
      EVEC(L)=0.D0
      IF (DEBUG) THEN
         WRITE(JOUT,'('' EMAT IN PULAY'')')
         DO 50 I=1,NFOCK1
         K=I+NFOCK1*(I-1)
   50    WRITE(JOUT,'(1P,7E11.4)')(EVEC(J),J=I,K,NFOCK1)
      ENDIF
*********************************************************************
*   THE MATRIX EMAT SHOULD HAVE THE SYMMETRIC FORM
*
*      !<E(1)*E(1)>  <E(1)*E(2)> ...   -1.0!
*      !<E(2)*E(1)>  <E(2)*E(2)> ...   -1.0!
*      !<E(3)*E(1)>  <E(3)*E(2)> ...   -1.0!
*      !<E(4)*E(1)>  <E(4)*E(2)> ...   -1.0!
*      !     .            .      ...     . !
*      !   -1.0         -1.0     ...    0. !
*
*   WHERE <E(I)*E(J)> IS THE SCALAR PRODUCT OF \[325]F,P\[345] FOR ITERATION I
*   TIMES \[325]F,P\[345] FOR ITERATION J.
*
*********************************************************************
      CALL OSINV(EVEC,NFOCK1,D)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(ABS(D).LT.1.D-10)THEN
         START=.TRUE.
         RETURN
      ENDIF
      IF(NFOCK.LT.2) RETURN
      IL=NFOCK*NFOCK1
      DO 60 I=1,NFOCK
   60 COEFFS(I)=-EVEC(I+IL)
      IF(DEBUG) THEN
         WRITE(JOUT,'('' EVEC'')')
         WRITE(JOUT,'(7F11.6)')(COEFFS(I),I=1,NFOCK)
         WRITE(JOUT,'(''    LAGRANGIAN MULTIPLIER (ERROR) =''
     1             ,F13.6)')EVEC(NFOCK1*NFOCK1)
      ENDIF
*
*   EXTRAPOLATED FOCK MATRIX
*
      CALL MXM (FOCK,LINEAR,COEFFS,NFOCK,F,1)
      RETURN
      END
