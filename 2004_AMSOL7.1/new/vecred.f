      SUBROUTINE VECRED(A,NUMB, IR) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION  A(*) 
C********************************************************************** 
C 
C  VECRED READS A LOWER-HALF TRIANGLE OF A SQUARE MATRIX, THE 
C         LOWER-HALF TRIANGLE BEING STORED IN PACKED FORM IN THE 
C         ARRAY "A" 
C 
C ON INPUT: 
C      A      = ARRAY TO BE PRINTED 
C      NUMB   = SIZE OF ARRAY TO BE PRINTED 
C      IR     = CHANNEL NUMBER FOR INPUT 
C(REF) NUMAT  = NUMBER OF ATOMS IN THE MOLECULE (THIS IS NEEDED TO 
C               DECIDE IF AN ATOMIC ARRAY OR ATOMIC ORBITAL ARRAY IS 
C               TO BE PRINTED 
C(REF) NAT    = LIST OF ATOMIC NUMBERS 
C(REF) NFIRST = LIST OF ORBITAL COUNTERS 
C(REF) NLAST  = LIST OF ORBITAL COUNTERS 
C 
C 
C********************************************************************* 
      CHARACTER*2 ATORBS(9) 
      SAVE
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/ 
      NUMB=ABS(NUMB) 
      LIMIT=(NUMB*(NUMB+1))/2 
      KK=8 
      NA=1 
   20 LL=0 
      M=MIN((NUMB+1-NA),6)                                              GCL0393
      MA=2*M+1 
      M=NA+M-1 
      READ( IR,'(A)')DUMC,DUMC,DUMC 
      DO 40 I=NA,NUMB 
         LL=LL+1 
         K=(I*(I-1))/2 
         L=MIN((K+M),(K+I))                                             GCL0393
         K=K+NA 
         IF ((KK+LL).LE.50) GO TO 30 
         READ( IR,'(A)')DUMC,DUMC,DUMC 
         KK=4 
         LL=0 
   30    READ( IR,'(A9,10F11.6)')DUMY,(A(N),N=K,L) 
   90 FORMAT (1H ,A2,1X,A2,I3,10F11.6) 
   40 CONTINUE 
      IF (L.GE.LIMIT) GO TO 50 
      KK=KK+LL+4 
      NA=M+1 
      IF ((KK+NUMB+1-NA).LE.50) GO TO 20 
      KK=4 
C#      READ( IR,'(A)')DUMC 
      GO TO 20 
   50 CONTINUE 
C 
   60 FORMAT (1H0/9X,10(2X,A2,1X,A2,I3,1X)) 
   70 FORMAT (1H ,21A6) 
   80 FORMAT (1H1) 
C#      CALL VECPRT(A,NUMB) 
C 
      RETURN 
      END 
