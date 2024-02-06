      SUBROUTINE VECPRT (A,NUMB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION  A(*)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /ELEMTS/ ELEMNT(107)
C**********************************************************************
C
C  VECPRT PRINTS A LOWER-HALF TRIANGLE OF A SQUARE MATRIX, THE
C         LOWER-HALF TRIANGLE BEING STORED IN PACKED FORM IN THE
C         ARRAY "A"
C
C ON INPUT:
C      A      = ARRAY TO BE PRINTED
C      NUMB   = SIZE OF ARRAY TO BE PRINTED
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
      DIMENSION NATOM(200)
      CHARACTER * 6 LINE(21)
      CHARACTER*2 ELEMNT,ATORBS(9), ITEXT(200),JTEXT(200)
      SAVE
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/
      IF(NUMAT.NE.0.AND.NUMAT.EQ.NUMB) THEN
C
C    PRINT OVER ATOM COUNT
C
         DO 10 I=1,NUMAT
            ITEXT(I)='  '
            JTEXT(I)=ELEMNT(NAT(I))
            NATOM(I)=I
   10    CONTINUE
      ELSE
         IF (NUMAT.NE.0.AND.NLAST(NUMAT) .EQ. NUMB) THEN
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
   20          CONTINUE
   30       CONTINUE
         ELSE
            NUMB=ABS(NUMB)
            DO 40 I=1,NUMB
               ITEXT(I) = '  '
               JTEXT(I) = '  '
   40       NATOM(I)=I
         ENDIF
      END IF
      NUMB=ABS(NUMB)
      DO 50 I=1,21
   50 LINE(I)='------'
      LIMIT=(NUMB*(NUMB+1))/2
      KK=8
      NA=1
   60 LL=0
      M=MIN((NUMB+1-NA),6)                                              GCL0393
      MA=2*M+1
      M=NA+M-1
      WRITE(JOUT,100)(ITEXT(I),JTEXT(I),NATOM(I),I=NA,M)
      WRITE (JOUT,110) (LINE(K),K=1,MA)
      DO 80 I=NA,NUMB
         LL=LL+1
         K=(I*(I-1))/2
         L=MIN((K+M),(K+I))                                             GCL0393
         K=K+NA
         IF ((KK+LL).LE.50) GOTO 70
         WRITE (JOUT,120)
         WRITE (JOUT,100) (ITEXT(N),JTEXT(N),NATOM(N),N=NA,M)
         WRITE (JOUT,110) (LINE(N),N=1,MA)
         KK=4
         LL=0
   70    WRITE (JOUT,130) ITEXT(I),JTEXT(I),NATOM(I),(A(N),N=K,L)
   80 CONTINUE
      IF (L.GE.LIMIT) RETURN
      KK=KK+LL+4
      NA=M+1
      IF ((KK+NUMB+1-NA).LE.50) GOTO 60
      KK=4
      WRITE (JOUT,120)
      GOTO 60
C
  100 FORMAT (/,10X,10(2X,A2,1X,A2,I3,1X))                              GL0492
  110 FORMAT (1X,21A6)                                                  GL0492
  120 FORMAT (/,1X)                                                     GL0492
  130 FORMAT (1X,A2,1X,A2,I3,10F11.5)                                   DL0397
C
      END
