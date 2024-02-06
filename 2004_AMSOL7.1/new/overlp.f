      SUBROUTINE OVERLP(DMAX,OSMIN,NEWMOD,NVAR,LORJK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                GDH1095
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      DIMENSION XO(MAXPAR)
      LOGICAL LORJK
      SAVE
C
C  ON THE FIRST STEP SIMPLY DETERMINE WHICH MODE TO FOLLOW
C
      IF (ICONTR(28).EQ.1) THEN                                         DJG0295
         ICONTR(28)=2                                                   DJG0295
         IF(MODE.GT.NVAR)THEN
            WRITE(JOUT,*)'ERROR!! MODE IS LARGER THAN NVAR',MODE
          ISTOP=1                                                       GDH1095
          IWHERE=150                                                    GDH1095
          RETURN                                                        GDH1095
         ENDIF
         IT=MODE
         IF (IPRNT.GE.1) WRITE(JOUT,40) MODE
   40 FORMAT(5X,'HESSIAN MODE FOLLOWING SWITCHED ON'/
     1     '     FOLLOWING MODE ',I3)
C
      ELSE
C
C  ON SUBSEQUENT STEPS DETERMINE WHICH HESSIAN EIGENVECTOR HAS
C  THE GREATEST OVERLAP WITH THE MODE WE ARE FOLLOWING
C
         IT=1
       LORJK=.FALSE.
         TOVLP=DOT(U(1,1),VMODE,NVAR)
         TOVLP=ABS(TOVLP)
C        XO(1)=TOVLP
         DO 10 I=2,NVAR
            OVLP=DOT(U(1,I),VMODE,NVAR)
            OVLP=ABS(OVLP)
C           XO(I)=OVLP
            IF(OVLP.GT.TOVLP) THEN
               TOVLP=OVLP
               IT=I
            ENDIF
   10    CONTINUE
C
         IF(IPRNT.GE.1)WRITE(JOUT,30) IT,TOVLP
       IF (TOVLP.LT.OMIN) THEN
          IF (DMAX.GT.OSMIN) THEN
          LORJK=.TRUE.
          IF (IPRNT.GE.1)WRITE(JOUT,31)OMIN
          RETURN
          ELSE
          IF (IPRNT.GE.1)WRITE(JOUT,32)OMIN,DMAX,OSMIN
          ENDIF
         ENDIF
      ENDIF
   30 FORMAT(5X,'OVERLAP OF CURRENT MODE',I3,' WITH PREVIOUS MODE IS ',
     $       F6.3)
   31 FORMAT(5X,'OVERLAP LESS THAN OMIN',
     1F6.3,' REJECTING PREVIOUS STEP')
   32 FORMAT(5X,'OVERLAP LESS THAN OMIN',F6.3,' BUT TRUST RADIUS',F6.3,
     $          ' IS LESS THAN',F6.3,/,5X,' ACCEPTING STEP')
C
C  SAVE THE EIGENVECTOR IN VMODE
C
      DO 20 I=1,NVAR
         VMODE(I)=U(I,IT)
   20 CONTINUE
C
      NEWMOD=IT
      RETURN
C
      END
