      SUBROUTINE EFOVLP(dmax,osmin,NEWMOD,NVAR,lorjk,*)                 CSTP             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      COMMON /NUMCAL/ NUMCAL                                            IR1294
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      dimension xo(maxpar)
      logical lorjk
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DATA ICALCN /0/                                                   IR1294
C
C  ON THE FIRST STEP SIMPLY DETERMINE WHICH MODE TO FOLLOW
C
c     IF(NSTEP.EQ.1) THEN
      IF(ICALCN.NE.NUMCAL) THEN                                         IR1294
         ICALCN=NUMCAL                                                  IR1294
         IF(MODE.GT.NVAR)THEN
          IF (DOPRNT) WRITE(6,*)'ERROR!! MODE IS LARGER THAN NVAR',MODE CIO
      RETURN 1                                                           CSTP (stop)
         ENDIF
         IT=MODE
         if (DOPRNT.AND.(iprnt.ge.1)) WRITE(6,40) MODE                  CIO
   40 FORMAT(5X,'HESSIAN MODE FOLLOWING SWITCHED ON'/
     1     '     FOLLOWING MODE ',I3)
C
      ELSE
C
C  ON SUBSEQUENT STEPS DETERMINE WHICH HESSIAN EIGENVECTOR HAS
C  THE GREATEST OVERLAP WITH THE MODE WE ARE FOLLOWING
C
         IT=1
         lorjk=.false.
         TOVLP=DDOT(NVAR,U(1,1),1,VMODE,1)                              IR0494
         TOVLP=ABS(TOVLP)
c        xo(1)=tovlp
         DO 10 I=2,NVAR
            OVLP=DDOT(NVAR,U(1,I),1,VMODE,1)                            IR0494
            OVLP=ABS(OVLP)
c           xo(i)=ovlp
            IF(OVLP.GT.TOVLP) THEN
               TOVLP=OVLP
               IT=I
            ENDIF
   10    CONTINUE
C
         if (iprnt.ge.5) then
         do 11 j=1,5
         xxx=0.d0
         do 12 i=1,nvar
         if (xo(i).gt.xxx)ix=i
         if (xo(i).gt.xxx)xxx=xo(i)
12       continue
         xo(ix)=0.d0
         IF (DOPRNT) write(6,*)'overlaps',ix,xxx                        CIO
11       continue
         endif

         if(DOPRNT.AND.(iprnt.ge.1)) WRITE(6,30) IT,TOVLP               CIO
         if (tovlp.lt.omin) then
            if (dmax.gt.osmin) then
            lorjk=.true.
            if (DOPRNT.AND.(iprnt.ge.1)) write(6,31)omin                CIO
            return
            else
            if (DOPRNT.AND.(iprnt.ge.1)) write(6,32)omin,dmax,osmin     CIO
            endif
         endif
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
 9999 RETURN 1                                                          CSTP
      END
