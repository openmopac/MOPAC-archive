      SUBROUTINE UPDHES(SVEC,TVEC,NVAR,IUPD,*)                          CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      DIMENSION TVEC(*),SVEC(*)
      COMMON /NUMCAL/NUMCAL                                              IR1294
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),                  
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C
      DATA ZERO/0.0D0/
C
C  UPDATING OF THE HESSIAN
C  DEPENDS ON CURRENT GRADIENTS, OLD GRADIENTS AND THE
C  CORRECTION VECTOR USED ON THE LAST CYCLE
C  SVEC & TVEC ARE FOR TEMPORARY STORAGE
C
C  2 UPDATING PROCEDURES ARE POSSIBLE
C  (I)   THE POWELL UPDATE
C        THIS PRESERVES THE SYMMETRIC CHARACTER OF THE HESSIAN
C        WHILST ALLOWING ITS EIGENVALUE STRUCTURE TO CHANGE.
C        IT IS THE DEFAULT UPDATE FOR A TRANSITION STATE SEARCH
C  (II)  THE BFGS UPDATE
C        THIS UPDATE HAS THE IMPORTANT CHARACTERISTIC OF RETAINING
C        POSITIVE DEFINITENESS (NOTE: THIS IS NOT RIGOROUSLY
C        GUARANTEED, BUT CAN BE CHECKED FOR BY THE PROGRAM).
C        IT IS THE DEFAULT UPDATE FOR A MINIMUM SEARCH
C
C     SWITCH : IUPD
C       IUPD = 0  :  SKIP UPDATE
C       IUPD = 1  :  POWELL
C       IUPD = 2  :  BFGS
C
      ICALCN=0                                                          LF0610 (nowhere else defined)
      IF(ICALCN.NE.NUMCAL) THEN                                         IR1294
         ICALCN=NUMCAL                                                  IR1294
         IF(IPRNT.GE.2.AND.DOPRNT) THEN                                 CIO
            IF (IUPD.EQ.0)WRITE(6,90)
            IF (IUPD.EQ.1)WRITE(6,80)
            IF (IUPD.EQ.2)WRITE(6,120)
         ENDIF
      ENDIF
      IF(IUPD.EQ.0) RETURN
      DO 5 I=1,NVAR
         TVEC(I)=ZERO
 5    CONTINUE
      DO 10 J=1,NVAR
         DO 10 I=1,NVAR
            TVEC(I)=TVEC(I) + HESS(I,J)*D(J)
   10 CONTINUE
C
      IF(IUPD.EQ.1) THEN
C
C   (I) POWELL UPDATE
C
         DO 20 I=1,NVAR
            TVEC(I)=GRAD(I)-OLDF(I)-TVEC(I)
            sVEC(I)=GRAD(I)-OLDF(I)
   20    CONTINUE
         DDS=DD*DD
         DDTD=DDOT(NVAR,TVEC,1,D,1)                                     IR0494
         DDTD=DDTD/DDS
C
         DO 40 I=2,NVAR
            DO 30 J=1,I-1
               TEMP=TVEC(I)*D(J) + D(I)*TVEC(J) - D(I)*DDTD*D(J)
               HESS(I,J)=HESS(I,J)+TEMP/DDS
               HESS(J,I)=HESS(I,J)
   30       CONTINUE
   40    CONTINUE
         DO 45 I=1,NVAR
            TEMP=D(I)*(2.0D0*TVEC(I) - D(I)*DDTD)
            HESS(I,I)=HESS(I,I)+TEMP/DDS
 45      CONTINUE
C
      ENDIF
      IF (IUPD.EQ.2) THEN
C
C  (II) BFGS UPDATE
C
         DO 50 I=1,NVAR
            SVEC(I)=GRAD(I)-OLDF(I)
   50    CONTINUE
         DDS=DDOT(NVAR,SVEC,1,D,1)                                      IR0494
C
C  IF DDS IS NEGATIVE, RETENTION OF POSITIVE DEFINITENESS IS NOT
C  GUARANTEED. PRINT A WARNING AND SKIP UPDATE THIS CYCLE.
C
cfrj With the current level shift technique I think the Hessian should
cfrj be allowed to aquire negative eigenvalues. Without updating the
cfrj optimization has the potential of stalling
cfrj     IF(DDS.LT.ZERO) THEN
cfrj        WRITE(6,100)
cfrj        WRITE(6,110)
cfrj        RETURN
cfrj     ENDIF
C
         DDTD=DDOT(NVAR,D,1,TVEC,1)                                     IR0494
C
         DO 70 I=2,NVAR
            DO 60 J=1,I-1
               TEMP= (SVEC(I)*SVEC(J))/DDS - (TVEC(I)*TVEC(J))/DDTD
               HESS(I,J)=HESS(I,J)+TEMP
               HESS(J,I)=HESS(I,J)
   60       CONTINUE
   70    CONTINUE
         DO 75 I=1,NVAR
            TEMP= (SVEC(I)*SVEC(I))/DDS - (TVEC(I)*TVEC(I))/DDTD
            HESS(I,I)=HESS(I,I)+TEMP
 75      CONTINUE
      ENDIF
C
C
   80 FORMAT(/,5X,'HESSIAN IS BEING UPDATED USING THE POWELL UPDATE',/)
   90 FORMAT(/,5X,'HESSIAN IS NOT BEING UPDATED',/)
c 100 FORMAT(5X,'WARNING! HEREDITARY POSITIVE DEFINITENESS ENDANGERED')
c 110 FORMAT(5X,'UPDATE SKIPPED THIS CYCLE')
  120 FORMAT(/,5X,'HESSIAN IS BEING UPDATED USING THE BFGS UPDATE',/)
      RETURN
 9999 RETURN 1                                                          CSTP
      END
