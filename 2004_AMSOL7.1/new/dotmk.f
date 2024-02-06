      SUBROUTINE DOTMK(IPSUM,NARCS,NPTS,COORV,VQUAD,HSVECT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        INCLUDE 'SIZES.i'
        INCLUDE 'SIZES2.i'
      INTEGER VQUAD,HSVECT,HOLD                                         GDH0493
      DIMENSION COORV(MXPT,3),VQUAD(MXPT),ANGS(160)
      DIMENSION HSVECT(MXPT,14)
      DIMENSION T1COS(400),T1SIN(400)
      NRCS=NARCS
      ROTANG=(PD2)/((NRCS+1)/2.D0)
      NACD2=NRCS/2
      DO 20 I=(-NACD2),(NACD2)
20      ANGS(I+NACD2+1)=ROTANG*I
      IPSUM=0
        DO 50 IA=1,NRCS
c
c       Construct the dots on the surface of the sphere
c
          ZDISP=SIN(ANGS(IA))
          RAD=COS(ANGS(IA))
c
c       Calculate the number of points in the circle
c
          NP=INT(RAD*NPTS)
          XTERM=(2.D0*PI)/NP
c
c       Build the list of theta's
c
          DO 30 J=1,NP
            THETA=(J-1)*XTERM
            T1COS(J)=COS(THETA)
30          T1SIN(J)=SIN(THETA)
c
c       Update the list of points
c
          DO 40 J=IPSUM+1,IPSUM+NP
            COORV(J,1)=T1COS(J-IPSUM)*RAD
            COORV(J,2)=T1SIN(J-IPSUM)*RAD
            COORV(J,3)=ZDISP        
            IF (COORV(J,1).GT.0.D0) THEN
               IF (COORV(J,2).GT.0.D0) THEN
                   IF (COORV(J,3).GT.0.D0) THEN
                      HOLD=1
                   ELSE
                      HOLD=6
                   ENDIF
               ELSE
                   IF (COORV(J,3).GT.0.D0) THEN
                      HOLD=4
                   ELSE
                      HOLD=7
                   ENDIF
               ENDIF
            ELSE
               IF (COORV(J,2).GT.0.D0) THEN
                   IF (COORV(J,3).GT.0.D0) THEN
                      HOLD=2
                   ELSE
                      HOLD=5
                   ENDIF
               ELSE
                   IF (COORV(J,3).GT.0.D0) THEN
                      HOLD=3
                   ELSE
                      HOLD=8
                   ENDIF
                ENDIF
            ENDIF
40          VQUAD(J)=HOLD
          IPSUM=IPSUM+NP
50      CONTINUE
        DO 60 J=1,IPSUM
        X=COORV(J,1)
        Y=COORV(J,2)
        Z=COORV(J,3)
        IF (X.GT.0.D0) THEN
           HSVECT(J,9)=1
           HSVECT(J,12)=0
        ELSE
           HSVECT(J,9)=0
           HSVECT(J,12)=1
        ENDIF
        IF (Y.GT.0.D0) THEN
           HSVECT(J,10)=1
           HSVECT(J,13)=0
        ELSE
           HSVECT(J,10)=0
           HSVECT(J,13)=1
        ENDIF
        IF (Z.GT.0.D0) THEN
           HSVECT(J,11)=1
           HSVECT(J,14)=0
        ELSE
           HSVECT(J,11)=0
           HSVECT(J,14)=1
        ENDIF
        IF (X+Y+Z.GT.0.D0) THEN
           HSVECT(J,1)=1
           HSVECT(J,8)=0
        ELSE
           HSVECT(J,1)=0
           HSVECT(J,8)=1
        ENDIF
        IF (-X+Y+Z.GT.0.D0) THEN
           HSVECT(J,2)=1
           HSVECT(J,7)=0
        ELSE
           HSVECT(J,2)=0
           HSVECT(J,7)=1
        ENDIF
        IF (X+Y-Z.GT.0.D0) THEN
           HSVECT(J,6)=1
           HSVECT(J,3)=0
        ELSE
           HSVECT(J,6)=0
           HSVECT(J,3)=1
        ENDIF
        IF (-X+Y-Z.GT.0.D0) THEN
           HSVECT(J,5)=1
           HSVECT(J,4)=0
        ELSE
           HSVECT(J,5)=0
           HSVECT(J,4)=1
        ENDIF
60      CONTINUE
        RETURN
        END
