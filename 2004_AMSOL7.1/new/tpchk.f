      SUBROUTINE TPCHK(NATOMS,LABELS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
      INCLUDE 'FFILES.i'                                                GDH1095
      DIMENSION LABELS(*)
C******************************************************************************
C
C     TPCHK CHECKS FOR THE USE OF ATOM TYPES WHICH ARE INCOMPATIBLE WITH
C     THE MODEL YOU ARE USING.
C
C     THIS SUBROUTINE WAS CREATED BY GDH IN 04/96
C
C******************************************************************************
      ICHK1=ISM1+ISM1A+ISM2+ISM3+ISM21+ISM23+ISM31+IS5P2P+IS5A2P
      ICHK2=ISM22
      ICHK3=ISM5U+ISM5A+ISM5P+IPDS5U+IPDS5A+IPDS5P+ISM50                GDH0797
      DO 10 IJJ=1,NATOMS-1
         ICMP=LABELS(IJJ)
         ICNT=0
         IFAILA=0
         IF (LABELS(IJJ).LT.90) THEN
         IF (ICHK1.EQ.1) THEN
            IF (ICMP.EQ.1) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.6) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.7) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.8) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.9) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.15) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.16) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.17) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.35) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.53) THEN
               ICNT=ICNT+1
            ENDIF
            IF (ICNT.EQ.0) IFAILA=1
         ELSE IF (ICHK2.EQ.1) THEN
            IF (ICMP.EQ.1) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.6) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.7) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.8) THEN
               ICNT=ICNT+1
            ENDIF
            IF (ICNT.EQ.0) IFAILA=1
         ELSE IF (ICHK3.EQ.1) THEN
            IF (ICMP.EQ.1) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.6) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.7) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.8) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.9) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.16) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.17) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.35) THEN
               ICNT=ICNT+1
            ELSE IF (ICMP.EQ.53) THEN
               ICNT=ICNT+1
            ENDIF
            IF (ICNT.EQ.0) IFAILA=1
         ENDIF
         IF (IFAILA.EQ.1.AND.IDEV.EQ.0) THEN                            GDH0497
             WRITE(JOUT,110)
             ISTOP=1                                                    GDH1095
             IWHERE=170                                                 GDH1095
             RETURN                                                     GDH1095
         ELSE IF (IFAILA.EQ.1) THEN                                     GDH0497
             WRITE(JOUT,110)                                            GDH0497
         ENDIF
         ENDIF
10     CONTINUE
       RETURN
110   FORMAT(/,'WARNING: AN ATOM TYPE WAS USED WHICH IS INCOMPATIBLE',
     1       /,'         WITH THE SOLVATION MODEL CHOSEN.')
       END
