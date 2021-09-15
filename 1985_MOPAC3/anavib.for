      SUBROUTINE ANAVIB(COORD,EIGS,N3,VIBS,RIJ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,100),EIGS(N3),VIBS(N3,N3), RIJ(900)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /ELEMTS/ ELEMNT(107)
      LOGICAL VIB1, VIB2
      CHARACTER*2 ELEMNT
      DIMENSION VANRAD(86)
      DATA VANRAD/
     1   0.32,0.93,
     2   1.23, 0.90, 0.82, 0.77, 0.75, 0.73, 0.72, 0.71,
     3   1.54, 1.36, 1.18, 1.11, 1.06, 1.02, 0.99, 0.98,
     4   2.03, 1.74, 1.44, 1.32, 1.22, 1.18, 1.17, 1.17, 1.16,
     5   1.15, 1.17, 1.25, 1.26, 1.22, 1.20, 1.16, 1.14, 1.12,
     6   2.16, 1.91, 1.62, 1.45, 1.34, 1.30, 1.27, 1.25, 1.25,
     7   1.28, 1.34, 1.48, 1.44, 1.41, 1.40, 1.36, 1.33, 1.31,
     8   2.35, 1.98, 1.69,
     9   1.65, 1.65, 1.64, 1.63, 1.62, 1.85, 1.61, 1.59, 1.59, 1.58,
     1   1.57, 1.56, 1.56, 1.56,
     2   1.44, 1.34, 1.30, 1.28, 1.26, 1.27, 1.30, 1.34,
     3   1.49, 1.48, 1.47, 1.46, 1.46, 1.45,1.45/
      N3=NUMAT*3
C
C    COMPUTE INTERATOMIC DISTANCES.
C
      L=0
      DO 10 I=1,NUMAT
         DO 10 J=1,I
            L=L+1
   10 RIJ(L)=SQRT((COORD(1,J)-COORD(1,I))**2+
     1            (COORD(2,J)-COORD(2,I))**2+
     2            (COORD(3,J)-COORD(3,I))**2)
C
C     ANALYSE VIBRATIONS
C
      WRITE(6,'(//10X,''DESCRIPTION OF VIBRATIONS'',/)')
      DO 30 K=1,N3
         VIB1=.TRUE.
         VIB2=.TRUE.
         J3=0
         L=0
         DO 20 J=1,NUMAT
            XJ=COORD(1,J)
            YJ=COORD(2,J)
            ZJ=COORD(3,J)
            J1=J3+1
            J2=J1+1
            J3=J2+1
            I3=0
            DO 20 I=1,J
               XI=COORD(1,I)
               YI=COORD(2,I)
               ZI=COORD(3,I)
               I1=I3+1
               I2=I1+1
               I3=I2+1
               L=L+1
               VDW=(VANRAD(NAT(I))+VANRAD(NAT(J)))*1.4
               IF(   RIJ(L)  .LT.  VDW) THEN
                  X= VIBS(J1,K)-VIBS(I1,K)
                  Y= VIBS(J2,K)-VIBS(I2,K)
                  Z= VIBS(J3,K)-VIBS(I3,K)
                  SHIFT=X*X+Y*Y+Z*Z
                  IF(SHIFT .GT. 0.1) THEN
                     SHIFT=SQRT(SHIFT)
                     RADIAL=ABS(X*(XI-XJ)+Y*(YI-YJ)+Z*(ZI-ZJ))
     1                  /(SHIFT*RIJ(L))*100.D0
                     IF (VIB1) THEN
                        WRITE(6,'(/,'' VIB.'',I3,''    ATOMS  '',
     1A2,I2,''  AND  '',A2,I2,''  SHIFT'',
     2F6.2,''  ANGSTROMS'',F7.1,''%  RADIALLY'')')K,ELEMNT(NAT(I)),I,
     3ELEMNT(NAT(J)),J,SHIFT,RADIAL
                        VIB1=.FALSE.
                     ELSEIF (VIB2) THEN
                        VIB2=.FALSE.
                        WRITE(6,'('' FREQ.   '',F8.2,2X,
     1A2,I2,''       '',A2,I2,7X,F6.2,''           '',
     2F7.1,''%          '')')EIGS(K),ELEMNT(NAT(I)),I,
     3ELEMNT(NAT(J)),J,SHIFT,RADIAL
                     ELSE
                        WRITE(6,'(''                   '',
     1A2,I2,''       '',A2,I2,7X,F6.2,''           '',
     2F7.1,''%          '')')ELEMNT(NAT(I)),I,
     3ELEMNT(NAT(J)),J,SHIFT,RADIAL
                     ENDIF
                  ENDIF
               ENDIF
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
