      SUBROUTINE SM1ARA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C
C   THIS PRINTS OUT THE SASA'S FOR SM1A
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C
C******************************************************************************
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      DIMENSION EST(100)
      DO 50 M=1,100
         EST(M)=0.D0                                                    CC1290
50    CONTINUE
      DO 150 M=1,100
         DO 100 N=1,NUMAT                                               CC1290
            IF(ITYPE(N).NE.M) GO TO 100                                 CC1290
            EST(M)=EST(M)+ATAR(N)                                       CC1290
100      CONTINUE                                                       CC1290
150   CONTINUE
      WRITE (JOUT,'(/)')                                                GL0592
      DO 200 N=1,18                                                     CC1290
         IF(EST(N).EQ.0.D0) GO TO 200                                   CC1290
         WRITE(JOUT, 1000) N, EST(N)                                    GL0592
200   CONTINUE                                                          CC1290
1000  FORMAT(1X,'Total area for environment type ', I2, ' is ',         GL0592
     1       F7.2, ' A**2')                                             GL0592
      RETURN
      END
