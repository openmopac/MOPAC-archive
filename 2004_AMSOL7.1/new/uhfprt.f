      SUBROUTINE UHFPRT(SZ,SS2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         INCLUDE 'KEYS.i'
         INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C
C   THIS SUBROUTINE PRINTS OUT INFORMATION FOR A UHF CALCULATION
C   AND HYPERFINE CALCULATIONS.
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C
C******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /OPTIMI / IMP,IMP0          
      DIMENSION Q(MAXORB)
      CHARACTER*2 ELEMNT
C
      LINEAR=(NORBS*(NORBS+1))/2
      SZ=ABS(NALPHA-NBETA)*0.5D0
      SS2=SZ*SZ
      L=0
      DO 120 I=1,NORBS
         DO 110 J=1,I
            L=L+1
            PA(L)=PA(L)-PB(L)
110      SS2=SS2+PA(L)**2
120   SS2=SS2-0.5D0*PA(L)**2
      WRITE(JOUT,1000)SZ                                                DJG0995
      WRITE(JOUT,1010)SS2                                               DJG0995
      IF(ISPIN.NE.0) THEN                                               DJG0995
         WRITE(JOUT,1020)                                               DJG0995
         CALL VECPRT(PA,NORBS)
      ELSE
         WRITE(JOUT,1030)                                               DJG0995
         WRITE(JOUT,1050)(PA((I*(I+1))/2),I=1,NORBS)                    DJG0995
      ENDIF
C
C  WORK OUT THE HYPERFINE COUPLING CONSTANTS.
C
      IF(IHYPER.NE.0) THEN                                              DJG0995
         WRITE(JOUT,1040)                                               DJG0995
         J=(NALPHA-1)*NORBS
         DO 130 K=1,NUMAT
            I=NFIRST(K)
130      Q(K)=PA((I*(I+1))/2)*0.3333333D0+C(I+J)**2*0.66666666D0
         WRITE(JOUT,1060) (ELEMNT(NAT(I)),I,Q(I),I=1,NUMAT)             DJG0995
      ENDIF
      DO 140 I=1,LINEAR
140   PA(I)=P(I)-PB(I)
1000  FORMAT(/,20X,'(SZ)    =',F10.6)                                   DJG0995
1010  FORMAT(20X,'(S**2)  =',F10.6)                                     DJG0995
1020  FORMAT(/,10X,'Spin density matrix')                               DJG0995
1030  FORMAT(/,10X,'Atomic orbital spin populations',/)                 DJG0995
1040  FORMAT(/,14X,'Hyperfine coupling coefficients',/)                 DJG0995
1050  FORMAT(8F10.5)                                                    DJG0995
1060  FORMAT(5(2X,A2,I2,F9.5,1X))                                       DJG0995
      RETURN
      END
