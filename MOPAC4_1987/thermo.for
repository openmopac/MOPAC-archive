      SUBROUTINE THERMO(A,B,C,LINEAR,SYM,WT,VIBS,NVIBS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VIBS(*)
      LOGICAL LINEAR
      CHARACTER*80 KEYWRD, KOMENT, TITLE
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
C
C
C   THERMO CALCULATES THE VARIOUS THERMODYNAMIC QUANTITIES FOR A
C   SPECIFIED TEMPERATURE GIVEN THE VIBRATIONAL FREQUENCIES, MOMENTS OF
C   INERTIA, MOLECULAR WEIGHT AND SYMMETRY NUMBER.
C
C   REFERENCE: G.HERZBERG MOLECULAR SPECTRA AND MOLECULAR STRUCTURE
C              VOL 2, CHAP. 5
C
C   ----    TABLE OF SYMMETRY NUMBERS    ----
C
C        C1 CI CS     1      D2 D2D D2H  4       C(INF)V   1
C        C2 C2V C2H   2      D3 D3D D3H  6       D(INF)H   2
C        C3 C3V C3H   3      D4 D4D D4H  8       T TD     12
C        C4 C4V C4H   4      D6 D6D D6H  12      OH       24
C        C6 C6V C6H   6      S6          3
C
C
C   PROGRAM LIMITATIONS:  THE EQUATIONS USED ARE APPROPRIATE TO THE
C   HIGH TEMPERATURE LIMIT AND WILL BEGIN TO BE INADEQUATE AT TEMPERA-
C   TURES BELOW ABOUT 100 K.  SECONDLY THIS PROGRAM IS ONLY APPROPRIATE
C   IN THE CASE OF MOLECULES IN WHICH THERE IS NO FREE ROTATION
C
C
C
C
*******************************************************************
*
*  THE FOLLOWING CONSTANTS ARE NOW DEFINED:
*          PI  = CIRCUMFERENCE TO DIAMETER OF A CIRCLE
*          R   = GAS CONSTANT IN CALORIES/MOLE
*          H   = PLANCK'S CONSTANT IN ERG-SECONDS
*          AK  = BOLTZMANN CONSTANT IN ERG/DEGREE
*          AC  = SPEED OF LIGHT IN CM/SEC
*******************************************************************
      DATA PI /3.14159D0 /
      DATA R/1.98726D0/
      DATA H/6.626D-27/
      DATA AK/1.3807D-16/
      DATA AC/2.99776D+10/
*******************************************************************
      IT1=200
      IT2=400
      ISTEP=10
      I=INDEX(KEYWRD,'THERMO(')
      IF(I.NE.0) THEN
         IT1=READA(KEYWRD,I)
         IF(IT1.LT.100) THEN
            WRITE(6,'(//10X,''TEMPERATURE RANGE STARTS TOO LOW,'',
     1'' LOWER BOUND IS RESET TO 30K'')')
            IT1=100
         ENDIF
         I=INDEX(KEYWRD,',')
         IF(I.NE.0) THEN
            KEYWRD(I:I)=' '
            IT2=READA(KEYWRD,I)
            IF(IT2.LT.IT1) THEN
               IT2=IT1+200
               ISTEP=10
               GOTO 10
            ENDIF
            I=INDEX(KEYWRD,',')
            IF(I.NE.0) THEN
               KEYWRD(I:I)=' '
               ISTEP=READA(KEYWRD,I)
               IF(ISTEP.LT.1)ISTEP=1
            ELSE
               ISTEP=(IT2-IT1)/20
               IF(ISTEP.EQ.0)ISTEP=1
               IF(ISTEP.GE.2.AND. ISTEP.LT.5)ISTEP=2
               IF(ISTEP.GE.5.AND. ISTEP.LT.10)ISTEP=5
               IF(ISTEP.GE.10.AND. ISTEP.LT.20)ISTEP=10
               IF(ISTEP.GT.20.AND. ISTEP.LT.50)ISTEP=20
               IF(ISTEP.GT.50.AND. ISTEP.LT.100)ISTEP=50
               IF(ISTEP.GT.100)ISTEP=100
            ENDIF
         ELSE
            IT2=IT1+200
         ENDIF
      ENDIF
   10 CONTINUE
      WRITE(6,'(//,A)')TITLE
      WRITE(6,'(A)')KOMENT
      IF(LINEAR) THEN
         WRITE(6,'(//10X,''MOLECULE IS LINEAR'')')
      ELSE
         WRITE(6,'(//10X,''MOLECULE IS NOT LINEAR'')')
      ENDIF
      WRITE(6,'(/10X,''THERE ARE'',I3,'' GENUINE VIBRATIONS IN THIS '',
     1''SYSTEM'')')NVIBS
      WRITE(6,20)
   20 FORMAT(10X,'THIS THERMODYNAMICS CALCULATION IS LIMITED TO',/
     110X,'MOLECULES WHICH HAVE NO INTERNAL ROTATIONS'//)
      WRITE(6,'(//20X,''CALCULATED THERMODYNAMIC PROPERTIES'')')
      WRITE(6,'(/,''   TEMP. (K)   PARTITION FUNCTION      ENTHALPY'',
     1''     HEAT CAPACITY    ENTROPY'')')
      WRITE(6,'(  ''                                        CAL/MOL'',
     1''       CAL/K/MOL     CAL/K/MOL'',/)')
      DO 30 I=1,NVIBS
   30 VIBS(I)=ABS(VIBS(I))
      DO 70 ITEMP=IT1,IT2,ISTEP
         T=ITEMP
C   ***   INITIALISE SOME VARIABLES   ***
         C1=H*AC/AK/T
         QV=1.0D0
         HV=0.0D0
         E0=0.0D0
         CPV=0.0D0
         SV1=0.0D0
         SV2=0.0D0
C   ***   CONSTRUCT THE FREQUENCY DEPENDENT PARTS OF PARTITION FUNCTION
         DO 40 I=1,NVIBS
            WI=VIBS(I)
            EWJ=EXP(-WI*C1)
            QV=QV/(1-EWJ)
            HV=HV+WI*EWJ/(1-EWJ)
            E0=E0+WI
            CPV=CPV+WI*WI*EWJ/(1-EWJ)/(1-EWJ)
            SV1=SV1+LOG(1.0D0-EWJ)
   40    SV2=SV2+WI*EWJ/(1-EWJ)
C   ***   FINISH CALCULATION OF VIBRATIONAL PARTS   ***
         HV=HV*R*H*AC/AK
         E0=E0*1.4295D0
         CPV=CPV*R*C1*C1
         SV=SV2*R*C1-R*SV1
C   ***   NOW CALCULATE THE ROTATIONAL PARTS  (FIRST LINEAR MOLECULES
         IF(.NOT.LINEAR) GOTO 50
         QR=1/(C1*A*SYM)
         HR=R*T
         CPR=R
         SR=R*(LOG(T*AK/(H*AC*A*SYM)))+R
         GOTO 60
   50    QR=SQRT(PI/(A*B*C*C1*C1*C1))/SYM
         HR=3.0D0*R*T/2.0D0
         CPR=3.0D0*R/2.0D0
         SR=0.5D0*R*(3.D0*LOG(T*AK/(H*AC))
     1-2.D0*LOG(SYM)+LOG(PI/(A*B*C))+3.D0)
   60    CONTINUE
C   ***   CALCULATE INTERNAL CONTRIBUTIONS   ***
         QINT=QV*QR
         HINT=HV+HR
         CPINT=CPV+CPR
         SINT=SV+SR
C   ***   CONSTRUCT TRANSLATION CONTRIBUTIONS   ***
         QTR=(SQRT(2.D0*PI*WT*T*AK*1.6606D-24)/H)**3
         HTR=5.0D0*R*T/2.0D0
         CPTR=5.0D0*R/2.0D0
         STR=2.2868D0*(5.0D0*LOG10(T)+3.0D0*LOG10(WT))-2.3135D0
C   ***   CONSTRUCT TOTALS   ***
         CPTOT=CPTR+CPINT
         STOT=STR+SINT
         HTOT=HTR+HINT
C   ***   OUTPUT SECTION   ***
         WRITE(6,'(/,I7,''  VIB.'',G18.4
     1           ,6X,3F14.8        )')ITEMP,QV,  HV,  CPV,  SV
         WRITE(6,'(7X,''  ROT.'',G13.3
     1           ,6X,3F14.3        )')      QR,  HR,  CPR,  SR
         WRITE(6,'(7X,''  INT.'',G13.3
     1           ,6X,3F14.3        )')      QINT,HINT,CPINT,SINT
         WRITE(6,'(7X,''  TRA.'',G13.3,6X,3F14.3)')
     1                                      QTR, HTR, CPTR, STR
         WRITE(6,'(7X,''  TOT.'',13X,7X,3F14.4)')
     1                                      HTOT,CPTOT,STOT
   70 CONTINUE
      END
