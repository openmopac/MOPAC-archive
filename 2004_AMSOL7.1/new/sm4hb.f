      SUBROUTINE SM4HB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'SIZES.i'
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES SURFACE TENSIONS WHICH ARE LATER
C   MULTIPLIED TIMES SASA TO GET THE CDS TERM.
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN BORNPL AND BRNPL2
C
C******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)             
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /HBONDA/ HBONDO(100),CBONDO,COBND5,CNBNDO,CBOND5,          GDH1196
     .                OOBNDO,ONBNDO,SSBNDO,OPBND5,CLCBND,BRCBND,        GDH1196
     .                FCBNDO,CNBOC2,TNCBC3,TNCBC2,HOHBND,HNNBND,        GDH1296
     .                SPBNDO,OSIBND                                     PDW0601
      COMMON /ONESCM/ ICONTR(100)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      IF (ICONTR(59).EQ.1) THEN 
         ICONTR(59)=2
         SQRT3=SQRT(3.0D0)
      ENDIF
      DO 50 M=1,100                                                     DJG0994
         HBONDO(M)=0.0D0                                                DJG0994
50    CONTINUE                                                          DJG0994
      DO 950 IAT=1,NUMAT
         NATIAT=NAT(IAT)
         ARED3=ATAR(IAT)*1D-3
         HB(IAT)=0.0D0                                                  DJG0295
         IF (NAT(IAT).EQ.1) THEN                                        DJG0994
            N=NFIRST(IAT)                                               DJG0994
            DO 700 J=1,NUMAT                                            DJG0994
               IF (J.LT.IAT) THEN                                       DJG0994
                  DO 600 M=NFIRST(J),NLAST(J)                           DJG0994
                     MN=N*(N-1)/2+M                                     DJG0994
                     HB(IAT)=HB(IAT)+P(MN)*P(MN)*HRFN4(NAT(J))*ARED3    DJG0295
                     HBONDO(NAT(J))=HBONDO(NAT(J))+P(MN)*P(MN)*ARED3    DJG0994
     1                              *1000                               DJG0994
600               CONTINUE                                              DJG0994
               ELSE IF (J.GT.IAT) THEN                                  DJG0994
                  DO 650 M=NFIRST(J),NLAST(J)                           DJG0994
                     MN=M*(M-1)/2+N                                     DJG0994
                     HB(IAT)=HB(IAT)+P(MN)*P(MN)*HRFN4(NAT(J))*ARED3    DJG0295
                     HBONDO(NAT(J))=HBONDO(NAT(J))+P(MN)*P(MN)*ARED3    DJG0994
     1                              *1000                               DJG0994
650               CONTINUE                                              DJG0994
               ENDIF                                                    DJG0994
700         CONTINUE                                                    DJG0994
         ENDIF
CCC These are the CC bond order terms (for the alkane model)            CCC0495
         IF (NAT(IAT).EQ.6) THEN                                        DJG1094
            DO 925 J=1,NUMAT                                            DJG1094
               IF (J.LT.IAT.AND.NAT(J).EQ.6) THEN                       DJG1094
                  BSUM=0.0D0                                            DJG1094
                  DO 800 N=NFIRST(IAT),NLAST(IAT)                       DJG1094
                     DO 750 M=NFIRST(J),NLAST(J)                        DJG1094
                        MN=N*(N-1)/2+M                                  DJG1094
                        BSUM=BSUM+P(MN)*P(MN)                           DJG1094
750                  CONTINUE                                           DJG1094
800               CONTINUE                                              DJG1094
                  P3=BSUM*BSUM*BSUM                                     DJG1094
                  HB(IAT)=HB(IAT)+P3*CRFNH4*ARED3                       DJG0295
                  CBONDO=CBONDO+P3*ARED3*1000                           DJG1094
               ELSE IF (J.GT.IAT.AND.NAT(J).EQ.6) THEN                  DJG1094
                  BSUM=0.0D0                                            DJG1094
                  DO 900 N=NFIRST(IAT),NLAST(IAT)                       DJG1094
                     DO 850 M=NFIRST(J),NLAST(J)                        DJG1094
                        MN=M*(M-1)/2+N                                  DJG1094
                        BSUM=BSUM+P(MN)*P(MN)                           DJG1094
850                  CONTINUE                                           DJG1094
900               CONTINUE                                              DJG1094
                  P3=BSUM*BSUM*BSUM                                     DJG1094
                  HB(IAT)=HB(IAT)+P3*CRFNH4*ARED3                       DJG0295
                  CBONDO=CBONDO+P3*ARED3*1000                           DJG1094
               ENDIF                                                    DJG1094
925         CONTINUE                                                    DJG1094
         ENDIF                                                          DJG1094
950   CONTINUE
      RETURN
      END
