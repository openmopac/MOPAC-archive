      SUBROUTINE SM5RHB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'PARAMS.i'
C ---------------------------------------------------------------------+
C     CALCULATE CDS HB(i) WHICH ARE LATER MULTIPLIED TIMES SASA
C     TO GET THE CDS TERM. SM5.x                     CREATED BY GDH 0197
C  INPUT
C     LGR               : .TRUE. IF DERIVATIVES TO BE COMPUTED.
C     R(NUMAT,NUMAT)    : INTERATOMIC DISTANCES.
C     UN(3,NUMAT,NUMAT) : ASSOCIATED UNIT VECTORS.
C  OUTPUT
C     HB(NUMAT)    : SURFACE TENSION TERM.
C     DCDS(3,NUMAT): CARTESIAN DERIVATIVES (COMPUTED ON REQUEST "LGR"):
C                    DCDS(i,j) = d(SUM_OF_HB)/dCOORD(i,j)
C  MODIFICATION NOTES
C     CLEANED, COMMENTED, SPEEDED UP AND IMPLEMENTED GRADIENTS,
C     CREATED "TKK" FOR Tkk FUNCTION AND ITS DERIVATIVE.
C     MATHEMATICAL REGULARIZATION OF O-O TERMS,
C
C ---------------------------------------------------------------------+
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,
     1                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,
     2                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,
     3                HOHST1,HNNST1, SPRFN5,OSIBD5
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /HBONDA/ HBONDO(100),CBONDO,COBND5,CNBNDO,CBOND5,
     1                OOBNDO,ONBNDO,SSBNDO,OPBND5,CLCBND,BRCBND,
     2                FCBNDO,CNBOC2,TNCBC3,TNCBC2,HOHBND,HNNBND,
     3                SPBNDO,OSIBND
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /VOLCOM/ R(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,UN(3,NUMATM,NUMATM)
      COMMON /SCRCHR/ BUF(3*NUMATM),BUF1(3*NUMATM),BUF2(3*NUMATM)
     1               ,BUF3(3,NUMATM),BUF4(3,NUMATM),BUF0(3)
     2               ,DHB(3*NUMATM)
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)
     2               ,DATLGR(3,NUMATM)
     3               ,LGR,FLAGRD
                      LOGICAL LGR,FLAGRD
      DATA ZERO /0D0/,ONE /1D0/,TWO /2D0/
      NUMAT3=3*NUMAT
      CALL VFILL (100,ZERO,HBONDO,1)
      CBONDO=ZERO
      CBOND5=ZERO
      COBND5=ZERO
      CNBNDO=ZERO
      OOBNDO=ZERO
      ONBNDO=ZERO
      SSBNDO=ZERO
      CLCBND=ZERO
      BRCBND=ZERO
      FCBNDO=ZERO
      OPBND5=ZERO
      SPBNDO=ZERO
      HOHBND=ZERO
      HNNBND=ZERO
      CNBOC2=ZERO
      TNCBC3=ZERO
      TNCBC2=ZERO
      OSIBND=ZERO
      IF (LGR) CALL VFILL (NUMAT3,ZERO,DCDS,1)
C
C     OUTER BIG LOOP OVER ATOMS, ENDS ON HB(IAT) AND DCDS(3,NUMAT).
C
      DO 1000 IAT=1,NUMAT
      NATIAT=NAT(IAT)
      ATARI=ATAR(IAT)
      IF (ATARI.LE.ZERO) THEN
         HB(IAT)=ZERO
         GOTO 1000
      ENDIF
C
CDL   NOTE: THE LEFT DIGIT(S) OF THE STATEMENT'S LABELS GIVE(S)
CDL         THE ATOMIC NUMBER OF THE ELEMENT CONSIDERED.
CDL         THE LABELS APPEAR IN ASCENDING ORDER.
CDL   COULD HELP THE CODE A BIT MORE READABLE...
C
      AA=ZERO
      IF (LGR) CALL VFILL (NUMAT3,ZERO,DHB,1)
CCC   sigma Hk term, based on a Tkk.
CCC   NOTE: Hydrogen has this extra surface tension when bonded to
CCC   Carbon, Oxygen, Nitrogen, Sulfur
      IF (NATIAT.EQ.1) THEN
         ITPC=NATCNV(1)
         DELTAR=DRVAL(1)
         DO 19 J=1,NUMAT
         NTP=NAT(J)
         IF (NTP.EQ.6.OR.NTP.EQ.7.OR.NTP.EQ.8.OR.NTP.EQ.16) THEN
            NTPC=NATCNV(NTP)
            CUTOF1=RKKVAL(ITPC,NTPC)+DELTAR
            IF (R(J,IAT).LT.CUTOF1) THEN
               IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF,1)
               CALL TKK (IAT,J,CUTOF1,DELTAR,ONE,F0,LGR,BUF)
               AA=AA+F0*HRFN4(NTP)
               IF (LGR) CALL SAXPY (NUMAT3,HRFN4(NTP),BUF,1,DHB,1)
               HBONDO(NTP)=HBONDO(NTP)+F0*ATARI
               IF ((NTP.EQ.7.OR.NTP.EQ.8).AND.F0.GT.ZERO) THEN
                  F1=ZERO
                  IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF1,1)
                  IF (NTP.EQ.7.AND.HNNST1.NE.ZERO) THEN
                     CUTOF=RKKVAL(NTPC,NATCNV(7))+DELTAR
                     DO 10 K=1,NUMAT
                     IF (J.NE.K.AND.NAT(K).EQ.7) THEN
                        IF (R(K,J).LT.CUTOF) THEN
                           CALL TKK (J,K,CUTOF,DELTAR,HNNST1,F,LGR,BUF1)
                           HNNBND=HNNBNB+F0*F*ATARI/HNNST1
                           F1=F1+F
                        ENDIF
                     ENDIF
   10                CONTINUE
                  ELSE IF (NTP.EQ.8.AND.HOHST1.NE.ZERO) THEN
                     CUTOF=RKKVAL(NTPC,NATCNV(1))+DELTAR
                     DO 11 K=1,NUMAT
                     IF (IAT.NE.K.AND.NAT(K).EQ.1) THEN
                        IF (R(K,J).LT.CUTOF) THEN
                           CALL TKK (J,K,CUTOF,DELTAR,HOHST1,F,LGR,BUF1)
                           HOHBND=HOHBND+F0*F*ATARI/HOHST1
                           F1=F1+F
                        ENDIF
                     ENDIF
   11                CONTINUE
                  ENDIF
                  IF (F1.NE.ZERO) THEN
                     AA=AA+F0*F1
                     IF (LGR) THEN
                        CALL SAXPY (NUMAT3,F1,BUF,1,DHB,1)
                        CALL SAXPY (NUMAT3,F0,BUF1,1,DHB,1)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
   19    CONTINUE
      ELSE IF (NATIAT.EQ.6) THEN
         ITPC=NATCNV(6)
CCC      SM5 CC CDS terms. They are of the Tkk form.
         DO 62 KCCC=1,2
         IF (KCCC.EQ.1.AND.CRFNH4.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF1=RKKVAL(ITPC,NATCNV(6))+DELTAR
            DO 60 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 60
            IF (NAT(J).EQ.6) THEN
               IF (R(J,IAT).LT.CUTOF1) THEN
                  CALL TKK (IAT,J,CUTOF1,DELTAR,CRFNH4,F,LGR,DHB)
                  AA=AA+F
                  CBONDO=CBONDO+F*ATARI/CRFNH4
               ENDIF
            ENDIF
   60       CONTINUE
         ELSE IF (KCCC.EQ.2.AND.CRFNH5.NE.ZERO) THEN
            DELTAR=DRVAL(3)
            CUTOF3=RKKVL2(ITPC,NATCNV(6))+DELTAR
            DO 61 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 61
            IF (NAT(J).EQ.6) THEN
               IF (R(J,IAT).LT.CUTOF3) THEN
                  CALL TKK (IAT,J,CUTOF3,DELTAR,CRFNH5,F,LGR,DHB)
                  AA=AA+F
                  CBOND5=CBOND5+F*ATARI/CRFNH5
               ENDIF
            ENDIF
   61       CONTINUE
         ENDIF
   62    CONTINUE
CCC      SM5 C-N CDS terms. They are of the (Tkk)**2 form.
         IF (CNSTC2.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF1=RKKVAL(ITPC,NATCNV(7))+DELTAR
            RTKK=ZERO
            IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF,1)
            DO 63 J=1,NUMAT
            IF (NAT(J).EQ.7) THEN
               IF (R(J,IAT).LT.CUTOF1) THEN
                  CALL TKK (IAT,J,CUTOF1,DELTAR,ONE,F,LGR,BUF)
                  RTKK=RTKK+F
               ENDIF
            ENDIF
   63       CONTINUE
            AA=AA+CNSTC2*RTKK**2
            IF (LGR) CALL SAXPY (NUMAT3,CNSTC2*RTKK*TWO,BUF,1,DHB,1)
            CNBOC2=CNBOC2+RTKK**2*ATARI/CNSTC2
         ENDIF
      ELSE IF (NATIAT.EQ.7) THEN
         ITPC=NATCNV(7)
CCC      Add the Nitrogen bonded to Carbon Surface Tension terms.
CCC      The terms are a sum over all N bonded to C of a COT times the
CCC      sum of all other atoms attached to the C attached to the N
CCC      raised to a power. See the paper for more explicit details.
         RTKKS1=ZERO
         RTKKS2=ZERO
         IF (LGR) THEN
            CALL VFILL (NUMAT3,ZERO,BUF1,1)
            CALL VFILL (NUMAT3,ZERO,BUF2,1)
         ENDIF
         DELTAR=DRVAL(1)
         NTPC=NATCNV(6)
         CUTOF1=RKKVAL(ITPC,NTPC)+DELTAR
         DO 71 J=1,NUMAT
         IF (NAT(J).EQ.6) THEN
            IF (R(J,IAT).LT.CUTOF1) THEN
               IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF,1)
               CALL TKK (IAT,J,CUTOF1,DELTAR,ONE,RTKK,LGR,BUF)
               RTKK3=ZERO
               RTKK4=ZERO
               IF (LGR) THEN
                  CALL VFILL (NUMAT3,ZERO,BUF3,1)
                  CALL VFILL (NUMAT3,ZERO,BUF4,1)
               ENDIF
               DO 70 K=1,NUMAT
               IF (K.NE.IAT.AND.K.NE.J) THEN
                  CUTOF2=RKKVAL(NATCNV(NAT(K)),NTPC)+DELTAR
                  IF (R(K,J).LT.CUTOF2) THEN
                     CALL TKK (J,K,CUTOF2,DELTAR,ONE,RTKK2,.FALSE.,BUF0)
                     RTKK3=RTKK3+RTKK2
                     IF (LGR) THEN
                        DFDR=-RTKK2*DELTAR/(R(K,J)-CUTOF2)**2
                        BUF0(1)=UN(1,K,J)*DFDR
                        BUF0(2)=UN(2,K,J)*DFDR
                        BUF0(3)=UN(3,K,J)*DFDR
                        BUF3(1,J)=BUF3(1,J)+BUF0(1)
                        BUF3(2,J)=BUF3(2,J)+BUF0(2)
                        BUF3(3,J)=BUF3(3,J)+BUF0(3)
                        BUF3(1,K)=BUF3(1,K)-BUF0(1)
                        BUF3(2,K)=BUF3(2,K)-BUF0(2)
                        BUF3(3,K)=BUF3(3,K)-BUF0(3)
                     ENDIF
                     IF (NAT(K).EQ.8) THEN
                        RTKK4=RTKK4+RTKK2
                        IF (LGR) THEN
                           BUF4(1,J)=BUF4(1,J)+BUF0(1)
                           BUF4(2,J)=BUF4(2,J)+BUF0(2)
                           BUF4(3,J)=BUF4(3,J)+BUF0(3)
                           BUF4(1,K)=BUF4(1,K)-BUF0(1)
                           BUF4(2,K)=BUF4(2,K)-BUF0(2)
                           BUF4(3,K)=BUF4(3,K)-BUF0(3)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
   70          CONTINUE
               RTKKS1=RTKKS1+RTKK*RTKK3**2
               RTKKS2=RTKKS2+RTKK*RTKK4
               IF (LGR) THEN
                  CALL SAXPY (NUMAT3,TWO*RTKK3*RTKK,BUF3,1,BUF1,1)
                  CALL SAXPY (NUMAT3,RTKK3**2,BUF,1,BUF1,1)
                  CALL SAXPY (NUMAT3,RTKK,BUF4,1,BUF2,1)
                  CALL SAXPY (NUMAT3,RTKK4,BUF,1,BUF2,1)
               ENDIF
            ENDIF
         ENDIF
   71    CONTINUE
         DHOLDER=RTKKS1**1.3D0
         DANDY=1.3D0*RTKKS1**0.3D0
         AA=AA+DHOLDER*CNRFN4+RTKKS2*TNCSC2
         IF (LGR) THEN
            CALL SAXPY (NUMAT3,TNCSC2,BUF2,1,DHB,1)
            CALL SAXPY (NUMAT3,CNRFN4*DANDY,BUF1,1,DHB,1)
         ENDIF
         CNBNDO=CNBNDO+DHOLDER*ATARI
         TNCBC2=TNCBC2+RTKKS2*ATARI
C        CDS terms which separates N triple bonded C
         IF (TNCSC3.NE.ZERO) THEN
            DELTAR=DRVAL(4)
            CUTOF=RKKVL2(ITPC,NATCNV(6))+DELTAR
            DO 72 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 72
            IF (NAT(J).EQ.6) THEN
               IF (R(J,IAT).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,TNCSC3,F,LGR,DHB)
                  AA=AA+F
                  TNCBC3=TNCBC3+F*ATARI/TNCSC3
               ENDIF
            ENDIF
   72       CONTINUE
         ENDIF
      ELSE IF (NATIAT.EQ.8) THEN
         ITPC=NATCNV(8)
CCC      O-C CDS terms. They are of the Tkk form.
         IF (CORFN5.NE.ZERO) THEN
            DELTAR=DRVAL(2)
            CUTOF=RKKVL2(ITPC,NATCNV(6))+DELTAR
            DO 80 J=1,NUMAT
            IF (NAT(J).EQ.6) THEN
               IF (R(J,IAT).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,CORFN5,F,LGR,DHB)
                  AA=AA+F
                  COBND5=COBND5+F*ATARI/CORFN5
               ENDIF
            ENDIF
   80       CONTINUE
         ENDIF
CCC      O-N CDS terms. They are of the Tkk form.
         IF (ONRFN4.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF=RKKVAL(ITPC,NATCNV(7))+DELTAR
            DO 81 J=1,NUMAT
            IF (NAT(J).EQ.7) THEN
               IF (R(J,IAT).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,ONRFN4,F,LGR,DHB)
                  AA=AA+F
                  ONBNDO=ONBNDO+F*ATARI/ONRFN4
               ENDIF
            ENDIF
   81       CONTINUE
         ENDIF
CCC      Add the O-O CDS term.
         IF (OORFN5.NE.ZERO) THEN
            RTKK=ZERO
            KCCAN=0
            IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF,1)
            DELTAR=DRVAL(1)
            CUTOF1=RKKVAL(ITPC,NATCNV(8))+DELTAR
            DO 82 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 82
            IF (NAT(J).EQ.8) THEN
               IF (R(J,IAT).LT.CUTOF1) THEN
                  CALL TKK (IAT,J,CUTOF1,DELTAR,ONE,F,LGR,BUF)
                  RTKK=RTKK+F
                  KCCAN=KCCAN+1
               ENDIF
            ENDIF
   82       CONTINUE
            IF (KCCAN.GE.1) THEN
CDL            SRTNO=-1.0D0*RTKK
CDL            REYYO=-0.4D0
CDL            DELTAO=0.4D0
CDL            BYYO=1.0D0
CDL            CUTOF1=REYYO+DELTAO
CDL            IF (SRTNO.LT.CUTOF1) THEN
CDL               XRTKO=EXP(1-(BYYO/(1-((SRTNO-REYYO)/DELTAO))))*EXP(-1)
CDL               HB=HB+XRTKO*OORFN5
CDL            ENDIF
CDL            SUBSUMES TO, AND ADDING GRADIENTS:
               X=RTKK/0.4D0
C --- Liotard had this for XRTKO, But I think it's wrong                !JT0303
C              XRTKO=OORFN5*EXP(X) 
C --- I think this is what we want for XRTKO instead                    !JT0303
               XRTKO=OORFN5*(1.0D0/(EXP((1.0D0/X))))                    !JT0303
               AA=AA+XRTKO
               IF (LGR) THEN
                  DXDS=XRTKO/0.4D0
                  CALL SAXPY (NUMAT3,DXDS,BUF,1,DHB,1)
               ENDIF
               OOBNDO=OOBNDO+XRTKO*ATARI/OORFN5
            ENDIF
         ENDIF
CCC      O-Si CDS terms. They are of the Tkk form.
         IF (OPRFN5.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF=RKKVAL(ITPC,NATCNV(14))+DELTAR
            DO 83 J=1,NUMAT
            IF (NAT(J).EQ.14) THEN
               IF (R(J,IAT).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,OSIBD5,F,LGR,DHB)
                  AA=AA+F
                  IF (ABS(OSIBD5) .GE. 1.0D-40) THEN                    BJL1003
                      OSIBND=OSIBND+F*ATARI/OSIBD5                      
                  ENDIF                                                 BJL1003
               ENDIF
            ENDIF
   83       CONTINUE
         ENDIF
CCC      O-P CDS terms. They are of the Tkk form.
         IF (OPRFN5.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF=RKKVAL(ITPC,NATCNV(15))+DELTAR
            DO 84 J=1,NUMAT
            IF (NAT(J).EQ.15) THEN
               IF (R(J,IAT).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,OPRFN5,F,LGR,DHB)
                  AA=AA+F
                  OPBND5=OPBND5+F*ATARI/OPRFN5
               ENDIF
            ENDIF
   84       CONTINUE
         ENDIF
      ELSE IF (NATIAT.EQ.16) THEN
         ITPC=NATCNV(16)
CCC      S-P CDS terms. They are of the Tkk form.
         IF (SPRFN5.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF=RKKVAL(ITPC,NATCNV(15))+DELTAR
            DO 160 J=1,NUMAT
            IF (NAT(J).EQ.15) THEN
               IF (R(J,IAT).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,SPRFN5,F,LGR,DHB)
                  AA=AA+F
                  SPBNDO=SPBNDO+F*ATARI/SPRFN5
               ENDIF
            ENDIF
  160       CONTINUE
         ENDIF
CCC      S-S CDS terms. They are of the Tkk form.
         IF (SSRFN5.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            CUTOF=RKKVAL(ITPC,NATCNV(16))+DELTAR
            DO 161 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 161
            IF (NAT(J).EQ.16) THEN
               IF (R(IAT,J).LT.CUTOF) THEN
                  CALL TKK (IAT,J,CUTOF,DELTAR,SSRFN5,F,LGR,DHB)
                  AA=AA+F
                  SSBNDO=SSBNDO+F*ATARI/SSRFN5
               ENDIF
            ENDIF
  161       CONTINUE
         ENDIF
      ELSE IF (NATIAT.EQ.17) THEN
         ITPC=NATCNV(17)
CCC      Cl-C CDS terms. They are of the Tkk form.
         IF (CLCRFN.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            NTPC=NATCNV(6)
            CUTOF1=RKKVAL(ITPC,NTPC)+DELTAR
            DO 171 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 171
            IF (NAT(J).EQ.6) THEN
               IF (R(J,IAT).LT.CUTOF1) THEN
                  IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF,1)
                  CALL TKK (IAT,J,CUTOF1,DELTAR,CLCRFN,F0,LGR,BUF)
                  F1=ZERO
                  IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF1,1)
                  DO 170 K=1,NUMAT
                  IF (K.NE.IAT.AND.K.NE.J) THEN
                     CUTOF2=RKKVAL(NATCNV(NAT(K)),NTPC)+DELTAR
                     IF (R(K,J).LT.CUTOF2) THEN
                        ENEG=ONE/ENEGIT(NAT(K))
                        CALL TKK (J,K,CUTOF2,DELTAR,ENEG,F,LGR,BUF1)
                        F1=F1+F
                     ENDIF
                  ENDIF
  170             CONTINUE
                  AA=AA+F0*F1
                  IF (LGR) THEN
                     CALL SAXPY (NUMAT3,F1,BUF ,1,DHB,1)
                     CALL SAXPY (NUMAT3,F2,BUF1,1,DHB,1)
                  ENDIF
                  CLCBND=CLCBND+F0*F1*ATARI/CLCRFN
               ENDIF
            ENDIF
  171       CONTINUE
         ENDIF
      ELSE IF (NATIAT.EQ.35) THEN
         ITPC=NATCNV(35)
CCC      Br-C CDS terms. They are of the Tkk form.
         IF (BRCRFN.NE.ZERO) THEN
            DELTAR=DRVAL(1)
            NTPC=NATCNV(6)
            CUTOF1=RKKVAL(ITPC,NTPC)+DELTAR
            DO 351 J=1,NUMAT
            IF (J.EQ.IAT) GOTO 351
            IF (NAT(J).EQ.6) THEN
               IF (R(J,IAT).LT.CUTOF1) THEN
                  IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF,1)
                  CALL TKK (IAT,J,CUTOF1,DELTAR,BRCRFN,F0,LGR,BUF)
                  F1=ZERO
                  IF (LGR) CALL VFILL (NUMAT3,ZERO,BUF1,1)
                  DO 350 K=1,NUMAT
                  IF (K.NE.IAT.AND.K.NE.J) THEN
                     CUTOF2=RKKVAL(NATCNV(NAT(K)),NTPC)+DELTAR
                     IF (R(K,J).LT.CUTOF2) THEN
                        ENEG=ONE/ENEGIT(NAT(K))
                        CALL TKK (J,K,CUTOF2,DELTAR,ENEG,F,LGR,BUF1)
                        F1=F1+F
                     ENDIF
                  ENDIF
  350             CONTINUE
                  AA=AA+F0*F1
                  IF (LGR) THEN
                     CALL SAXPY (NUMAT3,F1,BUF ,1,DHB,1)
                     CALL SAXPY (NUMAT3,F2,BUF1,1,DHB,1)
                  ENDIF
                  BRCBND=BRCBND+RTKK4*ATARI/BRCRFN
               ENDIF
            ENDIF
  351       CONTINUE
         ENDIF
      ENDIF
CCC
CCC   HB(IAT) AND GRADIENTS IN FINAL FORM.
CCC
      ARED3=ATARI*1D-3
      HB(IAT)=AA*ARED3
      IF (LGR) THEN
         CALL SAXPY (NUMAT3,ARED3,DHB,1,DCDS,1)
         FACTR1=AA*1D-3
         CALL SAXPY (NUMAT3,FACTR1,DATAR(1,1,IAT),1,DCDS,1)
      ENDIF
 1000 CONTINUE
      END
C======================================================================+
      SUBROUTINE TKK (I,J,RCUT,DELTAR,FACTOR,F,LGR,DXYZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      DIMENSION DXYZ(3,*)
      LOGICAL LGR
C ---------------------------------------------------------------------+
C     VALUE AND CARTESIAN GRADIENTS OF A FUNCTION OF THE Tkk TYPE:
C     F(R(I,J))=EXP(1-(1/(1-((R(I,J)-RHLD)/DELTAR))))/EXP(1)*FACTOR
C                                                             DL, MAR 2K
C  INPUT
C     I,J:                  LABELS OF THE PAIR OF ATOMS.
C     RCUT, DELTAR, FACTOR: PARAMETERS OF THE FUNCTION.
C     LGR:                  .TRUE. IF GRADIENTS REQUIRED.
C     R(NUMAT,NUMAT):       INTERATOMIC DISTANCES.
C     UN(3,NUMAT,NUMAT):    ASSOCIATED UNIT VECTORS.
C  OUTPUT
C     F:                    VALUE OF THE FUNCTION.
C     DXYZ(3,NUMAT):        VALUES ON INPUT + dF/dCOORD(i,j)
C                           (PERFORMED ON REQUEST "LGR").
C ---------------------------------------------------------------------+
C     NOTE: A MATHEMATICALLY EQUIVALENT FORMULATION OF THE Tkk IS:
C           F(R(I,J)) = EXP(DELTAR/(R(I,J)-RCUT))*FACTOR
C           WITH RCUT = DELTAR+RHLD
C     THE DERIVATIVE OF F WITH RESPECT TO R(I,J) READS:
C           dF/dR = -F*DELTAR/(R(I,J)-RCUT)**2
C
      COMMON /VOLCOM/ R(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,UN(3,NUMATM,NUMATM)
      X1=1D0/(R(I,J)-RCUT)
      X=DELTAR*X1
      F=EXP(X)*FACTOR
      IF (LGR) THEN
         DFDR=-F*X*X1
         DXYZ(1,I)=DXYZ(1,I)+UN(1,J,I)*DFDR
         DXYZ(2,I)=DXYZ(2,I)+UN(2,J,I)*DFDR
         DXYZ(3,I)=DXYZ(3,I)+UN(3,J,I)*DFDR
         DXYZ(1,J)=DXYZ(1,J)-UN(1,J,I)*DFDR
         DXYZ(2,J)=DXYZ(2,J)-UN(2,J,I)*DFDR
         DXYZ(3,J)=DXYZ(3,J)-UN(3,J,I)*DFDR
      ENDIF
      END
C======================================================================+
      SUBROUTINE VFILL (N,C,V,NV)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(*)
C ---------------------------------------------------------------------+
C     FILL A VECTOR WITH A CONSTANT.                          DL, MAR 03
C  INPUT
C     N : NUMBER OF ELEMENTS.
C     C : CONSTANT.
C     NV: STRIDE FOR THE VECTOR. V(1), V(1+NV), V(1+2.NV),...
C  OUTPUT
C     V : VECTOR, FILLED BY "C".
C ---------------------------------------------------------------------+
      DO 10 I=1,(N-1)*NV+1,NV
      V(I)=C
   10 CONTINUE
      END
