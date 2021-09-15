



     This code has been copied by mistake. Please do NOT use it
     unless by invitation. If you have not been specifically requested
     to perform tests with it, please destroy this file



     SUBROUTINE FOCK1(F, PTOT, PA, PB)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES/NOLIST'
     DIMENSION F(*), PTOT(*), PA(*), PB(*)
 *********************************************************************

 *** COMPUTE THE REMAINING CONTRIBUTIONS TO THE ONE-CENTRE ELEMENTS.   
                                                                       
 *********************************************************************
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    +                NLAST(NUMATM), NORBS, NELECS,
    1                NALPHA, NBETA, NCLOSE, NOPEN
     COMMON /TWOELE/ GSS(54),GSP(54),GPP(54),GP2(54),HSP(54)
     COMMON /GAUSS / FN1(54),FN2(54)
     DIMENSION QTOT(NUMATM), QA(NUMATM), QB(NUMATM)
      CALL CHRGE(PTOT,QTOT)
      CALL CHRGE(PA,QA)
      DO 10 I=1,NUMAT
 10   QB(I)=QTOT(I)-QA(I)
      DO 100 II=1,NUMAT
        IA=NFIRST(II)                                                  
        IB=NLAST(II)                                                   
        NI=NAT(II)                                                     
     IF(NI.EQ.1)THEN
     SUM=0.D0
     ELSE
        SUM2=0.D0
        SUM1=0.D0
        DO 111 I=IA,IB
        IM1=I-1
        DO 112 J=IA,IM1
 112    SUM1=SUM1+PTOT(J+(I*(I-1))/2)**2
 111    SUM2=SUM2+PTOT((I*(I+1))/2)**2
        SUM=SUM1*2.D0+SUM2 
        SUM=(SUM-QTOT(II)**2*0.25D0)
#         WRITE(6,'('' ATOM'',I3,'' ANISOTROPY'',F12.6)')NI,SUM
        SUM=SUM*FN1(NI)
     ENDIF
#         WRITE(6,'('' ATOM'',I3,'' CORRECTION'',F12.5)')II,SUM
                                                                       
     F(S,S)                                                            
                                                                       
        KA=(IA*(IA+1))/2                                               
        F(KA)=F(KA)+PB(KA)*GSS(NI)+(QTOT(II)-PTOT(KA))*GSP(NI)
    +         -(QA(II)-PA(KA))*HSP(NI)

    MODIFICATION TO ACCOUNT FOR DIPOLAR BONDS!

    #     +SUM

   END OF MODIFICATION

        IF (NI.LT.3) GO TO 100                                         
        IPLUS=IA+1                                                     
        L=KA                                                           
        DO 80 J=IPLUS,IB                                               
           M=L+IA                                                      
           L=L+J                                                       
                                                                       
     F(P,P)                                                            
                                                                       
           F(L)=F(L)+PTOT(KA)*GSP(NI)-PA(KA)*HSP(NI)+ 
    1      PB(L)*GPP(NI)+(QTOT(II)-PTOT(L)-PTOT(KA))*GP2(NI)
    2      -0.5D0*(QA(II)-PA(L)-PA(KA))*(GPP(NI)-GP2(NI))

    MODIFICATION TO ACCOUNT FOR DIPOLAR BONDS!

    #     +SUM

   END OF MODIFICATION

                                                                       
     F(S,P)                                                            
                                                                       
  80    F(M)=F(M)+2.D0*PTOT(M)*HSP(NI)-PA(M)*(HSP(NI)+GSP(NI))
                                                                       
     F(P,P*)                                                           
                                                                       
        IMINUS=IB-1                                                    
        DO 90 J=IPLUS,IMINUS                                           
           IC=J+1                                                      
        DO 90 L=IC,IB                                                  
           M=(L*(L-1))/2+J                                             
 90     F(M)=F(M)+PTOT(M)*(GPP(NI)-GP2(NI))
    +      -0.5D0*PA  (M)*(GPP(NI)+GP2(NI))                 
 100 CONTINUE                                                          

     RETURN
     END
     PROGRAM MAIN

     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES/NOLIST'
     COMMON /KEYWRD/ KEYWRD 
     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), XPARAM(MAXPAR) 
     COMMON /GEOSYM/ NDEP,LOCPAR(200),IDEPFN(200),LOCDEP(200)
     COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
    +NA(NUMATM),NB(NUMATM),NC(NUMATM)
     COMMON /GMETRY/ GEO(3,NUMATM)
     COMMON /GRADNT/ GRAD(MAXPAR),GNORM
     COMMON /NUMCAL/ NUMCAL
     COMMON /TIME  / TIME0
     COMMON /PATH  / LATOM,LPARAM,REACT(100)
     CHARACTER*80 KEYWRD
     NUMCAL=1

     TIME0=SECOND()

 READ AND CHECK INPUT FILE, EXIT IF NECESSARY. 
     WRITE INPUT FILE TO UNIT 6 AS FEEDBACK TO USER

  5  CALL READ

 INITIALIZE CALCULATION AND WRITE CALCULATION INDEPENDENT INFO

     CALL MOLDAT

 CALCULATE


     IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
         CALL REACT1(FUNCT)
         CALL WRITE(TIME0,FUNCT)
         STOP
     ENDIF
     IF (LATOM .NE. 0) THEN

       DO PATH

        CALL PATHS
        STOP
     END IF
     IF (INDEX(KEYWRD,'FORCE') .NE. 0 ) THEN

 FORCE CALCULATION IF DESIRED

        CALL FORCE
        STOP
     ENDIF

     IF(INDEX(KEYWRD,'NLLSQ') .NE. 0) THEN
        CALL NLLSQ(XPARAM, NVAR )
        CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,GRAD,.TRUE.)
        CALL WRITE(TIME0,ESCF)
     STOP
     ENDIF

     IF (INDEX(KEYWRD,'1SCF') .NE. 0) THEN
           NVAR=0
     IF(INDEX(KEYWRD,'GRAD').NE.0) THEN
     NVAR=0
     DO 10 I=2,NATOMS
         IF(LABELS(I).EQ.99) GOTO 10
         IF(I.EQ.2)ILIM=1
         IF(I.EQ.3)ILIM=2
         IF(I.GT.3)ILIM=3
         DO 13 J=1,ILIM
             NVAR=NVAR+1
             LOC(1,NVAR)=I
             LOC(2,NVAR)=J3            XPARAM(NVAR)=GEO(J,I)0    CONTINUE
           ENDIF
     ENDIF
     IF(INDEX(KEYWRD,'SIGMA') .NE. 0) THEN
        CALL POWSQ(XPARAM, NVAR, ESCF)
        CALL WRITE(TIME0,ESCF)
        STOP
     ENDIF

 ORDINARY GEOMETRY OPTIMISATION

        CALL FLEPO(XPARAM, NVAR, ESCF)
        CALL WRITE(TIME0,ESCF)
     STOP
     END


     BLOCK DATA
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     COMMON /NATORB/ NATORB(54)
    +       /ALPHA / ALP(54)
    1       /CORE  / CORE(54)
    2       /MULTIP/ DD(54),QQ(54),AM(54),AD(54),AQ(54)
    3       /EXPONT/ ZS(54),ZP(54),ZD(54)
    4       /ONELEC/ USS(54),UPP(54),UDD(54)
    5       /BETAS / BETAS(54),BETAP(54),BETAD(54)
    6       /TWOELE/ GSS(54),GSP(54),GPP(54),GP2(54),HSP(54)
    7       /ATOMIC/ EISOL(54),EHEAT(54)
    8       /AM1REF/ AM1REF(54)
    9       /VSIPS / VS(54),VP(54),VD(54)
    A       /ISTOPE/ AMS(54)
    B       /IDEAS / GUESS1(54,10),GUESS2(54,10),GUESS3(54,10)
    C       /GAUSS / FN1(54),FN2(54)

  COMMON BLOCKS FOR MINDO/3

     COMMON /ONELE3 /  USS3(18),UPP3(18)
    +       /TWOEL3 /  F03(18)
    1       /ATOMI3 /  EISOL3(18),EHEAT3(18)
    2       /BETA3  /  BETA3(153)
    3       /ALPHA3 /  ALP3(153)
    4       /EXPON3 /  ZS3(18),ZP3(18)

  END OF MINDO/3 COMMON BLOCKS



   NATORB IS THE NUMBER OF ATOMIC ORBITALS PER ATOM.

     DATA NATORB/2*1,8*4,8*4,2*4,10*9,6*4,2*4,10*9,6*4/
     DATA AM1REF /54*1.D0/

   THE ATOMIC MASSES OF THE ISOTOPES
     DATA  AMS(1) / 1.007825D0/
     DATA  AMS(4) / 9.012190D0/
     DATA  AMS(5) / 11.00931D0/
     DATA  AMS(6) / 12.00000D0/
     DATA  AMS(7) / 14.00307D0/
     DATA  AMS(8) / 15.99491D0/
     DATA  AMS(9) / 18.99840D0/
     DATA  AMS(13)/ 26.98153D0/
     DATA  AMS(14)/ 27.97693D0/
     DATA  AMS(15)/ 30.97376D0/
     DATA  AMS(16)/ 31.97207D0/
     DATA  AMS(17)/ 34.96885D0 /
     DATA  AMS(35)/ 79.9D0 /
     DATA  AMS(53)/ 126.9D0 /
   ATOMIC WEIGHTS FOR LESS ABUNDANT ISOTOPES:
                  AMS(2) = DEUTERIUM
                  AMS(3) = CARBON 13
                  AMS(10)= CARBON 14
                  AMS(11)= OXYGEN 18
                  AMS(12)= BORON 10
     DATA  AMS(2) /2.0141022D0/
     DATA  AMS(3) /13.003354D0/
     DATA  AMS(10)/14.003242D0/
     DATA  AMS(11)/17.999160D0/
     DATA  AMS(12)/10.012940D0/

   CORE IS THE CHARGE ON THE ATOM AS SEEN BY THE ELECTRONS

     DATA CORE(1),CORE(4) /1.0D00,  2.D0/
     DATA CORE(5)/3.0D00/
     DATA CORE(6)/4.0D00/
     DATA CORE(7)/5.0D00/
     DATA CORE(8)/6.0D00/
     DATA CORE(9)/7.0D00/
     DATA CORE(13)/3.0D00/
     DATA CORE(14)/4.0D00/
     DATA CORE(15)/5.0D00/
     DATA CORE(16)/6.0D00/
     DATA CORE(17)/7.0D00/
     DATA CORE(35)/7.D0/
     DATA CORE(53)/7.D0/

     ENTHALPIES OF FORMATION OF GASEOUS ATOMS ARE TAKEN FROM \ANNUAL
     REPORTS,1974,71B,P 117\  THERE ARE SOME SIGNIFICANT DIFFERENCES
     BETWEEN THE VALUES REPORTED THERE AND THE VALUES PREVIOUSLY IN
     THE BLOCK DATA OF THIS PROGRAM.  ONLY THE THIRD  ROW ELEMENTS
     HAVE BEEN UPDATED.

     DATA EHEAT(1),EHEAT(4) /52.102D00 , 76.96D00 /
     DATA EHEAT(5)/135.7D00/
     DATA EHEAT(6) / 170.89D00 /
     DATA EHEAT(7)/113.0D00/
     DATA EHEAT(8)/59.559D00/
     DATA EHEAT(9)/18.89D00/
     DATA EHEAT(13)/79.49D00/
     DATA EHEAT(14)/108.39D00/
     DATA EHEAT(15)/75.57D00/
     DATA EHEAT(16)/66.40D00/
     DATA EHEAT(17)/28.99D00/
     DATA EHEAT(35)/26.74D0/
     DATA EHEAT(53)/25.517D0/
 *** VS AND VP ARE THE VALENCE STATE IONIZATION POTENTAIL OF S AND P
     ELECTRONS IN E.V. : USED IN THE EHT RESONANCE INTEGRALS.

     DATA VS(1) /  -13.605  /
     DATA VS(5)/-15.16D00/
     DATA VS(6)/-21.34D00/
     DATA VS(7)/-27.51D00/
     DATA VS(8)/-35.30D00/
     DATA VS(9)/-43.70D00/
     DATA VS(14)/-17.82D00/
     DATA VS(15)/-21.10D00/
     DATA VS(16)/-23.84D00/
     DATA VS(17)/-25.26D00/
     DATA VP(1)  /  0.0D00  /
     DATA VP(5)/-8.52D00/
     DATA VP(6)/-11.54D00/
     DATA VP(7)/-14.34D00/
     DATA VP(8)/-17.91D00/
     DATA VP(9)/-20.89D00/
     DATA VP(14)/-8.51D00/
     DATA VP(15)/-10.29D00/
     DATA VP(16)/-12.41D00/
     DATA VP(17)/-15.09D00/
      DATA NPQ/1,1, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,
     +4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/

 *** ONE CENTER REPULSION INTEGRALS
     GSS ::= (SS,SS)
     GPP ::= (PP,PP)
     GSP ::= (SS,PP)
     GP2 ::= (PP,P*P*)
     HSP ::= (SP,SP)
     DATA GSS(1) / 12.848D00 /
     DATA GSS(4)/9.00D00/
     DATA GSS(5)/10.59D00/
     DATA GSS(6) / 12.23D00 /
     DATA GSS(7)/13.59D00/
     DATA GSS(8)/15.42D00/
     DATA GSS(9)/16.92D00/
     DATA GSS(13)/8.09D00/
     DATA GSS(14)/9.82D00/
     DATA GSS(15)/11.56D00/
     DATA GSS(16)/12.88D00/
     DATA GSS(17)/15.03D00/
     DATA GSS(35)/15.03643948D0/
     DATA GSS(53)/15.04044855D0/
     DATA GPP(4)/6.97D00/
     DATA GPP(5)/8.86D00/
     DATA GPP(6) / 11.08D00 /
     DATA GPP(7)/12.98D00/
     DATA GPP(8)/14.52D00/
     DATA GPP(9)/16.71D00/
     DATA GPP(13)/5.98D00/
     DATA GPP(14)/7.31D00/
     DATA GPP(15)/8.64D00/
     DATA GPP(16)/9.90D00/
     DATA GPP(17)/11.30D00/
     DATA GPP(35)/11.27632539D0/
     DATA GPP(53)/11.14778369D0/
     DATA GSP(4)/7.43D00/
     DATA GSP(5)/9.56D00/
     DATA GSP(6) / 11.47D00 /
     DATA GSP(7)/12.66D00/
     DATA GSP(8)/14.48D00/
     DATA GSP(9)/17.25D00/
     DATA GSP(13)/6.63D00/
     DATA GSP(14)/8.36D00/
     DATA GSP(15)/10.08D00/
     DATA GSP(16)/11.26D00/
     DATA GSP(17)/13.16D00/
     DATA GSP(35)/13.03468242D0/
     DATA GSP(53)/13.05655798D0/
     DATA GP2(4)/6.22D00/
     DATA GP2(5)/7.86D00/
     DATA GP2(6) / 9.84D00 /
     DATA GP2(7)/11.59D00/
     DATA GP2(8)/12.98D00/
     DATA GP2(9)/14.91D00/
     DATA GP2(13)/5.40D00/
     DATA GP2(14)/6.54D00/
     DATA GP2(15)/7.68D00/
     DATA GP2(16)/8.83D00/
     DATA GP2(17)/9.97D00/
     DATA GP2(35)/9.85442552D0/
     DATA GP2(53)/9.91409071D0/
     DATA HSP(4)/1.28D00/
     DATA HSP(5)/1.81D00/
     DATA HSP(6) / 2.43D00 /
     DATA HSP(7)/3.14D00/
     DATA HSP(8)/3.94D00/
     DATA HSP(9)/4.83D00/
     DATA HSP(13)/0.70D00/
     DATA HSP(14)/1.32D00/
     DATA HSP(15)/1.92D00/
     DATA HSP(16)/2.26D00/
     DATA HSP(17)/2.42D00/
     DATA HSP(35)/2.45586832D0/
     DATA HSP(53)/2.45638202D0/

     THE MONOCENTRIC INTEGRALS HSP AND GSP FOR ALUMINIUM ARE ONLY
     ESTIMATES. A VALUE OF G1 FOR AL IS NEEDED TO RESOLVE OLEARIS
     INTEGRALS.

     OPTIMIZED MNDO PARAMETERS FOR H, BE, B, C, N, O, F
                                                     CL
     ESTIMATED MNDO PARAMETERS FOR       AL,SI, P, S

     ELEMENTS H, C, N, O WERE PARAMETERIZED BY WALTER THIEL
     ELEMENTS B,SI,P,S   WERE      ..          MICHAEL MCKEE
     ELEMENTS BE,F,AL,CL WERE      ..          HENRY RZEPA

     DATA USS(1)/-11.906276D00/
     DATA USS(4)/-16.602378D00/
     DATA USS(5)/-34.547130D00/
     DATA USS(6)/-52.279745D00/
     DATA USS(7)/ -71.932122D00/
     DATA USS(8)/-99.644309D00/
     DATA USS(9)/-131.071548D00/
     DATA USS(13)/-23.807097D00/
     DATA USS(14)/-40.568292D00/
     DATA USS(15)/-56.143360D00/
     DATA USS(16)/-75.239152D00/
     DATA USS(17)/-100.227166D00/
     DATA USS(35)/ -99.98644054D0/
     DATA USS(53)/-100.00305378D0/
     DATA UPP(4)/-10.703771D00/
     DATA UPP(5)/-23.121690D00/
     DATA UPP(6)/-39.205558D00/
     DATA UPP(7)/-57.172319D00/
     DATA UPP(8)/-77.797472D00/
     DATA UPP(9)/-105.782137D00/
     DATA UPP(13)/-17.519878D00/
     DATA UPP(14)/-28.089187D00/
     DATA UPP(15)/-42.851080D00/
     DATA UPP(16)/-57.832013D00/
     DATA UPP(17)/-77.378667D00/
     DATA UPP(35)/-75.67130754D0/
     DATA UPP(53)/-74.61146919D0/

     OPTIMIZED ORBITAL EXPONENTS (DEFAULT = CLEMENTI + 0.3)

     DATA ZS(1),ZP(1)/1.331967D00 , 0.0D00/
     DATA ZS(4),  ZP(4)   /           1.004210D0,  1.004210D0  /
     DATA ZS(5),  ZP(5)   /           1.506801D0,  1.506801D0  /
     DATA ZS(6),ZP(6) / 1.787537D00 , 1.787537D00 /
     DATA ZS(7),  ZP(7)   /           2.255614D0,  2.255614D0  /
     DATA ZS(8),  ZP(8)   /           2.699905D0,  2.699905D0  /
     DATA ZS(9),  ZP(9)   /           2.848487D0,  2.848487D0  /
     DATA ZS(13), ZP(13)  /           1.444161D0,  1.444161D0  /
     DATA ZS(14), ZP(14)  /           1.435306D0,  1.435306D0  /
     DATA ZS(15), ZP(15)  /           2.108720D0,  1.785810D0  /
     DATA ZS(16), ZP(16)  /           2.613591D0,  2.034393D0  /
     DATA ZS(17), ZP(17)  /           3.784645D0,  2.036263D00 /
     DATA ZS(35), ZP(35)  /           3.85430190D0,2.19920914D0/
     DATA ZS(53), ZP(53)  /           2.2729610D0, 2.16949803D0/
     DATA ZD/10*0.D0,1.D0,1.D0,1.D0,1.D0,1.D0,1.D0,1.D0,1.D0,36*1.D0/
     TWO CENTRE RESONANCE INTEGRALS AND CORE-CORE REPULSION FUNCTIONS

     DATA BETAS(1)/    -6.989064D0 /
     DATA BETAS(4)/    -4.017096D0 /
     DATA BETAS(5)/    -8.252054D0 /
     DATA BETAS(6)/   -18.985044D0 /
     DATA BETAS(7)/   -20.495758D0 /
     DATA BETAS(8)/   -32.688082D0 /
     DATA BETAS(9)/   -48.290466D0 /
     DATA BETAS(13)/   -2.450284D0 /
     DATA BETAS(14)/   -4.256218D0 /
     DATA BETAS(15)/   -6.791600D0 /
     DATA BETAS(16)/  -11.142231D0 /   
     DATA BETAS(17)/  -14.262320D0 /
     DATA BETAS(35)/   -8.917107D0 /
     DATA BETAS(53)/   -7.414451D0 /
     DATA BETAP(4)/    -4.017096D0 /
     DATA BETAP(5)/    -8.252054D0 /
     DATA BETAP(6)/    -7.934122D0 /
     DATA BETAP(7)/   -20.495758D0 /
     DATA BETAP(8)/   -32.688082D0 /
     DATA BETAP(9)/   -36.508540D0 /
     DATA BETAP(13)/   -2.670284D0 /
     DATA BETAP(14)/   -4.256218D0 /
     DATA BETAP(15)/   -6.791600D0 /
     DATA BETAP(16)/  -11.142231D0 /
     DATA BETAP(17)/  -14.623200D0 /
     DATA BETAP(35)/   -9.943740D0 /
     DATA BETAP(53)/   -6.196781D0 /
     DATA ALP(1) / 2.5441341D00/
     DATA ALP(4)/          1.669434D0/
     DATA ALP(5)/           2.134993D0/
     DATA ALP(6) / 2.546380D00 /
     DATA ALP(7)/           2.861342D0/
     DATA ALP(8)/           3.160604D0/
     DATA ALP(9)/           3.4196606D0/
     DATA ALP(13)/          1.8688394D0/
     DATA ALP(14)/           2.1961078D0/
     DATA ALP(15)/           2.4152800D0/
     DATA ALP(16)/           2.4916445D0/
     DATA ALP(17)/           2.542201D0/
     DATA ALP(35)/           2.44570512D0/
     DATA ALP(53)/           2.2073200D0/

     ELECTRONIC ENERGIES OF NEUTRAL FREE ATOMS

     DATA EISOL(1) / -11.906276D00 /
     DATA EISOL(4)/          -24.204740D0/
     DATA EISOL(5)/           -64.315950D0/
     DATA EISOL(6) / -120.500606D00 /
     DATA EISOL(7)/           -202.581201D0/
     DATA EISOL(8)/           -317.868506D0/
     DATA EISOL(9)/           -476.683781D0/
     DATA EISOL(13)/          -44.4840711D0/
     DATA EISOL(14)/           -90.53496D0/
     DATA EISOL(15)/           -152.959960D0/
     DATA EISOL(16)/           -235.45636D0/
     DATA EISOL(17)/           -353.137667D0/
     DATA EISOL(35)/           -346.681250D0/
     DATA EISOL(53)/           -340.59836D00/

     DIPOLE AND QUADRUPOLE CHARGE SEPARATIONS.

     DATA DD(4), QQ(4) /              1.43732454D0, 1.21961031D0 /
     DATA DD(5), QQ(5) /              0.95790730D0, 0.81281129D0 /
     DATA DD(6),QQ(6) / .80746618D00 , .68515777D00 /
     DATA DD(7), QQ(7) /              0.63990367D0, 0.54297627D0 /
     DATA DD(8), QQ(8) /              0.53460239D0, 0.45362517D0 /
     DATA DD(9), QQ(9) /              0.50671661D0, 0.42996330D0 /
     DATA DD(13),QQ(13)/              1.39923869D0, 1.15867971D0 /
     DATA DD(14),QQ(14)/              1.40787117D0, 1.16582809D0 /
     DATA DD(15),QQ(15)/              1.01296990D0, 0.93700901D0 /
     DATA DD(16),QQ(16)/              0.82315963D0, 0.82251564D0 /
     DATA DD(17),QQ(17)/              0.49868702D0, 0.82176028D0 /
     DATA DD(35),QQ(35)/              0.6051074D0,  0.9645873D0  /
     DATA DD(53),QQ(53)/              1.4253233D0,  1.1841707D0  /

     ADDITIVE TERMS( STORED AS A LINEAR ARRAY FOR COMPATIBILITY)

     DATA AM(1),AD(1),AQ(1)/ 0.47217935D0,0.47217935D0,0.00000000D0/
     DATA AM(4),AD(4),AQ(4)/ 0.33076075D0,0.33561420D0,0.38629372D0/
     DATA AM(5),AD(5),AQ(5)/ 0.38919515D0,0.49047299D0,0.55569787D0/
     DATA AM(6),AD(6),AQ(6)/ 0.44946711D0,0.61494736D0,0.66858974D0/
     DATA AM(7),AD(7),AQ(7)/0.49944873D0,0.78436428D0,0.81447199D0/
     DATA AM(8),AD(8),AQ(8)/ 0.56670342D0,0.95925620D0,0.94959338D0/
     DATA AM(9),AD(9),AQ(9)/ 0.62183021D0,1.08503007D0,1.03436433D0/
     DATA AM(13),AD(13),AQ(13)/0.29731716D0,0.26355743D0,0.36735599D0/
     DATA AM(14),AD(14),AQ(14)/0.36089673D0,0.34418174D0,0.39826149D0/
     DATA AM(15),AD(15),AQ(15)/0.42484381D0,0.48824197D0,0.49794058D0/
     DATA AM(16),AD(16),AQ(16)/0.47335538D0,0.58893950D0,0.56496234D0/
     DATA AM(17),AD(17),AQ(17)/0.55237045D0,0.80612202D0,0.60686609D0/
     DATA AM(35),AD(35),AQ(35)/0.5526068D0, 0.7258330D0, 0.5574589D0 /
     DATA AM(53),AD(53),AQ(53)/0.5527541D0, 0.4593451D0, 0.4585376D0 /


   ALL THE FOLLOWING DATA APPLY TO MINDO/3 AND NOT TO MNDO




 *** F03 IS THE ONE CENTER AVERAGED REPULSION INTEGRAL FOR USE IN THE
        TWO CENTER ELECTRONIC REPULSION INTEGRAL EVALUATION.

     DATA F03              /  12.848D0, 0.0D0, 0.0D0, 0.0D0,
    .  8.958D0, 10.833D0, 12.377D0, 13.985D0, 16.250D0,
    .         0.000D0, 0.000D0, 0.000D0, 0.000D0,
    .    7.57D0 ,  9.00D0 , 10.20D0 , 11.73,.0D0/

 *** USS AND UPP ARE THE ONE-CENTER CORE ELECTRON ATTRACTION AND KINETI
     ENERGY INTEGRALS FOR S AND P ELECTRONS RESPECTIVELY IN E.V.

     DATA USS3             / -12.505D0, 0.000D0, 0.000D0, 0.000D0,
    .                       -33.61D0, -51.79D0, -66.06D0, -91.73D0 ,
    .                       -129.86D0,
    .                        0.0000D0 , 0.000 D0 ,0.000D0 , 0.000D0 ,
    .          -39.82D0 , -56.23D0 , -73.39D0 , -98.99D0 ,.0D0/
     DATA UPP3             /   0.0D0, 0.0D0, 0.0D0, 0.0D0,
    .     -25.11D0 , -39.18D0 , -56.40D0 , -78.80D0 , -105.93D0 ,
    .                        0.000D0 , 0.000D0 , 0.000D0 , 0.000D0 ,
    .         -29.15D0 , -42.31D0 , -57.25D0 , -76.43D0 ,.0D0/


 *** EISOL3 AND EHEAT3 ARE THE GS ELECTRONIC ENERGY OF THE NEUTRAL ATOM
     (IN E.V.) AND THE HEAT OF FORMATION IF THE FREE ATOM (IN KCAL/MOL)

     DATA EISOL3             /-12.505D0 , 0.0D0 , 0.0D0 ,0.0D0 ,
    .        -61.70D0 ,-119.47D0 , -187.51D0 , -307.07D0 , -475.00D0 ,
    .                         0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
    .          -90.98D0 , -150.81D0 , -229.15D0 , -345.93D0 , 0.0D0/
     DATA EHEAT3             / 52.102D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
    .     135.7 D0 , 170.89D0 ,  113.0 D0 ,  59.559D0 ,  18.86D0 ,
    .                         0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
    .     106.0D0 ,   79.8D0 ,  65.65D0 ,  28.95D0 , 0.0D0 /

 *** BETA3 AND ALP3 ARE THE BOND PARAMETERS USED IN THE
     RESONANCE INTEGRAL AND THE CORE CORE REPULSION INTEGRAL RESPECTIVE
     THAT IS ACCORDING TO THE FOLLOWING CONVENTION

     HERE IS THE
     BOND TYPE DESIGNATION


         H   B   C   N   O   F  SI   P   S  CL
       -----------------------------------------
      H  1  11  16  22  29  37  92 106 121 137
      B     15  20  26  33  41  
      C         21  27  34  42  97 111 126 142
      N             28  35  43         127 143
      O                 36  44         128 144
      F                     45         129
     SI                        105
      P                            120     151
      S                                136 152
     CL                                    153

     DATA BETA3(1),ALP3(1)   /  0.244770D0 ,  1.489450D0 /
     DATA BETA3(11),ALP3(11)   /  0.185347D0 ,  2.090352D0 /
     DATA BETA3(15),ALP3(15)   /  0.151324D0 ,  2.280544D0 /
     DATA BETA3(16),ALP3(16)   /  0.315011D0 ,  1.475836D0 /
     DATA BETA3(20),ALP3(20)   /  0.250031D0 ,  2.138291D0 /
     DATA BETA3(21),ALP3(21)   /  0.419907D0 ,  1.371208D0 /
     DATA BETA3(22),ALP3(22)   /  0.360776D0 ,  0.589380D0 /
     DATA BETA3(26),ALP3(26)   /  0.310959D0 ,  1.909763D0 /
     DATA BETA3(27),ALP3(27)   /  0.410886D0 ,  1.635259D0 /
     DATA BETA3(28),ALP3(28) /  0.377342D0 ,  2.029618D0 /
     DATA BETA3(29),ALP3(29) /  0.417759D0 ,  0.478901D0 /
     DATA BETA3(33),ALP3(33) /  0.349745D0 ,  2.484827D0 /
     DATA BETA3(34),ALP3(34) /  0.464514D0 ,  1.820975D0 /
     DATA BETA3(35),ALP3(35) /  0.458110D0 ,  1.873859D0 /
     DATA BETA3(36),ALP3(36) /  0.659407D0 ,  1.537190D0 /
     DATA BETA3(37),ALP3(37) /  0.195242D0 ,  3.771362D0 /
     DATA BETA3(41),ALP3(41) /  0.219591D0 ,  2.862183D0 /
     DATA BETA3(42),ALP3(42) /  0.247494D0 ,  2.725913D0 /
     DATA BETA3(43),ALP3(43) /  0.205347D0 ,  2.861667D0 /
     DATA BETA3(44),ALP3(44) /  0.334044D0 ,  2.266949D0 /
     DATA BETA3(45),ALP3(45) /  0.197464D0 ,  3.864997D0 /
     DATA BETA3(92),ALP3(92) /  0.289647D0 ,  0.940789D0 /
     DATA BETA3(97),ALP3(97) /  0.411377D0 ,  1.101382D0 /
     DATA BETA3(105),ALP3(105) /  0.291703D0 ,  0.918432D0 /
     DATA BETA3(106),ALP3(106) /  0.320118D0 ,  0.923170D0 /
     DATA BETA3(111),ALP3(111) /  0.457816D0 ,  1.029693D0 /
     DATA BETA3(120),ALP3(120) /  0.311790D0 ,  1.186652D0 /
     DATA BETA3(121),ALP3(121) /  0.220654D0 ,  1.700698D0 /
     DATA BETA3(126),ALP3(126) /  0.284620D0 ,  1.761370D0 /
     DATA BETA3(127),ALP3(127) /  0.313170D0 ,  1.878176D0/
     DATA BETA3(128),ALP3(128) /  0.422890D0 ,  2.077240D0 /
     DATA BETA3(129),ALP3(129)  /  0.000000D0 ,  0.000000D0 /
     DATA BETA3(136),ALP3(136) /  0.202489D0 ,  1.751617D0 /
     DATA BETA3(137),ALP3(137) /  0.231653D0 ,  2.089404D0 /
     DATA BETA3(142),ALP3(142) /  0.315480D0 ,  1.676222D0 /
     DATA BETA3(143),ALP3(143) /  0.302298D0 ,  1.817064D0 /
     DATA BETA3(144),ALP3(144) /  0.000000D0 ,  0.000000D0 /
     DATA BETA3(151),ALP3(151) /  0.277322D0 ,  1.543720D0 /
     DATA BETA3(152),ALP3(152) /  0.221764D0 ,  1.950318D0 /
     DATA BETA3(153),ALP3(153) /  0.258969D0 ,  1.792125D0 /



 *** HERE COMES THE OPTIMIZED SLATER_S EXPONENTS FOR THE EVALUATION
     OF THE OVERLAP INTEGRALS AND MOLECULAR DIPOLE MOMENTS.

     DATA ZS3(1),ZP3(1)      /  1.3D0       ,  0.0D0      /
     DATA ZS3(5),ZP3(5)      /  1.211156D0 ,  0.972826D0 /
     DATA ZS3(6),ZP3(6)      /  1.739391D0 ,  1.709645D0 /
     DATA ZS3(7),ZP3(7)      /  2.704546D0 ,  1.870839D0 /
     DATA ZS3(8),ZP3(8)      /  3.640575D0 ,  2.168448D0 /
     DATA ZS3(9),ZP3(9)      /  3.111270D0 ,  1.41986D0 /
     DATA ZS3(14),ZP3(14)    /  1.629173D0 ,  1.381721D0 /
     DATA ZS3(15),ZP3(15)    /  1.926108D0 ,  1.590665D0 /
     DATA ZS3(16),ZP3(16)    /  1.719480D0 ,  1.403205D0 /
     DATA ZS3(17),ZP3(17)    /  3.430887D0 ,  1.627017D0 /



   END OF MINDO/3 SPECIFIC DATA


                    DATA FOR ELEMENT  1
     DATA USS   ( 1)/     -11.3954710D0/
     DATA BETAS ( 1)/      -6.9012090D0/
     DATA ZS    ( 1)/       1.3319670D0/
     DATA ALP   ( 1)/       2.6448900D0/
     DATA EISOL ( 1)/     -11.3954710D0/
     DATA AM    ( 1)/       0.4721793D0/
     DATA AD    ( 1)/       0.4721793D0/
     DATA AQ    ( 1)/       0.4721793D0/
     DATA GUESS1( 1,1)/       0.0763700D0/
     DATA GUESS2( 1,1)/       7.7536570D0/
     DATA GUESS3( 1,1)/       1.8607430D0/
     DATA GUESS1( 1,2)/      -0.0219120D0/
     DATA GUESS2( 1,2)/       2.9661090D0/
     DATA GUESS3( 1,2)/       2.6653650D0/
                    DATA FOR ELEMENT  4
     DATA USS   ( 4)/     -16.6023780D0/
     DATA UPP   ( 4)/     -10.7037710D0/
     DATA BETAS ( 4)/      -4.0170960D0/
     DATA BETAP ( 4)/      -4.0170960D0/
     DATA ZS    ( 4)/       1.0042100D0/
     DATA ZP    ( 4)/       1.0042100D0/
     DATA ZD    ( 4)/       0.2000000D0/
     DATA ALP   ( 4)/       1.6694340D0/
     DATA EISOL ( 4)/     -24.2047560D0/
     DATA DD    ( 4)/       1.4373245D0/
     DATA QQ    ( 4)/       1.2196103D0/
     DATA AM    ( 4)/       0.3307607D0/
     DATA AD    ( 4)/       0.3356142D0/
     DATA AQ    ( 4)/       0.3846373D0/
                    DATA FOR ELEMENT  5
     DATA USS   ( 5)/     -34.5471300D0/
     DATA UPP   ( 5)/     -23.1216900D0/
     DATA BETAS ( 5)/      -8.2520540D0/
     DATA BETAP ( 5)/      -8.2520540D0/
     DATA ZS    ( 5)/       1.5068010D0/
     DATA ZP    ( 5)/       1.5068010D0/
     DATA ZD    ( 5)/       0.2000000D0/
     DATA ALP   ( 5)/       2.1349930D0/
     DATA EISOL ( 5)/     -64.3159500D0/
     DATA DD    ( 5)/       0.9579073D0/
     DATA QQ    ( 5)/       0.8128113D0/
     DATA AM    ( 5)/       0.3891951D0/
     DATA AD    ( 5)/       0.4904730D0/
     DATA AQ    ( 5)/       0.5556979D0/
                    DATA FOR ELEMENT  6
     DATA USS   ( 6)/     -52.2869850D0/
     DATA UPP   ( 6)/     -39.3957210D0/
     DATA BETAS ( 6)/     -14.7893280D0/
     DATA BETAP ( 6)/      -9.4013270D0/
     DATA ZS    ( 6)/       1.7875370D0/
     DATA ZP    ( 6)/       1.7875370D0/
     DATA ZD    ( 6)/       0.2000000D0/
     DATA ALP   ( 6)/       2.6361300D0/
     DATA EISOL ( 6)/    -120.8954120D0/
     DATA DD    ( 6)/       0.8074662D0/
     DATA QQ    ( 6)/       0.6851578D0/
     DATA AM    ( 6)/       0.4494671D0/
     DATA AD    ( 6)/       0.6149474D0/
     DATA AQ    ( 6)/       0.6685897D0/
     DATA FN1   ( 6)/       0.0356490D0/
     DATA GUESS1( 6,1)/       0.0419910D0/
     DATA GUESS2( 6,1)/       7.7536570D0/
     DATA GUESS3( 6,1)/       1.8607430D0/
     DATA GUESS1( 6,2)/      -0.0006430D0/
     DATA GUESS2( 6,2)/       2.9661090D0/
     DATA GUESS3( 6,2)/       2.6653650D0/
                    DATA FOR ELEMENT  7
     DATA USS   ( 7)/     -66.7680870D0/
     DATA UPP   ( 7)/     -57.3605830D0/
     DATA BETAS ( 7)/     -12.8274670D0/
     DATA BETAP ( 7)/     -21.8979740D0/
     DATA ZS    ( 7)/       2.2556140D0/
     DATA ZP    ( 7)/       2.2556140D0/
     DATA ZD    ( 7)/       0.2000000D0/
     DATA ALP   ( 7)/       2.8749780D0/
     DATA EISOL ( 7)/    -192.8029230D0/
     DATA DD    ( 7)/       0.6399037D0/
     DATA QQ    ( 7)/       0.5429763D0/
     DATA AM    ( 7)/       0.4994487D0/
     DATA AD    ( 7)/       0.7843643D0/
     DATA AQ    ( 7)/       0.8126445D0/
     DATA FN1   ( 7)/       0.4664350D0/
     DATA GUESS1( 7,1)/       0.0191900D0/
     DATA GUESS2( 7,1)/       7.7536570D0/
     DATA GUESS3( 7,1)/       1.8607430D0/
     DATA GUESS1( 7,2)/       0.0105670D0/
     DATA GUESS2( 7,2)/       2.9661090D0/
     DATA GUESS3( 7,2)/       2.6653650D0/
                    DATA FOR ELEMENT  8
     DATA USS   ( 8)/     -92.6269880D0/
     DATA UPP   ( 8)/     -77.8228510D0/
     DATA BETAS ( 8)/     -22.7022330D0/
     DATA BETAP ( 8)/     -32.4538720D0/
     DATA ZS    ( 8)/       2.6999050D0/
     DATA ZP    ( 8)/       2.6999050D0/
     DATA ZD    ( 8)/       0.2000000D0/
     DATA ALP   ( 8)/       3.2543070D0/
     DATA EISOL ( 8)/    -303.9353800D0/
     DATA DD    ( 8)/       0.5346024D0/
     DATA QQ    ( 8)/       0.4536252D0/
     DATA AM    ( 8)/       0.5667034D0/
     DATA AD    ( 8)/       0.9592562D0/
     DATA AQ    ( 8)/       0.9495934D0/
     DATA FN1   ( 8)/       0.1647660D0/
     DATA GUESS1( 8,1)/       0.0266930D0/
     DATA GUESS2( 8,1)/       7.7536570D0/
     DATA GUESS3( 8,1)/       1.8607430D0/
     DATA GUESS1( 8,2)/      -0.0044940D0/
     DATA GUESS2( 8,2)/       2.9661090D0/
     DATA GUESS3( 8,2)/       2.6653650D0/
                    DATA FOR ELEMENT  9
     DATA USS   ( 9)/    -131.0715480D0/
     DATA UPP   ( 9)/    -105.7821370D0/
     DATA BETAS ( 9)/     -48.2904660D0/
     DATA BETAP ( 9)/     -36.5085400D0/
     DATA ZS    ( 9)/       2.8484870D0/
     DATA ZP    ( 9)/       2.8484870D0/
     DATA ZD    ( 9)/       0.2000000D0/
     DATA ALP   ( 9)/       3.4196606D0/
     DATA EISOL ( 9)/    -476.6837810D0/
     DATA DD    ( 9)/       0.5067166D0/
     DATA QQ    ( 9)/       0.4299633D0/
     DATA AM    ( 9)/       0.6218302D0/
     DATA AD    ( 9)/       1.0850301D0/
     DATA AQ    ( 9)/       1.0343643D0/
                    DATA FOR ELEMENT 13
     DATA USS   (13)/     -23.8070970D0/
     DATA UPP   (13)/     -17.5198780D0/
     DATA BETAS (13)/      -2.4502840D0/
     DATA BETAP (13)/      -2.6702840D0/
     DATA ZS    (13)/       1.4441610D0/
     DATA ZP    (13)/       1.4441610D0/
     DATA ZD    (13)/       1.0000000D0/
     DATA ALP   (13)/       1.8688394D0/
     DATA EISOL (13)/     -43.0840720D0/
     DATA DD    (13)/       1.3992387D0/
     DATA QQ    (13)/       1.1586797D0/
     DATA AM    (13)/       0.2973172D0/
     DATA AD    (13)/       0.2635574D0/
     DATA AQ    (13)/       0.3673560D0/
                    DATA FOR ELEMENT 14
     DATA USS   (14)/     -40.5682920D0/
     DATA UPP   (14)/     -28.0891870D0/
     DATA BETAS (14)/      -4.2562180D0/
     DATA BETAP (14)/      -4.2562180D0/
     DATA ZS    (14)/       1.4353060D0/
     DATA ZP    (14)/       1.4353060D0/
     DATA ZD    (14)/       1.0000000D0/
     DATA ALP   (14)/       2.1961078D0/
     DATA EISOL (14)/     -90.5399580D0/
     DATA DD    (14)/       1.4078712D0/
     DATA QQ    (14)/       1.1658281D0/
     DATA AM    (14)/       0.3608967D0/
     DATA AD    (14)/       0.3441817D0/
     DATA AQ    (14)/       0.3999442D0/
                    DATA FOR ELEMENT 15
     DATA USS   (15)/     -56.1433600D0/
     DATA UPP   (15)/     -42.8510800D0/
     DATA BETAS (15)/      -6.7916000D0/
     DATA BETAP (15)/      -6.7916000D0/
     DATA ZS    (15)/       2.1087200D0/
     DATA ZP    (15)/       1.7858100D0/
     DATA ZD    (15)/       1.0000000D0/
     DATA ALP   (15)/       2.4152800D0/
     DATA EISOL (15)/    -152.9599600D0/
     DATA DD    (15)/       1.0129699D0/
     DATA QQ    (15)/       0.9370090D0/
     DATA AM    (15)/       0.4248438D0/
     DATA AD    (15)/       0.4882420D0/
     DATA AQ    (15)/       0.4979406D0/
                    DATA FOR ELEMENT 16
     DATA USS   (16)/     -75.2391520D0/
     DATA UPP   (16)/     -57.8320130D0/
     DATA BETAS (16)/     -11.1422310D0/
     DATA BETAP (16)/     -11.1422310D0/
     DATA ZS    (16)/       2.6135910D0/
     DATA ZP    (16)/       2.0343930D0/
     DATA ZD    (16)/       1.0000000D0/
     DATA ALP   (16)/       2.4916445D0/
     DATA EISOL (16)/    -235.4413560D0/
     DATA DD    (16)/       0.8231596D0/
     DATA QQ    (16)/       0.8225156D0/
     DATA AM    (16)/       0.4733554D0/
     DATA AD    (16)/       0.5889395D0/
     DATA AQ    (16)/       0.5632724D0/
                    DATA FOR ELEMENT 17
     DATA USS   (17)/    -100.2271660D0/
     DATA UPP   (17)/     -77.3786670D0/
     DATA BETAS (17)/     -14.2623200D0/
     DATA BETAP (17)/     -14.6232000D0/
     DATA ZS    (17)/       3.7846450D0/
     DATA ZP    (17)/       2.0362630D0/
     DATA ZD    (17)/       1.0000000D0/
     DATA ALP   (17)/       2.5422010D0/
     DATA EISOL (17)/    -353.1176670D0/
     DATA DD    (17)/       0.4986870D0/
     DATA QQ    (17)/       0.8217603D0/
     DATA AM    (17)/       0.5523705D0/
     DATA AD    (17)/       0.8061220D0/
     DATA AQ    (17)/       0.6053435D0/
                    DATA FOR ELEMENT 35
     DATA USS   (35)/     -99.9864405D0/
     DATA UPP   (35)/     -75.6713075D0/
     DATA BETAS (35)/      -8.9171070D0/
     DATA BETAP (35)/      -9.9437400D0/
     DATA ZS    (35)/       3.8543019D0/
     DATA ZP    (35)/       2.1992091D0/
     DATA ZD    (35)/       1.0000000D0/
     DATA ALP   (35)/       2.4457051D0/
     DATA EISOL (35)/    -346.6812500D0/
     DATA DD    (35)/       0.6051074D0/
     DATA QQ    (35)/       0.9645873D0/
     DATA AM    (35)/       0.5526068D0/
     DATA AD    (35)/       0.7258330D0/
     DATA AQ    (35)/       0.5574589D0/
                    DATA FOR ELEMENT 53
     DATA USS   (53)/    -100.0030538D0/
     DATA UPP   (53)/     -74.6114692D0/
     DATA BETAS (53)/      -7.4144510D0/
     DATA BETAP (53)/      -6.1967810D0/
     DATA ZS    (53)/       2.2729610D0/
     DATA ZP    (53)/       2.1694980D0/
     DATA ZD    (53)/       1.0000000D0/
     DATA ALP   (53)/       2.2073200D0/
     DATA EISOL (53)/    -340.5983600D0/
     DATA DD    (53)/       1.4253233D0/
     DATA QQ    (53)/       1.1841707D0/
     DATA AM    (53)/       0.5527541D0/
     DATA AD    (53)/       0.4593451D0/
     DATA AQ    (53)/       0.4585376D0/
     END
     SUBROUTINE MOLDAT
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES/NOLIST'
     COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
    +                NA(NUMATM),NB(NUMATM),NC(NUMATM)
    +       /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
    +                NCLOSE,NOPEN
    2       /KEYWRD/ KEYWRD
    3       /NATORB/ NATORB(54)
    4       /CORE  / CORE(54)
    5       /BETAS / BETAS(54),BETAP(54),BETAD(54)
    6       /MOLORB/ USPD(MAXORB),PSPD(MAXORB)
    7       /VSIPS / VS(54),VP(54),VD(54)
    8       /ONELEC/ USS(54),UPP(54),UDD(54)
    9       /ATHEAT/ ATHEAT
     COMMON /GMETRY/ GEO(3,NUMATM)
     PARAMETER (MDUMY=MAXORB*MAXORB+(NUMATM*(NUMATM+1))/2-MPACK)
     COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MDUMY)

  COMMON BLOCKS FOR MINDO/3

     COMMON /ONELE3 /  USS3(18),UPP3(18)
    1       /ATOMI3 /  EISOL3(18),EHEAT3(18)
    4       /EXPON3 /  ZS3(18),ZP3(18)

  END OF MINDO/3 COMMON BLOCKS

     COMMON /EXPONT/ ZS(54),ZP(54),ZD(54)
     COMMON /ATOMIC/ EISOL(54),EHEAT(54)
     DIMENSION COORD(3,NUMATM)
     CHARACTER*80 KEYWRD
     LOGICAL DEBUG, UHF,EXCI, TRIP, MINDO3, BIRAD,PARAM
     DEBUG = (INDEX(KEYWRD,'MOLDAT').NE.0)
     MINDO3= (INDEX(KEYWRD,'MINDO3').NE.0)
     UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
     KHARGE=0
     I=INDEX(KEYWRD,'CHARGE')
     IF(I.NE.0) KHARGE=READA(KEYWRD,I)
     NELECS=-KHARGE
     NDORBS=0
     ATHEAT=0.D0
     EAT=0.D0
     NUMAT=0
     IF( MINDO3 ) THEN
         DO 10 I=1,18
         USS(I)=USS3(I)
         UPP(I)=UPP3(I)
         EISOL(I)=EISOL3(I)
         EHEAT(I)=EHEAT3(I)
         ZS(I)=ZS3(I)
 10      ZP(I)=ZP3(I)
     ENDIF
     IA=1
     IB=0
     DO 190 II=1,NATOMS
        IF(LABELS(II).EQ.99) GOTO 190
        NUMAT=NUMAT+1
        NAT(NUMAT)=LABELS(II)
        NFIRST(NUMAT)=IA
        NI=NAT(NUMAT)
        ATHEAT=ATHEAT+EHEAT(NI)
        EAT   =EAT   +EISOL(NI)
        NELECS=NELECS+NINT(CORE(NI))
        IB=IA+NATORB(NI)-1
        NMIDLE(NUMAT)=IB
        IF(NATORB(NI).EQ.9)NDORBS=NDORBS+5
        IF(NATORB(NI).EQ.9)NMIDLE(NUMAT)=IA+3
        NLAST(NUMAT)=IB
        USPD(IA)=USS(NI)
        IF(IA.EQ.IB) GOTO 183
        K=IA+1
        K1=IA+3
        DO 181 J=K,K1
        USPD(J)=UPP(NI)
 181    CONTINUE
 182    IF(K1.EQ.IB)GOTO 183
        K=K1+1
        DO 184 J=K,IB
 184    USPD(J)=UDD(NI)
 183    CONTINUE

 190 IA=IB+1
     ATHEAT=ATHEAT-EAT*23.061D0
     NORBS=NLAST(NUMAT)
     TRIP=(INDEX(KEYWRD,'TRIPLET').NE.0)
     EXCI=(INDEX(KEYWRD,'EXCITED').NE.0)
     BIRAD=(INDEX(KEYWRD,'BIRAD').NE.0)
     PARAM=(INDEX(KEYWRD,'PARAM').NE.0)
     IF(INDEX(KEYWRD,'C.I.') .NE. 0) THEN
         IF(TRIP) THEN
             WRITE(6,'(//10X,''C.I. NOT ALLOWED WITH TRIPLET '')')
             STOP
         ENDIF
         IF(UHF) THEN
             WRITE(6,'(//10X,''C.I. NOT ALLOWED WITH UHF '')')
             STOP
         ENDIF
     ENDIF

 NOW TO WORK OUT HOW MANY ELECTRONS ARE IN EACH TYPE OF SHELL

     NALPHA=0
     NBETA=0
     NCLOSE=0
     NOPEN=0
     IF( UHF ) THEN
         NBETA=NELECS/2
         IF( TRIP ) THEN
             IF(NBETA*2 .NE. NELECS) THEN
                 WRITE(6,'(//10X,''TRIPLET SPECIFIED WITH ODD NUMBER'',
    +            '' OF ELECTRONS, CORRECT FAULT '')')
                 STOP
                 ELSE
                 WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
                 NBETA=NBETA-1
             ENDIF
         ENDIF
         NALPHA=NELECS-NBETA
     WRITE(6,'(//10X,''UHF CALCULATION, NO. OF ALPHA ELECTRONS ='',I3,
    +/27X,''NO. OF BETA  ELECTRONS ='',I3)')NALPHA,NBETA
     ELSE

   NOW TO DETERMINE OPEN AND CLOSED SHELLS

          NCLOSE=NELECS/2
          NOPEN = NELECS-NCLOSE*2
         IF( TRIP .OR. EXCI .OR. BIRAD ) THEN
             IF(NCLOSE*2 .NE. NELECS) THEN
                 WRITE(6,'(//10X,''SYSTEM SPECIFIED WITH ODD NUMBER'',
    +            '' OF ELECTRONS, CORRECT FAULT '')')
                 STOP
                 ELSE
     WRITE(6,'(//'' SYSTEM IS A BIRADICAL'')')
     IF(TRIP)WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
     IF(EXCI)WRITE(6,'(//'' EXCITED STATE CALCULATION'')')
     IF(.NOT. (TRIP .OR. EXCI ).AND. .NOT. PARAM)
    +WRITE(6,'(//'' GROUND STATE CALCULATION'')')
                 NCLOSE=NCLOSE-1
                 NOPEN=2
             ENDIF
         ENDIF
     IF( .NOT. PARAM)WRITE(6,'(//10X,''RHF CALCULATION, NO. OF '',
    +''DOUBLY OCCUPIED LEVELS ='',I3)')NCLOSE
     IF(NOPEN.NE.0)
    +WRITE(6,'(/27X,''NO. OF SINGLY OCCUPIED LEVELS ='',I3)')NOPEN
     NOPEN=NOPEN+NCLOSE
     ENDIF
     YY=FLOAT(KHARGE)/FLOAT(NORBS)
     L=0
     DO 191 I=1,NUMAT
     NI=NAT(I)
     XX=1.D0
     IF(NI.GT.2) XX=0.25D0
     W=CORE(NI)*XX-YY
     IA=NFIRST(I)
     IB=NLAST(I)
     DO 360 J=IA,IB
     L=L+1
 360 PSPD(L)=W
 191 CONTINUE

   WRITE OUT THE INTERATOMIC DISTANCES

     CALL GMETRY(GEO,COORD)
     RMIN=100.D0
     L=0
     DO 17 I=1,NUMAT
     DO 17 J=1,I
     L=L+1
     RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
    +             (COORD(2,I)-COORD(2,J))**2+
    1             (COORD(3,I)-COORD(3,J))**2)
     IF(RMIN.GT.RXYZ(L) .AND. I .NE. J) THEN
         IMINR=I
         JMINR=J
         RMIN=RXYZ(L)
     ENDIF
 17  CONTINUE
     IF (INDEX(KEYWRD,'PARAM')+INDEX(KEYWRD,'NOINTER') .EQ. 0) THEN
     WRITE(6,'(//10X,''  INTERATOMIC DISTANCES'')')
     CALL VECPRT(RXYZ,NUMAT)
     ENDIF
     IF(RMIN.LT.0.8D0.AND.INDEX(KEYWRD,'GEO-OK') .EQ.0) THEN
     WRITE(6,332)IMINR,JMINR,RMIN
 332 FORMAT(//,'   ATOMS',I3,' AND',I3,' ARE SEPARATED BY',F8.4,
    +' ANGSTROMS.',/'   TO CONTINUE CALCULATION SPECIFY "GEO-OK"')
     STOP
     ENDIF
     IF(.NOT. DEBUG) RETURN
     WRITE(6,1)NUMAT,NORBS,NDORBS,NATOMS
  1  FORMAT('   NUMBER OF REAL ATOMS:',I4,/
    +      ,'   NUMBER OF ORBITALS:  ',I4,/
    1      ,'   NUMBER OF D ORBITALS:',I4,/
    2      ,'   TOTAL NO. OF ATOMS:  ',I4)
     WRITE(6,3)(USPD(I),I=1,NORBS)
  3  FORMAT('   ONE-ELECTRON DIAGONAL TERMS',/,10(/,10F8.3))
     WRITE(6,5)(PSPD(I),I=1,NORBS)
  5  FORMAT('   INITIAL P FOR ALL ATOMIC ORBITALS',/,10(/,10F8.3))
     RETURN
     END
     SUBROUTINE ROTATE (NI,NJ,XI,XJ,W,KR,E1B,E2A,ENUC,HSS)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     DIMENSION XI(3),XJ(3),W(100),E1B(10),E2A(10)
     COMMON /NATORB/ NATORB(54)
     COMMON /BETAS / BETAS(54),DUMY(108)
     COMMON /TWOEL3/ F03(18)
     COMMON /ALPHA3/ ALP3(153)
     COMMON /ALPHA / ALP(54)
     COMMON /CORE  / TORE(54)
     COMMON /IDEAS / FN1(54,10),FN2(54,10),FN3(54,10)
****************************************************************************

   DIELRE CALCULATES THE TWO-PARTICLE INTERACTIONS.

   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
             NJ     = ATOMIC NUMBER OF SECOND ATOM.
             XI     = COORDINATE OF FIRST ATOM.
             XJ     = COORDINATE OF SECOND ATOM.

   ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS.
             E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
                      E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
             ENUC   = NUCLEAR-NUCLEAR REPULSION TERM.

****************************************************************************
     COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
     COMMON /ROTDU2/ X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3
     COMMON /KEYWRD/ KEYWRD
     CHARACTER*80 KEYWRD
     DIMENSION X(3),Y(3),Z(3),RI(22),CORE(4,2), COVRAD(54)
     LOGICAL SI,SK, FIRST
     EQUIVALENCE (CORE(1,1),CSS1),(X(1),X1),(Y(1),Y1),(Z(1),Z1)
     DATA ITYPE /1/
     DATA COVRAD/54*2.4D0/
     DATA COVRAD(1) /0.9D0/
     DATA FIRST /.TRUE./
                                                                       
 *** THIS ROUTINE COMPUTES THE REPUSLION AND NUCLEAR ATTRACTION        
     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER   
     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND STORED
     AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* ) IN RI      
     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,    
     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,   
     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,   
     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,   
     (PP/P*P*)=21,   (P*P/P*P)=22.                                     

     IF(FIRST) THEN
     FIRST=.FALSE.
     ENDIF
     CONST1=ALP(2)
     CONST2=ALP(10)
     RIJ=0.D0
     SIJ=1.D0
     NT=NI+NJ
#      IF(NT.EQ.8.OR.NT.EQ.9) THEN
#      IF(NI.EQ.7.OR.NI.EQ.8) SIJ=2.D0*HSS/(BETAS(NI)+BETAS(NJ))
#      IF(NJ.EQ.7.OR.NJ.EQ.8) SIJ=2.D0*HSS/(BETAS(NI)+BETAS(NJ))
#      ENDIF
#      WRITE(6,'(4F12.6)')SIJ,HSS,BETAS(NI),BETAS(NJ)
     DO 15 I=1,3
     X(I)=XI(I)-XJ(I)
 15  RIJ=RIJ+X(I)**2
 14  GOTO (100,200,300) ITYPE
 100 CONTINUE
     IF(INDEX(KEYWRD,'MINDO3') .NE. 0) THEN
         ITYPE=2
     ELSE
         ITYPE=3
     ENDIF
     GOTO 14
 200 CONTINUE
     SUM=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2)
     W(1)=SUM
     KR=KR+1
     L=0
     DO 210 I=1,4
     DO 220 J=1,I
     L=L+1
     E1B(L)=0.D0
 220 E2A(L)=0.D0
     E1B(L)=-SUM*TORE(NJ)
 210 E2A(L)=-SUM*TORE(NI)
     II=MAX(NI,NJ)
     NBOND=(II*(II-1))/2+NI+NJ-II
     RIJ=SQRT(RIJ)
     IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) GO TO 2
     GO TO 1
  2  SCALE=ALP3(NBOND)*EXP(-RIJ)
     GO TO 10
  1  SCALE=EXP(-ALP3(NBOND)*RIJ)
 10  CONTINUE
     ENUC=TORE(NI)*TORE(NJ)*(SUM+(14.399D0/RIJ-SUM)*SCALE)
     RETURN
 300 CONTINUE
     RIJ=SQRT(RIJ)
     CALL REPP(NI,NJ,RIJ,RI,CORE)
     GAM=RI(1)                                                         
                                                                       
 *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
     ORDER IN WHICH THEY WILL LATER BE USED.  IE.  (I,J/K,L) WHERE     
     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST     
     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)                          

     A=1.D0/RIJ
     DO 11 I=1,3
 11  X(I)=X(I)*A
     Z(3)=0.D0
     IF(ABS(X(3)).GT.0.999999D0) GOTO 12
     Z(3)=SQRT(1.D0-X(3)**2)
     A=1.D0/Z(3)
     Y(1)=-A*X(2)*SIGN(1.D0,X(1))
     Y(2)=ABS(A*X(1))
     Y(3)=0.D0
     Z(1)=-A*X(1)*X(3)
     Z(2)=-A*X(2)*X(3)
     GOTO 13
 12  Y(1)=0.D0
     Y(2)=1.D0
     Y(3)=0.D0
     Z(1)=1.D0
     Z(2)=0.D0
 13  CONTINUE
     IB=NATORB(NI)
     JB=NATORB(NJ)
     KI=0
     DO 130 I=1,IB
        SI=I.EQ.1                                                     
        II=I-1                                                        
     DO 130 J=1,I                                                     
        JJ=J-1                                                        
        IJ=0                                                           
        IF (JJ.EQ.0) IJ=-1                                             
        IF (SI) IJ=+1                                                  
     DO 130 K=1,JB                                                    
        KK=K-1                                                        
        SK=KK.GT.0                                                     
     DO 130 L=1,K                                                     
        KI=KI+1                                                        
        IF (SK) GO TO 50                                               
                                                                       
 *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/S,S)                       
                                                                       
        IF (IJ) 30,40,20                                               
                                                                       
     (SS/SS)                                                           
                                                                       
  20    W(KI)=RI(1)                                                    
        GO TO 131                                                      
                                                                       
     (PS/SS)                                                           
                                                                       
  30    W(KI)=RI(2)*X(II)                                              
        GO TO 131                                                      
                                                                       
     (PP/SS)                                                           
                                                                       
  40    W(KI)=RI(3)*X(II)*X(JJ)+RI(4)*(Y(II)*Y(JJ)+Z(II)*Z(JJ))        
        GO TO 131                                                      
  50    LL=L-1                                                        
        IF (LL.GT.0) GO TO 90                                          
                                                                       
 *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/P,S)                       
                                                                       
        IF (IJ) 70,80,60                                               
                                                                       
     (SS/PS)                                                           
                                                                       
  60    W(KI)=RI(5)*X(KK)                                              
        GO TO 131                                                      
                                                                       
     (PS/PS)                                                           
                                                                       
  70    W(KI)=RI(6)*X(II)*X(KK)+RI(7)*(Y(II)*Y(KK)+Z(II)*Z(KK))        
        GO TO 131                                                      
                                                                       
     (PP/PS)                                                           
                                                                       
  80    W(KI)=X(KK)*(RI(8)*X(II)*X(JJ)+RI(9)*(Y(II)*Y(JJ)+Z(II)*Z(JJ)))
    1   +RI(10)*(X(II)*(Y(JJ)*Y(KK)+Z(JJ)*Z(KK))+X(JJ)*(Y(II)*Y(KK)+Z(I
    2   I)*Z(KK)))                                                     
        GO TO 131                                                      
                                                                       
 *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/P,P)                       
                                                                       
  90    IF (IJ) 110,120,101                                            
                                                                       
     (SS/PP)                                                           
                                                                       
 101    W(KI)=RI(11)*X(KK)*X(LL)+RI(12)*(Y(KK)*Y(LL)+Z(KK)*Z(LL))      
        GO TO 131                                                      
                                                                       
     (PS/PP)                                                           
                                                                       
 110    W(KI)=X(II)*(RI(13)*X(KK)*X(LL)+RI(14)*(Y(KK)*Y(LL)+Z(KK)*Z(LL)
    1   ))+RI(15)*(Y(II)*(Y(KK)*X(LL)+Y(LL)*X(KK))+Z(II)*(Z(KK)*X(LL)+Z
    2   (LL)*X(KK)))                                                   
        GO TO 131                                                      
                                                                       
     (PP/PP)                                                           
                                                                       
 120    W(KI)=(RI(16)*X(II)*X(JJ)+RI(17)*(Y(II)*Y(JJ)+Z(II)*Z(JJ)))*X(K
    1   K)*X(LL)+RI(18)*X(II)*X(JJ)*(Y(KK)*Y(LL)+Z(KK)*Z(LL))+RI(19)*(Y
    2   (II)*Y(JJ)*Y(KK)*Y(LL)+Z(II)*Z(JJ)*Z(KK)*Z(LL))+RI(20)*(X(II)*(
    3   X(KK)*(Y(JJ)*Y(LL)+Z(JJ)*Z(LL))+X(LL)*(Y(JJ)*Y(KK)+Z(JJ)*Z(KK))
    4   )+X(JJ)*(X(KK)*(Y(II)*Y(LL)+Z(II)*Z(LL))+X(LL)*(Y(II)*Y(KK)+Z(I
    5   I)*Z(KK))))+RI(21)*(Y(II)*Y(JJ)*Z(KK)*Z(LL)+Z(II)*Z(JJ)*Y(KK)*Y
    6   (LL))+RI(22)*(Y(II)*Z(JJ)+Z(II)*Y(JJ))*(Y(KK)*Z(LL)+Z(KK)*Y(LL)
    7   )                                                              
 131 CONTINUE                                                          
 130 CONTINUE                                                          
 150 CONTINUE
     E1B(1)=-CSS1
     IF(NI.GT.3) THEN
     E1B(2) = -CSP1 *X1
     E1B(3) = -CPPS1*X1**2-CPPP1*(Y1**2+Z1**2)
     E1B(4) = -CSP1 *X2
     E1B(5) = -CPPS1*X1*X2-CPPP1*(Y1*Y2+Z1*Z2)
     E1B(6) = -CPPS1*X2*X2-CPPP1*(Y2*Y2+Z2*Z2)
     E1B(7) = -CSP1 *X3
     E1B(8) = -CPPS1*X1*X3-CPPP1*(Y1*Y3+Z1*Z3)
     E1B(9) = -CPPS1*X2*X3-CPPP1*(Y2*Y3+Z2*Z3)
     E1B(10)= -CPPS1*X3*X3-CPPP1*(Y3*Y3+Z3*Z3)
     END IF
     E2A(1)=-CSS2
     IF(NJ.GT.3) THEN
     E2A(2) = -CSP2 *X1
     E2A(3) = -CPPS2*X1**2-CPPP2*(Y1**2+Z1**2)
     E2A(4) = -CSP2 *X2
     E2A(5) = -CPPS2*X1*X2-CPPP2*(Y1*Y2+Z1*Z2)
     E2A(6) = -CPPS2*X2*X2-CPPP2*(Y2*Y2+Z2*Z2)
     E2A(7) = -CSP2 *X3
     E2A(8) = -CPPS2*X1*X3-CPPP2*(Y1*Y3+Z1*Z3)
     E2A(9) = -CPPS2*X2*X3-CPPP2*(Y2*Y3+Z2*Z3)
     E2A(10)= -CPPS2*X3*X3-CPPP2*(Y3*Y3+Z3*Z3)
     END IF
     SCALE = EXP(-ALP(NI)*RIJ)+EXP(-ALP(NJ)*RIJ)
     NT=NI+NJ
     IF(NT.EQ.8.OR.NT.EQ.9) THEN
     IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NI)*RIJ)
     IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NJ)*RIJ)
     ELSE
     ENDIF
     ENUC = TORE(NI)*TORE(NJ)*GAM
     SCALE=SCALE*ENUC
#      ENUC = ENUC +FN1(NI)*EXP(-FN2(NI)*(RIJ-FN3(NI))**2)
#     +            +FN1(NJ)*EXP(-FN2(NJ)*(RIJ-FN3(NJ))**2)
#      WRITE(6,'('' GAUSSIAN'',F12.6)')
     DO 156 IG=1,10
     IF(ABS(FN1(NI,IG)).GT.0.D0)
    +SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*
    +FN1(NI,IG)*EXP(-FN2(NI,IG)*(RIJ-FN3(NI,IG))**2)
     IF(ABS(FN1(NJ,IG)).GT.0.D0)
    +SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*
    +FN1(NJ,IG)*EXP(-FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2)
 156 CONTINUE
     ENUC=ENUC+SCALE
                                                                       
 *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.                      
 *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS   
     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4                          
                                                                       
   DEBUG PRINTING                                                      
                                                                       
     KR=KR+KI
     RETURN                                                            
     END                                                               
