  module Parameters_for_RM1_C
    use vast_kind_param, ONLY:  double  
    real(double), dimension(107) :: ussRM1, uppRM1, zsRM1, zpRM1, betasRM1, &
    betapRM1, gssRM1, gspRM1, gppRM1, gp2RM1, hspRM1, alpRM1
    character (len=80), dimension(107) :: refRM1  
    real(double), dimension(107,4) :: guess1RM1, guess2RM1, guess3RM1
!
!                    Data for Element  1         Hydrogen
!
      data     ussRM1(  1)/     -11.9606770D0/
      data   betasRM1(  1)/      -5.7654447D0/
      data      zsRM1(  1)/       1.0826737D0/
      data     alpRM1(  1)/       3.0683595D0/
      data     gssRM1(  1)/      13.9832130D0/
      data guess1RM1(  1,1)/       0.1028888D0/
      data guess2RM1(  1,1)/       5.9017227D0/
      data guess3RM1(  1,1)/       1.1750118D0/
      data guess1RM1(  1,2)/       0.0645745D0/
      data guess2RM1(  1,2)/       6.4178567D0/
      data guess3RM1(  1,2)/       1.9384448D0/
      data guess1RM1(  1,3)/      -0.0356739D0/
      data guess2RM1(  1,3)/       2.8047313D0/
      data guess3RM1(  1,3)/       1.6365524D0/
!
!                    Data for Element  6           Carbon
!
      data     ussRM1(  6)/     -51.7255603D0/
      data     uppRM1(  6)/     -39.4072894D0/
      data   betasRM1(  6)/     -15.4593243D0/
      data   betapRM1(  6)/      -8.2360864D0/
      data      zsRM1(  6)/       1.8501880D0/
      data      zpRM1(  6)/       1.7683009D0/
      data     alpRM1(  6)/       2.7928208D0/
      data     gssRM1(  6)/      13.0531244D0/
      data     gspRM1(  6)/      11.3347939D0/
      data     gppRM1(  6)/      10.9511374D0/
      data     gp2RM1(  6)/       9.7239510D0/
      data     hspRM1(  6)/       1.5521513D0/
      data guess1RM1(  6,1)/       0.0746227D0/
      data guess2RM1(  6,1)/       5.7392160D0/
      data guess3RM1(  6,1)/       1.0439698D0/
      data guess1RM1(  6,2)/       0.0117705D0/
      data guess2RM1(  6,2)/       6.9240173D0/
      data guess3RM1(  6,2)/       1.6615957D0/
      data guess1RM1(  6,3)/       0.0372066D0/
      data guess2RM1(  6,3)/       6.2615894D0/
      data guess3RM1(  6,3)/       1.6315872D0/
      data guess1RM1(  6,4)/      -0.0027066D0/
      data guess2RM1(  6,4)/       9.0000373D0/
      data guess3RM1(  6,4)/       2.7955790D0/
!
!                    Data for Element  7         Nitrogen
!
      data     ussRM1(  7)/     -70.8512372D0/
      data     uppRM1(  7)/     -57.9773092D0/
      data   betasRM1(  7)/     -20.8712455D0/
      data   betapRM1(  7)/     -16.6717185D0/
      data      zsRM1(  7)/       2.3744716D0/
      data      zpRM1(  7)/       1.9781257D0/
      data     alpRM1(  7)/       2.9642254D0/
      data     gssRM1(  7)/      13.0873623D0/
      data     gspRM1(  7)/      13.2122683D0/
      data     gppRM1(  7)/      13.6992432D0/
      data     gp2RM1(  7)/      11.9410395D0/
      data     hspRM1(  7)/       5.0000085D0/
      data guess1RM1(  7,1)/       0.0607338D0/
      data guess2RM1(  7,1)/       4.5889295D0/
      data guess3RM1(  7,1)/       1.3787388D0/
      data guess1RM1(  7,2)/       0.0243856D0/
      data guess2RM1(  7,2)/       4.6273052D0/
      data guess3RM1(  7,2)/       2.0837070D0/
      data guess1RM1(  7,3)/      -0.0228343D0/
      data guess2RM1(  7,3)/       2.0527466D0/
      data guess3RM1(  7,3)/       1.8676382D0/
!
!                    Data for Element  8           Oxygen
!
      data     ussRM1(  8)/     -96.9494807D0/
      data     uppRM1(  8)/     -77.8909298D0/
      data   betasRM1(  8)/     -29.8510121D0/
      data   betapRM1(  8)/     -29.1510131D0/
      data      zsRM1(  8)/       3.1793691D0/
      data      zpRM1(  8)/       2.5536191D0/
      data     alpRM1(  8)/       4.1719672D0/
      data     gssRM1(  8)/      14.0024279D0/
      data     gspRM1(  8)/      14.9562504D0/
      data     gppRM1(  8)/      14.1451514D0/
      data     gp2RM1(  8)/      12.7032550D0/
      data     hspRM1(  8)/       3.9321716D0/
      data guess1RM1(  8,1)/       0.2309355D0/
      data guess2RM1(  8,1)/       5.2182874D0/
      data guess3RM1(  8,1)/       0.9036356D0/
      data guess1RM1(  8,2)/       0.0585987D0/
      data guess2RM1(  8,2)/       7.4293293D0/
      data guess3RM1(  8,2)/       1.5175461D0/
!
!                    Data for Element  9         Fluorine
!
      data     ussRM1(  9)/    -134.1836959D0/
      data     uppRM1(  9)/    -107.8466092D0/
      data   betasRM1(  9)/     -70.0000051D0/
      data   betapRM1(  9)/     -32.6798271D0/
      data      zsRM1(  9)/       4.4033791D0/
      data      zpRM1(  9)/       2.6484156D0/
      data     alpRM1(  9)/       6.0000006D0/
      data     gssRM1(  9)/      16.7209132D0/
      data     gspRM1(  9)/      16.7614263D0/
      data     gppRM1(  9)/      15.2258103D0/
      data     gp2RM1(  9)/      14.8657868D0/
      data     hspRM1(  9)/       1.9976617D0/
      data guess1RM1(  9,1)/       0.4030203D0/
      data guess2RM1(  9,1)/       7.2044196D0/
      data guess3RM1(  9,1)/       0.8165301D0/
      data guess1RM1(  9,2)/       0.0708583D0/
      data guess2RM1(  9,2)/       9.0000156D0/
      data guess3RM1(  9,2)/       1.4380238D0/
!
!                    Data for Element 15       Phosphorus
!
      data     ussRM1( 15)/     -41.8153318D0/
      data     uppRM1( 15)/     -34.3834253D0/
      data   betasRM1( 15)/      -6.1351497D0/
      data   betapRM1( 15)/      -5.9444213D0/
      data      zsRM1( 15)/       2.1224012D0/
      data      zpRM1( 15)/       1.7432795D0/
      data     alpRM1( 15)/       1.9099329D0/
      data     gssRM1( 15)/      11.0805926D0/
      data     gspRM1( 15)/       5.6833920D0/
      data     gppRM1( 15)/       7.6041756D0/
      data     gp2RM1( 15)/       7.4026518D0/
      data     hspRM1( 15)/       1.1618179D0/
      data guess1RM1( 15,1)/      -0.4106347D0/
      data guess2RM1( 15,1)/       6.0875283D0/
      data guess3RM1( 15,1)/       1.3165026D0/
      data guess1RM1( 15,2)/      -0.1629929D0/
      data guess2RM1( 15,2)/       7.0947260D0/
      data guess3RM1( 15,2)/       1.9072132D0/
      data guess1RM1( 15,3)/      -0.0488713D0/
      data guess2RM1( 15,3)/       8.9997931D0/
      data guess3RM1( 15,3)/       2.6585778D0/
!
!                    Data for Element 16           Sulfur
!
      data     ussRM1( 16)/     -55.1677512D0/
      data     uppRM1( 16)/     -46.5293042D0/
      data   betasRM1( 16)/      -1.9591072D0/
      data   betapRM1( 16)/      -8.7743065D0/
      data      zsRM1( 16)/       2.1334431D0/
      data      zpRM1( 16)/       1.8746065D0/
      data     alpRM1( 16)/       2.4401564D0/
      data     gssRM1( 16)/      12.4882841D0/
      data     gspRM1( 16)/       8.5691057D0/
      data     gppRM1( 16)/       8.5230117D0/
      data     gp2RM1( 16)/       7.6686330D0/
      data     hspRM1( 16)/       3.8897893D0/
      data guess1RM1( 16,1)/      -0.7460106D0/
      data guess2RM1( 16,1)/       4.8103800D0/
      data guess3RM1( 16,1)/       0.5938013D0/
      data guess1RM1( 16,2)/      -0.0651929D0/
      data guess2RM1( 16,2)/       7.2076086D0/
      data guess3RM1( 16,2)/       1.2949201D0/
      data guess1RM1( 16,3)/      -0.0065598D0/
      data guess2RM1( 16,3)/       9.0000018D0/
      data guess3RM1( 16,3)/       1.8006015D0/
!
!                    Data for Element 17         Chlorine
!
      data     ussRM1( 17)/    -118.4730692D0/
      data     uppRM1( 17)/     -76.3533034D0/
      data   betasRM1( 17)/     -19.9243043D0/
      data   betapRM1( 17)/     -11.5293520D0/
      data      zsRM1( 17)/       3.8649107D0/
      data      zpRM1( 17)/       1.8959314D0/
      data     alpRM1( 17)/       3.6935883D0/
      data     gssRM1( 17)/      15.3602310D0/
      data     gspRM1( 17)/      13.3067117D0/
      data     gppRM1( 17)/      12.5650264D0/
      data     gp2RM1( 17)/       9.6639708D0/
      data     hspRM1( 17)/       1.7648990D0/
      data guess1RM1( 17,1)/       0.1294711D0/
      data guess2RM1( 17,1)/       2.9772442D0/
      data guess3RM1( 17,1)/       1.4674978D0/
      data guess1RM1( 17,2)/       0.0028890D0/
      data guess2RM1( 17,2)/       7.0982759D0/
      data guess3RM1( 17,2)/       2.5000272D0/

!
!                    Data for Element 35          Bromine
!
      data     ussRM1( 35)/    -113.4839818D0/
      data     uppRM1( 35)/     -76.1872002D0/
      data   betasRM1( 35)/      -1.3413984D0/
      data   betapRM1( 35)/      -8.2022599D0/
      data      zsRM1( 35)/       5.7315721D0/
      data      zpRM1( 35)/       2.0314758D0/
      data     alpRM1( 35)/       2.8671053D0/
      data     gssRM1( 35)/      17.1156307D0/
      data     gspRM1( 35)/      15.6241925D0/
      data     gppRM1( 35)/      10.7354629D0/
      data     gp2RM1( 35)/       8.8605620D0/
      data     hspRM1( 35)/       2.2351276D0/
      data guess1RM1( 35,1)/       0.9868994D0/
      data guess2RM1( 35,1)/       4.2848419D0/
      data guess3RM1( 35,1)/       2.0001970D0/
      data guess1RM1( 35,2)/      -0.9273125D0/
      data guess2RM1( 35,2)/       4.5400591D0/
      data guess3RM1( 35,2)/       2.0161770D0/
!
!                    Data for Element 53           Iodine
!
      data     ussRM1( 53)/     -74.8999784D0/
      data     uppRM1( 53)/     -51.4102380D0/
      data   betasRM1( 53)/      -4.1931615D0/
      data   betapRM1( 53)/      -4.4003841D0/
      data      zsRM1( 53)/       2.5300375D0/
      data      zpRM1( 53)/       2.3173868D0/
      data     alpRM1( 53)/       2.1415709D0/
      data     gssRM1( 53)/      19.9997413D0/
      data     gspRM1( 53)/       7.6895767D0/
      data     gppRM1( 53)/       7.3048834D0/
      data     gp2RM1( 53)/       6.8542461D0/
      data     hspRM1( 53)/       1.4160294D0/
      data guess1RM1( 53,1)/      -0.0814772D0/
      data guess2RM1( 53,1)/       1.5606507D0/
      data guess3RM1( 53,1)/       2.0000206D0/
      data guess1RM1( 53,2)/       0.0591499D0/
      data guess2RM1( 53,2)/       5.7611127D0/
      data guess3RM1( 53,2)/       2.2048880D0/
!
!                    Data for Element102      Capped bond
!
      data     ussRM1(102)/     -11.9062760D0/
      data   betasRM1(102)/-9999999.0000000D0/
      data      zsRM1(102)/       4.0000000D0/
      data      zpRM1(102)/       0.3000000D0/
      data     alpRM1(102)/       2.5441341D0/
      data     gssRM1(102)/      12.8480000D0/
      data     hspRM1(102)/       0.1000000D0/
!
!       Data for Element 100:   3+ Sparkle
!
      data      alpRM1(100)/       1.5000000d0/
!
!       Data for Element 101:   3- Sparkle
!
      data      alpRM1(101)/       1.5000000d0/
!
!       Data for Element 103:   ++ Sparkle
!
      data      alpRM1(103)/       1.5000000d0/
!
!       Data for Element 104:    + Sparkle
!
      data      alpRM1(104)/       1.5000000d0/
!
!       Data for Element 105:   -- Sparkle
!
      data      alpRM1(105)/       1.5000000d0/
!
!       Data for Element 106:    - Sparkle
!
      data      alpRM1(106)/       1.5000000d0/
 end module Parameters_for_RM1_C
