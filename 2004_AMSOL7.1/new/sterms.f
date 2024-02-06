      SUBROUTINE STERMS(BPGT,FUNCT,TSOLVE,NFILE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         INCLUDE 'SIZES.i'
         INCLUDE 'KEYS.i'
C******************************************************************************
C
C   THIS SUBROUTINE PRINTS OUT THE SOLVATION ENERGY DIVIDED UP INTO TERMS
C
C   GASENG = GAS PHASE ENERGY
C   ENER2 = ELECTRONIC AND NUCLEAR ENERGY OF SOLUTE
C   BPGT = TOTAL POLARIZATION ENERGY
C   SURFCT = TOTAL CDS ENERGY
C
C   CREATED BY DJG 0995
C
C******************************************************************************
      COMMON /ASOLCM/ GASENG
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /ONESCM/ ICONTR(100)
      IF (ICONTR(49).EQ.1) THEN
         ICONTR(49)=2
         ENER1 = BPGT + SURFCT                                          GL0492
         IF (INOPOL.NE.0.OR.ISM505.NE.0) THEN                           GDH0297
            GASENG = FUNCT                                              GL0492
            ENER2  = FUNCT                                              GL0492
         ELSE                                                           GL0492
            ENER2 = FUNCT - ENER1                                       GL0492
         ENDIF                                                          GL0492
         IF (ITRUES.NE.0) ENER3 = ENER2 - GASENG                        DJG0995
         TSOLVE=ENER2+BPGT+SURFCT-GASENG
      ENDIF
C
C  WRITE BREAKDOWN OF SOLVATION ENERGY CALCULATED                       GL0592
C
      IF (INOCOG.NE.0) WRITE(NFILE,1180)                                DJG0995
      IF(INOPOL.NE.0) THEN                                              DJG0995
         WRITE(NFILE, 1000)                                             DJG0995
         WRITE(NFILE,1010) GASENG                                       DJG0995
         WRITE(NFILE,1020)                                              DJG0995
      ELSEIF(ISM505.NE.0) THEN                                          GDH1196
          WRITE(NFILE,1200) SURFCT+BPGT                                 GDH0297
      ELSEIF(ISM50.NE.0) THEN                                           GDH1196
          WRITE(NFILE,1190) SURFCT                                      GDH0297
      ELSEIF(I1SCF.NE.0)  THEN                                          DJG0995
         WRITE(NFILE,1030)                                              DJG0995
         WRITE(NFILE,1040)                                              DJG0995
      ELSEIF(ITRUES.EQ.0) THEN                                          DJG0995
         WRITE(NFILE,1050)                                              DJG0995
         WRITE(NFILE,1060)                                              DJG0995
      ENDIF                                                             DJG0995
      IF(ITRUES.NE.0) THEN                                              DJG0995
         WRITE(NFILE,1070)                                              DJG0995
         WRITE(NFILE,1010) GASENG                                       DJG0995
         WRITE(NFILE,1080)                                              DJG0995
      ENDIF                                                             DJG0995
      IF (ISM50.EQ.0) THEN                                              GDH1196
      WRITE(NFILE,1090) ENER2                                           DJG0995
      WRITE(NFILE,1100) BPGT                                            DJG0995
      WRITE(NFILE,1110) ENER2 + BPGT                                    DJG0995
      WRITE(NFILE,1120) SURFCT                                          DJG0995
      WRITE(NFILE,1130) BPGT + SURFCT                                   DJG0995
      WRITE(NFILE,1140) ENER2 + BPGT + SURFCT                           DJG0995
      IF(ITRUES.NE.0.OR.INOPOL.NE.0) THEN                               DJG0995
         WRITE(NFILE,1150) ENER3                                        DJG0995
         WRITE(NFILE,1160) ENER2 + BPGT - GASENG                        DJG0995
         WRITE(NFILE,1170) ENER2 + BPGT + SURFCT - GASENG               DJG0995
      ENDIF                                                             GL0592
      ENDIF                                                             GDH1196
1000   FORMAT(/,1X,'NOPOL run')                                         GL0592
1010   FORMAT(/,1X,'(0)',T7,'E-EN(g) gas-phase electronic-nuclear ',    GL0592
     1        'energy', T65, F10.3, T76, 'kcal')                        GL0592
1020   FORMAT(/,4X,'This is a breakdown of the solvation energy ',      GL0592
     1        'calculated',/,4X,'without electronic or geometric ',     GL0592
     2        'relaxation:',/)                                          GL0592
1030   FORMAT(/,1X,'1SCF run')                                          GL0592
1040   FORMAT(/,4X,'This is a breakdown of the solvation energy ',      GL0592
     1        'calculated',/,4X,'without geometric relaxation ',        GL0592
     2        'in solution:',/)                                         GL0592
1050   FORMAT(/,1X,'geometry optimization run without TRUES')           GL0592
1060   FORMAT(/,4X,'This is a breakdown of the solvation energy ',      GL0592
     1        'calculated',/,4X,'for this specific geometry:',/)        GL0592
1070   FORMAT(/,1X,'geometry optimization run with TRUES')              GL0592
1080   FORMAT(/,4X,'This is a breakdown of the true solvation energy:', GL0592
     1        /)                                                        GL0592
1090   FORMAT(1X,'(1)',T7,'E-EN(sol) electronic-nuclear energy of ',    DJG0796
     1        'solute', T65, F10.3, T76, 'kcal')                        GL0592
1100   FORMAT(1X,'(2)',T7,'G-P(sol) polarization free energy of ',      DJG0796
     1        'solvation', T65, F10.3, T76, 'kcal')                     GL0592
1110   FORMAT(1X,'(3)',T7,'G-ENP(sol) elect.-nuc.-pol. free energy ',   DJG0796
     1        'of system', T65, F10.3, T76,'kcal')                      GL0592
1120   FORMAT(1X,'(4)',T7,'G-CDS(sol) cavity-dispersion-solvent ',      DJG0796
     1        'structure free energy', T65, F10.3, T76,'kcal')          GL0592
1130   FORMAT(1X,'(5)',T7,'G-P-CDS(sol) = G-P(sol) + G-CDS(sol) = (2)', DJG0796
     1        ' + (4)',T65, F10.3, T76, 'kcal')                         DJG0796
1140   FORMAT(1X,'(6)',T7,'G-S(sol) free energy of system = (1) + (5)', DJG0796
     1        T65, F10.3, T76, 'kcal')                                  GL0592
1150   FORMAT(1X,'(7)',T7,'DeltaE-EN(sol) elect.-nuc. ',                DJG0796
     1        'reorganization energy of solute ',T65,F10.3,T76,'kcal',  GL0592
     2        /,1X,T7,'(7) = (1) - (0)')                                GL0592
1160   FORMAT(1X,'(8)',T7,'DeltaG-ENP(sol) elect.-nuc.-pol. free ',     DJG0796
     1        'energy of solvation', T65, F10.3, T76, 'kcal',           GL0592
     2        /,1X,T7,'(8) = (3) - (0)')                                GL0592
1170   FORMAT(1X,'(9)',T7,'DeltaG-S(sol) free energy of  solvation',    DJG0796
     1        T65, F10.3, T76, 'kcal',/,1X,T7,'(9) = (6) - (0)')        GL0592
1180   FORMAT(/,'!!!!!!!   THE NOCOGS KEYWORD WAS USED   !!!!!!!',      DJG1095
     1        /,'This is a non-standard calculation.  The parameters ', DJG1095
     2        'in this model were developed',/,'using the COGS.  ',     DJG1095
     3        'For further information, please consult the manual.',/)  DJG1095
1190   FORMAT(1X,'Free energy of  solvation calculated with',           GDH1196
     1        T65, F10.3, T76, 'kcal',/,1X,'the',                       GDH1196
     2        ' SM5.0R model which neglects electrostatics')            GDH0497
1200   FORMAT(1X,'Free energy of  solvation calculated with',           GDH1196
     1        T65, F10.3, T76, 'kcal',/,1X,'the',                       GDH1196
     2        ' SM5.05R model for ions')                                GDH0497
      RETURN
      END
