       SUBROUTINE HEADER(IFU)
C
C   THIS SUBROUTINE PRINTS OUT THE FIRST TWO LINES OF THE HEADER FOR THE
C   AMSOL program
C
       IMPLICIT DOUBLE PRECISION (A-H, O-Z)
       INCLUDE 'SIZES.i'
       WRITE(IFU,1000)                                                  DJG0996
       WRITE(IFU, 1020)                                                 GDH1093
       WRITE(IFU, 1030)                                                 PLF0199
       WRITE(IFU, 1040)                                                 PLF0199
C1000   FORMAT('AMSOL-version 6.7')                                     BJL1003
C1000   FORMAT('AMSOL-version 6.7.1')                                   BJL1003
C1000   FORMAT('AMSOL-version 6.7.2')                                   !JT0102
C1000   FORMAT('AMSOL-version 6.8'  )                                   !JT0802
C1000   FORMAT('AMSOL-version 6.9'  )                                   !JT0303
1000   FORMAT(' AMSOL-version 7.1'  )                                   1128BL04
1020   FORMAT(' by G. D. Hawkins, D. J. Giesen, G. C. Lynch, ',         BJL1003
     .        'C. C. Chambers,',/,                                      GDH1297
     1        ' I. Rossi, J. W. Storer, J. Li, J. D. Thompson, ',       BJL1003
     2        'P. Winget,',/,                                           BJL1003
     3        ' B. J. Lynch, D. Rinaldi, D. A. Liotard, C. J. Cramer,', BJL1003
     4        ' and D. G. Truhlar',/)                                   BJL1003

1030   FORMAT(' Copyright 2004 by Regents of the University of ',       BJL1003
     1        'Minnesota.',/,' All rights reserved.',/)                 BJL1003
1040   FORMAT(' Notice: recipients of this code are asked to comply',/, BJL1003
     1        ' with the user agreement in Section 1 of the ',          BJL1003
     2        'documentation file.',/)                                  DJG0794
C
       RETURN
       END


