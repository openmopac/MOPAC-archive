      SUBROUTINE EXTSMY(ANUM,LINE,ICNT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C******************************************************************************
C
C    THIS SUBROUTINE DOES AN OFT-REPEATED INTERPRETATION OF THE EXTSM
C    FILE DATA
C
C    ON INPUT
C       LINE = LINE FROM THE EXTSM FILE
C    
C    ON OUTPUT
C       ANUM = VALUE SOUGHT
C       ICNT = NUMBER OF VALUES FOUND
C
C    CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1
C
C******************************************************************************
      CHARACTER LINE*80, SPACE*1
      DIMENSION ANUM(7)
      DATA SPACE/' '/
      DO 50 J=1,7
         ANUM(J)=0.0D0
50    CONTINUE
      ICNT=0
      ISPACE=1                                                          DJG1094
      DO 100 I=3,80                                                     DJG1094
         IF (LINE(I:I).NE.SPACE) THEN                                   DJG1094
            IF (ISPACE.EQ.1) THEN                                       DJG1094
               ICNT=ICNT+1                                              DJG1094
               ANUM(ICNT)=READIF(LINE,I)                                DJG1094
               ISPACE=0                                                 DJG1094
            ENDIF                                                       DJG1094
         ELSE                                                           DJG1094
            ISPACE=1                                                    DJG1094
         ENDIF                                                          DJG1094
100   CONTINUE                                                          DJG1094
      RETURN
      END
