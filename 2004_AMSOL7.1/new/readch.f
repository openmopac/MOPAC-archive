      SUBROUTINE READCH (KEY,ISTART,WORD)                               DJG0995
C                                                                       DJG1094
C     SUBROUTINE READCH READS A CHARACTER STRING FROM THE VARIABLE      DJG1094
C     KEY AND RETURNS IT IN THE VARIABLE 'WORD'.  AT THE MOMENT         DJG0995
C     THE WORD IS LIMITED TO 6 CHARACTERS.  READCH TERMINATES WHEN IT   DJG1094
C     READS A SPACE. ISTART SHOULD BE THE INDEX OF THE BEGINNING OF WORDDJG1094
C                                                                       DJG1094
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               DJG1094
      CHARACTER*80 KEY                                                  DJG0995
      CHARACTER*1 SPACE                                                 DJG1094
      CHARACTER*6 WORD                                                  DJG1094
      SAVE                                                              DJG1094
      SPACE=' '                                                         DJG1094
      WORD='      '                                                     DJG1094
      ICOUNT=ISTART                                                     DJG1094
   10 IND=ICOUNT-ISTART+1                                               DJG1094
      IF (KEY(ICOUNT:ICOUNT).NE.SPACE)                                  DJG0995
     1               WORD(IND:IND)=KEY(ICOUNT:ICOUNT)                   DJG0995
      ICOUNT=ICOUNT+1                                                   DJG1094
      IF (KEY(ICOUNT:ICOUNT).NE.SPACE.AND.ICOUNT.NE.81) GOTO 10         DJG0995
      RETURN                                                            DJG1094
      END                                                               DJG1094
