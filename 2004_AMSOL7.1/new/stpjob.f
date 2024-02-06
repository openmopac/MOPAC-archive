      SUBROUTINE STPJOB                                                 DAL0303
      INCLUDE 'FFILES.i'
      WRITE (JOUT,10) IWHERE
 10   FORMAT('The submitted job was not completed successfully.',/,     GDH1095
     .       'The terminate signal was sent right after IWHERE was ',   GDH1095
     .       'set equal to ',I5,/,'in the source code.')                GDH1095
      STOP
      END
