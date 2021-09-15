      FUNCTION SECOND()
      DOUBLE PRECISION SECOND
C******************************************************                 
C                                                                       
C   SECOND, ON EXIT, CONTAINS THE NUMBER OF CPU SECONDS                 
C   SINCE THE START OF THE CALCULATION.                                  
C                                                                       
C******************************************************                 
      LOGICAL FIRST, SET, SETOK
      DATA FIRST, SETOK   /  2 * .TRUE.    /
      IF(FIRST) THEN
          FIRST=.FALSE.
          CALL DEFINE_EVENT_FLAG_CLUSTER
      ENDIF
      CALL TIMCLK(CPU)                                                  
      CALL CHECK_EVENT_FLAG(64,SET)
      IF(SET)   THEN
      CPU=CPU+1.D6
      IF( SETOK) THEN
      WRITE(6,'(///10X,''****   JOB STOPPED BY OPERATOR   ****'')')
      SETOK=.FALSE.
      ENDIF
      ENDIF
C TIMCLK IS MACHINE-DEPENDENT. IF YOU DO NOT KNOW THE LOCAL CALL THEN   
C INSERT INSTEAD OF THIS LINE "      CPU=CPU+0.1D0"                     
      SECOND=CPU                                                        
      RETURN                                                            
      END                                                               

      SUBROUTINE TIMBGN ! NOTE BEGINNING OF TIMED INTERVAL              
C***********************************************************************
C                                                                       
C  TIMBGN IS A MACHINE-DEPENDENT SUBROUTINE.                            
C         IT WILL ONLY WORK ON A DIGITAL VAX COMPUTER                   
C         IT IS CALLED BY SUBROUTINE SECOND AND BY THE MAIN SEGMENT     
C                                                                       
C  **** <<<<<  WARNING! THIS CODE IS NOT MACHINE-INDEPENDENT!!!  >>>> **
C                                                                       
C***********************************************************************
C                                                                       
C       SAVE CURRENT PROCESS STATISTICS IN VARIABLES IN COMMON          
C       USAGE:                                                          
C            CALL TIMRB !START OF TIMED INTERVAL                        
C                                                                       
C       THIS SUBROUTINE ALSO DEFINES AND DATA-INITIALIZES LOGICAL       
C       UNITS WHICH WILL BE USED BY BENCHMARK PROGRAMS                  
C       ..THIS IS RETAINED FOR COMPATIBILITY WITH PDP-11 VERSION        
C       DEFINE AND INITIALIZE LOGICAL UNIT ASSIGNMENTS:                 
C          ICRD  - 1    DATA INPUT (IF REQUIRED)                        
C          ILPT  - 4    LINE PRINTER BULK OUTPUT                        
C          IKBD  - 5    KEYBOARD INPUT (IF REQUIRED)                    
C          ITTY  - 6    TTY OUTPUT, INCLUDING ELAPSED TIME INFORMATION  
C                                                                       
      COMMON /LUNS/ ICRD,ILPT,IKBD,ITTY                                 
      DATA          ICRD,ILPT,IKBD,ITTY / 1,4,5,6 /                     
C                                                                       
C                                                                       
      COMMON /STAT_VARS/ T0,BUFIO,CPUTIME,DIRIO,PFLTS                   
      INTEGER*4 BUFIO,CPUTIME,DIRIO,PFLTS                               
C                                                                       
      COMMON /JOB_PARAM/ LEN4A,BUFIO_CODE,BUFIO_ADR,ZERO,               
     2 LEN4B,CPUTIME_CODE,CPUTIME_ADR,ZERO1,                            
     2 LEN4C,DIRIO_CODE,DIRIO_ADR,ZERO2,                                
     2 LEN4D,PFLTS_CODE,PFLTS_ADR,ZERO3,                                
     2 ZERO4                                                            
      INTEGER*2 LEN4A,LEN4B,LEN4C,LEN4D                                 
      INTEGER*2 BUFIO_CODE,CPUTIME_CODE,DIRIO_CODE,PFLTS_CODE           
      INTEGER*4 BUFIO_ADR,CPUTIME_ADR,DIRIO_ADR,PFLTS_ADR               
      INTEGER*4 NEW_BUFIO,NEW_CPUTIME,NEW_DIRIO,NEW_PFLTS               
      INTEGER*4 ZERO,ZERO1,ZERO2,ZERO3,ZERO4,SYS$GETJPI                 
C                                                                       
C       **** NOTE THE FOLLOWING CODES ARE VMS SYMBOLLIC PARAMS.         
C       .... THEY MAY CHANGE IN FUTURE VERSIONS OF VMS...BEWARE!        
      DATA BUFIO_CODE /1036/          ! JPI$_BUFIO                      
      DATA CPUTIME_CODE /1031/        ! JPI$_CPUTIM                     
      DATA DIRIO_CODE /1035/          ! JPI$_DIRIO                      
      DATA PFLTS_CODE /1034/          ! JPI$_PAGEFLTS                   
      DATA LEN4A,LEN4B,LEN4C,LEN4D /4,4,4,4/                            
C                                                                       
C                                                                       
C       ==================================================              
      T0 = SECNDS(0.)                                                   
      BUFIO_ADR       = %LOC(BUFIO)                                     
      CPUTIME_ADR     = %LOC(CPUTIME)                                   
      DIRIO_ADR       = %LOC(DIRIO)                                     
      PFLTS_ADR       = %LOC(PFLTS)                                     
C                                                                       
      IF (.NOT. SYS$GETJPI(,,,LEN4A,,,)) THEN                           
          WRITE(ITTY,*) 'ERROR FROM SYS$GETJPI'                         
          ENDIF                                                         
C                                                                       
      RETURN                                                            
C                                                                       
      ENTRY TIMCLK(CPUSECS) ! PRINT EXECUTION STATISTICS FOR INTERVAL   
C                                                                       
C       USAGE:                                                          
C           CALL TIMRE   !END OF TIMED INTERVAL                         
C                                                                       
C       TIMRE OBTAINS PROCESS STATISTICS AND SUBTRACTS THE              
C       BEGINNING-OF-INTERVAL STATISTICS RECORDED BY TIMRB.             
C       THE INCREMENTAL VALUES ARE WRITTEN TO UNIT "TTY"                
C       (FORTRAN UNIT 6).                                               
      BUFIO_ADR       = %LOC(NEW_BUFIO)                                 
      CPUTIME_ADR     = %LOC(NEW_CPUTIME)                               
      DIRIO_ADR       = %LOC(NEW_DIRIO)                                 
      PFLTS_ADR       = %LOC(NEW_PFLTS)                                 
C                                                                       
      IF (.NOT. SYS$GETJPI(,,,LEN4A,,,)) THEN                           
          WRITE(ITTY,*) 'ERROR FROM SYS$GETJPI'                         
          ENDIF                                                         
C                                                                       
      CLKTIME = SECNDS(T0)                                              
C                                                                       
      CPUSECS = (NEW_CPUTIME-CPUTIME)/100.                              
      BUFIO = NEW_BUFIO - BUFIO                                         
      DIRIO = NEW_DIRIO - DIRIO                                         
      PFLTS = NEW_PFLTS - PFLTS                                         
C                                                                       
      RETURN                                                            
      END                                                               
