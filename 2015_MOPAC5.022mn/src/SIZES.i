C
C  SIZES.i
C
C  This file is based on the MOPAC include file SIZES.
C
************************************************************************
*   THIS FILE CONTAINS ALL THE ARRAY SIZES FOR USE IN MOPAC.            
*                                                                       
*     THERE ARE ONLY  PARAMETERS THAT THE PROGRAMMER NEED SET:          
*     MAXHEV = MAXIMUM NUMBER OF HEAVY ATOMS (HEAVY: NON-HYDROGEN ATOMS)
*     MAXLIT = MAXIMUM NUMBER OF HYDROGEN ATOMS.                        
*     MAXTIM = DEFAULT TIME FOR A JOB. (SECONDS)                        
*     MAXDMP = DEFAULT TIME FOR AUTOMATIC RESTART FILE GENERATION (SECS)
*     MXATSP = MAXIMUM NUMBER OF ATOMIC SPECIES IN THE SYSTEM           IR0494
*     NMECI  = MAXIMUM NUMBER OF ACTIVE ORBITALS IN C.I.               *
*     MAXCI  = MAXIMUM NUMBER OF MICROSTATES IN C.I.                   *DL0397
*     NRELAX = MAXIMUM NUMBER OF MATRIX (A.O.*A.O.) ALLOWED IN         *
*             1) ANALYTICAL GRADIENT COMPUTATION           AND         *
*             2) INCONDITIONAL CONVERGERS IN SCF ITERATION.            *
*     NMECI SHOULD NOT BE LOWER THAN TWO (HOMO-LUMO FULL C.I.),        *
*     NRELAX  CANNOT   BE LOWER THAN TEN (CAMP-KING CONVERGER).        *
*                                                                       
C     PARAMETER (MAXHEV=200,   MAXLIT=400)                              LF0608
      PARAMETER (MAXHEV=60,    MAXLIT=60)   
      PARAMETER (MAXTIM=3600, MAXDMP=36000)                             PF1099
      PARAMETER (MXATSP=10)                                             IR0494
*                                                                       
************************************************************************
*                                                                       
*   THE FOLLOWING CODE DOES NOT NEED TO BE ALTERED BY THE PROGRAMMER    
*                                                                       
************************************************************************
*                                                                       
*    ALL OTHER PARAMETERS ARE DERIVED FUNCTIONS OF THESE TWO PARAMETERS 
*                                                                       
*      NAME                   DEFINITION                                
*     NUMATM         MAXIMUM NUMBER OF ATOMS ALLOWED.                   
*     MAXORB         MAXIMUM NUMBER OF ORBITALS ALLOWED.                
*     MAXPAR         MAXIMUM NUMBER OF PARAMETERS FOR OPTIMISATION.     
*     N2ELEC         MAXIMUM NUMBER OF TWO ELECTRON INTEGRALS ALLOWED.  
*     MPACK          AREA OF LOWER HALF TRIANGLE OF DENSITY MATRIX.     
*     MORB2          SQUARE OF THE MAXIMUM NUMBER OF ORBITALS ALLOWED.  
*     MAXHES         AREA OF HESSIAN MATRIX                             
*     MAXDMO         MAXIMUM DIAGONALIZABLE MATRIX ORDER                IR0494
*     MXSRPB         MAXIMUM NUMBER OF SRP SPECIAL BETA ALLOWED         IR0494
************************************************************************
      PARAMETER (VERSON=5.022D0)                                        LF0610
      PARAMETER (NUMATM=MAXHEV+MAXLIT)
      PARAMETER (MAXORB=4*MAXHEV+MAXLIT)
      PARAMETER (MAXPAR=3*NUMATM)
      PARAMETER (MAXBIG=MAXORB*MAXORB*2)
      PARAMETER (N2ELEC=2*(50*MAXHEV*(MAXHEV-1)+10*MAXHEV*MAXLIT
     +                     +(MAXLIT*(MAXLIT-1))/2))
      PARAMETER (MAXHES=(MAXPAR*(MAXPAR+1))/2,MORB2=MAXORB**2)
      PARAMETER (MPACK=(MAXORB*(MAXORB+1))/2)
      PARAMETER (MAXBET=(MXATSP*(MXATSP+1))/2)                          IR0494
************************************************************************
*   FOR SHORT VERSION USE LINE WITH NMECI=1, FOR LONG VERSION USE LINE  
*   WITH NMECI=10                                                       
************************************************************************
C     PARAMETER (NMECI=10,  NPULAY=MPACK)
C     PARAMETER (NMECI=1,   NPULAY=1)
C     PARAMETER (NMECI=10,  NPULAY=MPACK)
      PARAMETER (NRELAX=25)                                              JZ0315
      PARAMETER (NMECI=30, MAXCI=451, NPULAY=MPACK)                      JZ0315
C WARNING : MAXDMO have to be >=  max(MAXORB,NMECI**2)                   IR0494
      PARAMETER (MAXDMO=1200)                                            LF0608
C MAXDMO made large enough for handling system of 200 heavy atoms and    LF0608
C 400 light atoms (enough for 200 water molecules).                      LF0608
************************************************************************
***********************************************************************
*  NOTICE: THE PARAMETER MAXSIZ SHOULD BE SET SO IT SATISFIES THE FOLLOWING
*  CONDITION:MAXSIZ=MAX(MAXORB,MAXPAR,MAXCI)
      PARAMETER (MAXSIZ=451)                                            GDH0597
************************************************************************

