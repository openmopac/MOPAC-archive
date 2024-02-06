************************************************************************
*   THIS FILE CONTAINS ALL THE ARRAY SIZES FOR USE IN THE PACKAGE.     *
*                                                                      *
*   THERE ARE TWO MAIN PARAMETERS THAT THE PROGRAMMER NEED SET:        *
*     MAXHEV = MAXIMUM NUMBER OF HEAVY ATOMS (HEAVY: NON-HYDROGEN ATOMS)
*     MAXLIT = MAXIMUM NUMBER OF HYDROGEN ATOMS.                       *
*   ONE AUXILIARY PARAMETER FOR THE CHAIN METHOD:                      *
*     NCHAIN = MAXIMUM NUMBER OF POINTS ALONG THE PATH                 *
*   AND THREE AUXILIARY PARAMETERS REQUIRED FOR C.I. AND GRADIENT:     *
*     NMECI  = MAXIMUM NUMBER OF ACTIVE ORBITALS IN C.I.               *
*     MAXCI  = MAXIMUM NUMBER OF MICROSTATES IN C.I.                   *DL0397
*     NRELAX = MAXIMUM NUMBER OF MATRIX (A.O.*A.O.) ALLOWED IN         *
*             1) ANALYTICAL GRADIENT COMPUTATION           AND         *
*             2) INCONDITIONAL CONVERGERS IN SCF ITERATION.            *
*     NMECI SHOULD NOT BE LOWER THAN TWO (HOMO-LUMO FULL C.I.),        *
*     NRELAX  CANNOT   BE LOWER THAN TEN (CAMP-KING CONVERGER).        *
*     TDEF   = DEFAULT VALUE FOR THE MAXIMUM AMOUNT OF TIME (IN SECONDS)
*              ALLOWED FOR DIFFERENT CALCULATIONS (E.G. SCF CALCULATION)
*                                                                      *
      PARAMETER (MAXHEV=60, MAXLIT=60, NCHAIN=60, NRELAX=25)            DL0397
      PARAMETER (NMECI=30, MAXCI=451)                                   DL0397
*                                                                      *
************************************************************************
*                                                                      *
*   ALL OTHER PARAMETERS ARE DERIVED OF THE TWO 'MAIN' PARAMETERS      *
*                                                                      *
*      NAME                   DEFINITION                               *
*     NUMATM         MAXIMUM NUMBER OF ATOMS ALLOWED.                  *
*     MAXORB         MAXIMUM NUMBER OF ORBITALS ALLOWED.               *
*     MAXPAR         MAXIMUM NUMBER OF PARAMETERS FOR OPTIMISATION.    *
*     N2ELEC         MAXIMUM NUMBER OF TWO ELECTRON INTEGRALS ALLOWED. *
*     MPACK          LOWER HALF TRIANGLE OF MATRICES OF ORDER MAXORB.  *
*     MORB2          SQUARE MATRIX OF ORDER MAXORB                     *
*     MAXHES         LOWER HALF TRIANGLE OF MATRICES OF ORDER MAXPAR.  *
*     MAXLOW         MAXHES - THE NUMBER OF DIAGONAL ELEMENTS          *
************************************************************************
      PARAMETER (VERSON=0.D0)
      PARAMETER (NUMATM=MAXHEV+MAXLIT)
      PARAMETER (MAXORB=4*MAXHEV+MAXLIT)
      PARAMETER (MAXPAR=3*NUMATM)
      PARAMETER (N2ELEC=50*MAXHEV*(MAXHEV-1)+10*MAXHEV*MAXLIT
     +                     +MAXLIT*(MAXLIT-1)/2)
      PARAMETER (NPACK =(NUMATM*(NUMATM+1))/2)
      PARAMETER (MPACK =(MAXORB*(MAXORB+1))/2)
      PARAMETER (MORB2 =(MAXORB*MAXORB))
      PARAMETER (MAXHES=(MAXPAR*(MAXPAR+1))/2)
      PARAMETER (MAXLOW=(MAXHES-MAXPAR))
      PARAMETER (TDEF = 65000)
***********************************************************************
*  NOTICE: THE PARAMETER MAXSIZ SHOULD BE SET SO IT SATISFIES THE FOLLOWING
*  CONDITION:MAXSIZ=MAX(MAXORB,MAXPAR,MAXCI)
      PARAMETER (MAXSIZ=451)                                            GDH0597
************************************************************************
