************************************************************************
*   THIS FILE CONTAINS ALL THE ARRAY SIZES FOR USE IN THE PACKAGE.     *
*                                                                      *
*     MAXPTS = DEFAULT VALUE FOR THE MAXIMUM NUMBER OF POINTS ON A     *
*              SPHERE IN THE SURFACE AREA CALCULATION                  *
*     MXPTSV = DEFAULT VALUE FOR THE MAXIMUM NUMBER OF POINTS ON A     *
*              SPHERE IN THE SURFACE AREA CALCULATION WHERE ALL        *
*              POSSIBLE INFORMATION ON THE DISTANCE BETWEEN POINTS ON  *
*              EACH SPHERE AND THE SPHERES ITS CONNECTED TO WILL BE    *
*              SAVED.                                                  *
*     IATMSV = DEFAULT VALUE FOR THE MAXIMUM NUMBER OF ATOMS IN A      *
*              SYSTEM WHERE ALL POSSIBLE INFORMATION ON THE DISTANCE   *
*              BETWEEN POINTS ON EACH SPHERE AND THE SPHERES ITS       *
*              CONNECTED TO WILL BE SAVED.                             *
************************************************************************
*                                                                      *
************************************************************************
      PARAMETER (MXPT=3840)
      PARAMETER (MXPTSV=2620)
      PARAMETER (IATMSV=6)
      COMMON /PICOM/ PI,PD2,PD64
************************************************************************
