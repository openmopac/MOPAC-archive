C File for common block /METHOD/ declaration.

C IMPORTANT NOTE! The subroutine ANALYT does not use this as an include
C file as the global variable name "ANALYT" conflicts with the subroutine
C name.  Any changes occuring here should be paralleled with changes to
C the /METHOD/ common block declaration in the ANALYT subroutine!
C Also the subroutine DCART that calls the ANALYT subroutine has a name
C conflict, so the following section is duplicated in the DCART subroutine's
C declaration section.  
C
C *** Be sure to make any changes here to both the
C      ANALYT and DCART subroutines!!

      COMMON /METHOD/ LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,      LF0909
     &                LPM3D,LPM6,AM1PM3,ANALYT,                         LF0909
     &                LPM6G9,                                           LF0110
     &                LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,                 LF0310
     &                LPMOV1,LPMOV1A,                                   LF1010
     &                LMNDOD3,LAM1D3,D3METH,                            LF1111
     &                LPM3D3,LRM1D3,LPM6D3,                             LF1211
     &                LHGDAMP,LTDAMP,LHGDISP,LTDISP,LDAMP5,             LF0312
     &                LPMO2,                                            LF0113
     &                LPMO2A                                            LF0614
      LOGICAL  LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,LPM3D,LPM6,  LF0909
     &         AM1PM3,ANALYT,LPM6G9,LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,   LF0310
     &         LPMOV1,LPMOV1A,                                          LF1010
     &         LMNDOD3,LAM1D3,D3METH,                                   LF1111
     &         LPM3D3,LRM1D3,LPM6D3,                                    LF1211
     &         LHGDAMP,LTDAMP,LHGDISP,LTDISP,LDAMP5,                    LF0312
     &         LPMO2,                                                   LF0113
     &         LPMO2A                                                   LF0614


C Explanation of global variables:
C
C   LMINDO   = true if MINDO keyword is present.
C   LMNDO    = true if the keyword for all other methods are absent.
C   LAM1     = true if AM1 keyword is present (not AM1-D keyword).
C   LPM3     = true if PM3 keyword is present (not PM3-D keyword).
C   LRM1     = true if RM1 keyword is present (not RM1-D keyword).
C   LMDG     = true if MDG keyword is present (for PDDG/MNDO calculation).
C   LPDG     = true if PDG keyword is present (for PDDG/PM3 calculation).
C   LAM1D    = true if AM1-D keyword is present.
C   LPM3D    = true if PM3-D keyword is present.
C   LPM6     = true if PM6 or PM6G09 keyword is present (not PM6-D nor PM6G09-D).
C   AM1PM3   = true for any non-MNDO, non-MNDO-D methods: (for including
C              AM1-type gaussians in the nuclear energy between atom pairs)
C              AM1, PM3, RM1, PDG, PM6, AM1-D, PM3-D, RM1-D, and PM6-D.
C   ANALYT   = true if the ANALYT keyword is present (for analytical derivatives).
C   LPM6G9   = true if the PM6G09 or PM6G09-D keyword is present.
C   LMNDOD   = true if the MNDO-D keyword is present.
C   LSDAMP   = true if the SDAMP keyword is present.
C   LRM1D    = true if the RM1-D keyword is present.
C   LPM6D    = true if the PM6-D or PM6G09-D keyword is present.
C   DSPMET   = true if any -D dispersion method is present:
C                   MNDO-D, AM1-D, PM3-D, RM1-D, PM6-D, PM6G09-D.
C   LPMOV1   = true if PMOV1 or PMOV1A keyword is present.
C   LPMOV1A  = true if PMOV1A keyword is present.
C   LMNDOD3  = true if MNDO-D3 keyword is present.
C   LAM1D3   = true is AM1-D3 keyword is present.
C   D3METH   = true for any methods using -D3 dispersion (e.g. MNDO-D3, AM1-D3).
C   LPM3D3   = true if PM3-D3 keyword is present.
C   LRM1D3   = true is RM1-D3 keyword is present.
C   LPM6D3   = true is PM6-D3 keyword is present.
C   LHGDAMP  = uses the Head-Gordon damping function for dispersion.
C   LTDAMP   = uses the (R**p0)/(R**p1+d**p2)**p3 damping function for dispersion.
C   LHGDISP  = uses the Head-Gordon dispersion function.
C   LTDISP   = uses the -((C_6)/(R**6))*f_damp(r) dispersion function.
C   LDAMP5   = uses the R**6*(1-exp(-(R/d)**8)) damping function.
C   LPMO2    = true if PMO2 keyword is present.
C   LPMO2A   = true if PMO2A keyword is present.
C
C
C



