subroutine parkey (keywrd)
    use param_global_C, only : ifiles_8
    use reada_I
    implicit none
    character (len=*), intent (in) :: keywrd
    integer :: i, j
    intrinsic Index
     if (Index (keywrd, " MNDO ") /= 0) &
       write (ifiles_8, "(' *  MNDO       -  Default parameter set: MNDO')")
     if (Index (keywrd, " AM1") /= 0) &
       write (ifiles_8, "(' *  AM1        -  Default parameter set: AM1')")
     if (Index (keywrd, " PM3") /= 0) &
       write (ifiles_8, "(' *  PM3        -  Default parameter set: PM3')")
     if (Index (keywrd, " RM1") /= 0) &
       write (ifiles_8, "(' *  RM1        -  Default parameter set: RM1')")
     if (Index (keywrd, " PM6-DH") /= 0)  then
        write (ifiles_8, "(' *  PM6-DH2    -  Default parameter set: PM6-DH2')")
     else  if (Index (keywrd, " PM6") /= 0)  then
        write (ifiles_8, "(' *  PM6        -  Default parameter set: PM6')")
     end if
     if (Index (keywrd, " MNDOD") /= 0) &
       write (ifiles_8, "(' *  MNDOD      -  Default parameter set: MNDOD')")
     if (Index (keywrd, " PM7 ") /= 0 .or. Index (keywrd, " MNDO ") + Index (keywrd, " AM1") + &
       Index (keywrd, " PM3") + Index (keywrd, " PM6") + Index (keywrd, " RM1")== 0) &
       write (ifiles_8, "(' *  PM7        -  Default parameter set: PM7')")
     if (Index (keywrd, " PM5D ") /= 0) &
       write (ifiles_8, "(' *  PM5D       -  Default parameter set: PM5D')")
      if (Index (keywrd, "LARGE") /= 0) then
10000 format (" *  LARGE      -  GENERATE STANDARD MOPAC OUTPUT")
      write (ifiles_8, 10000)
    end if
    if (Index (keywrd, "SAVEG") /= 0) then
10010 format (" *  SAVEG      -  SAVE OPTIMIZED GEOMETRIES")
      write (ifiles_8, 10010)
    end if
    if (Index (keywrd, " NOSAVEP") /= 0) then
10011 format (" *  NOSAVEP    -  DO NOT SAVE OPTIMIZED PARAMETERS")
      write (ifiles_8, 10011)
    end if
    if (Index (keywrd, "FULL") /= 0) then
10021 format (" *  FULL       -  PRINT ALL DERIVATIVES")
      write (ifiles_8, 10021)
    end if
    if (Index (keywrd, "PARALLEL") /= 0) then
10022 format (" *  PARALLEL   -  COMBINE PARAMETERS FROM OTHER RUNNING PARAM JOBS")
      write (ifiles_8, 10022)
    end if
     if (Index (keywrd, "EXPORT") /= 0) &
     & write (ifiles_8, '(" *  EXPORT     -  REMOVE REFERENCE INFO FROM NEW DATA FILE")')
    if (Index (keywrd, "LET") /= 0) then
10040 format (" *  LET        -  CARRY OUT CALCULATION EVEN IF ", "SOME SYSTEMS &
     &ARE FAULTY")
      write (ifiles_8, 10040)
    end if
    if (Index(keywrd, " GNORM") /= 0) then
10041 format (" *  GNORM=     -  EXIT WHEN GRADIENT NORM DROPS BELOW ", g9.3)
      write (ifiles_8, 10041) reada (keywrd, Index (keywrd, " GNORM"))
    end if
    if (Index (keywrd, "CLEAN") /= 0) write(ifiles_8,'(a)') &
    & " * CLEAN       -  REMOVE DIAGNOSTIC KEYWORDS FROM REFERENCE DATA SETS"
    if (Index (keywrd, "ALL") /= 0) then
10042 format (" *  ALL        -  USE ALL REFERENCE DATA, INCLUDING SYSTEMS &
     &NOT BEING OPTIMIZED")
      write (ifiles_8, 10042)
    end if
    if (Index (keywrd, "ONLY") /= 0) then
10043 format (" *  ONLY       -  Use only compounds that contain elements being parameterized.")
      write (ifiles_8, 10043)
    end if
    if (Index (keywrd, "1SCF") /= 0) then
10050 format (" *  1SCF       -  GEOMETRIES ARE NOT TO BE OPTIMIZED ")
      write (ifiles_8, 10050)
    end if
    if (Index (keywrd, " PRECISE") /= 0) then
10055 format (" *  PRECISE    -  USE ""PRECISE"" IN MOPAC")
      write (ifiles_8, 10055)
    end if
    i = Index (keywrd, " PARAB=")
    if (i /= 0) then
10061 format (" *  PARAB=n    -  DAMPENING FACTOR OF",f13.2," USED")
      write (ifiles_8, 10061) Reada(keywrd,i)
    end if
     i = Index (keywrd, " POWER=")
    if (i /= 0) then
10063 format (" *  POWER=n    -  Power to be minimized",f5.2," USED")
      write (ifiles_8, 10063) Reada(keywrd,i)
    end if
    i = Index (keywrd, " RELH=")
    if (i /= 0) then
10051 format (" *  RELH=n     -  Relative weight for Heats of Formation:",f4.1)
      write (ifiles_8, 10051) Reada(keywrd,i)
    end if
    i = Index (keywrd, " RELD=")
    if (i /= 0) then
10052 format (" *  RELD=n     -  Relative weight for Dipole Moments:",f4.1)
      write (ifiles_8, 10052) Reada(keywrd,i)
    end if
     i = Index (keywrd, " RELI=")
    if (i /= 0) then
10053 format (" *  RELI=n     -  Relative weight for Ionization Potentials:",f4.1)
      write (ifiles_8, 10053) Reada(keywrd,i)
    end if
     i = Index (keywrd, " RELG=")
    if (i /= 0) then
10054 format (" *  RELG=n     -  Relative weight for Geometries:",f4.1)
      write (ifiles_8, 10054) Reada(keywrd,i)
    end if
    i = Index (keywrd, " CYCLES=")
    if (i /= 0) then
10062 format (" *  CYCLES=n   -  RUN A MAXIMUM OF",I5," CYCLES")
      write (ifiles_8, 10062) Nint(Reada(keywrd,i))
    end if
    i = Index (keywrd, " OUT=")
    if (i /= 0) then
      j = Index (keywrd(i+1:), " ") + i
10060 format (" *  OUT=n       -  WRITE OUTPUT TO DIRECTORY ",/16 x, a)
      write (ifiles_8, 10060) keywrd(i+5:j)
    end if
    i = Index (keywrd, " REF")
    if (i /= 0) then
      j = Index (keywrd(i+1:), " ") + i
10070 format (" *  REF=n       -  REFERENCE DATA ARE IN DIRECTORY ",/16 x, a)
      write (ifiles_8, 10070) keywrd(i+Index (keywrd(i:), "="):j)
    end if
    i = Index (keywrd, " PARAMS=")
    if (i /= 0) then
      j = Index (keywrd(i+1:), " ") + i
10071 format (" *  PARAMS=n    -  REFERENCE PARAMETERS TO BE READ FROM DIRECTORY ",/16 x, a)
      write (ifiles_8, 10071) keywrd(i+8:j)
    end if
    i = Index (keywrd, " NEW_REF=")
    if (i /= 0) then
      j = Index (keywrd(i+1:), " ") + i
10080 format (" *  NEW_RP=n    -  OPTIMIZED REFERENCE DATA ARE ", "WRITTEN TO DIR&
     &ECTORY ",/16 x, a)
      write (ifiles_8, 10080) keywrd(i+9:j)
    end if
    i = Index (keywrd, " NEW_REFP")
    if (i /= 0) then
      j = Index (keywrd(i+1:), " ") + i
10081 format (" *  NEW_RP=n    -  OPTIMIZED PARAMETERS ARE WRITTEN TO DIR&
     &ECTORY ",/16 x, a)
      write (ifiles_8, 10081) keywrd(i+14:j)
    end if
    if (Index (keywrd, "CHKPAR") /= 0) then
10090 format (" *  CHKPAR      -  CHECK THAT PARAMETER SET IS VALID")
      write (ifiles_8, 10090)
    end if
     if (Index (keywrd, " SURVEY") /= 0) then
10091 format (" *  SURVEY      -  GENERATE TABLES FOR STATISTICS, ETC")
      write (ifiles_8, 10091)
    end if
end subroutine parkey
