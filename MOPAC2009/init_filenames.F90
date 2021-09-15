 subroutine init_filenames
    use molkst_C, only: jobnam, gui
    use chanel_C, only : output_fn, restart_fn, brillouin_fn, &
     & density_fn, log_fn, end_fn, archive_fn, esp_fn, ump_fn, &
     mep_fn, pol_fn, gpt_fn, esr_fn, input_fn, xyz_fn, syb_fn, &
     cosmo_fn
    implicit none
    integer :: text_length
    text_length = len_trim (jobnam)
!
!  Set up the name of the files that are related to the input file name
!
    input_fn      = jobnam(1:text_length) // ".temp"
    output_fn     = jobnam(1:text_length) // ".out"
    restart_fn    = jobnam(1:text_length) // ".res"
    density_fn    = jobnam(1:text_length) // ".den"
    log_fn        = jobnam(1:text_length) // ".log"
    end_fn        = jobnam(1:text_length) // ".end"
    archive_fn    = jobnam(1:text_length) // ".arc"
    brillouin_fn  = jobnam(1:text_length) // ".brz"
    esp_fn        = jobnam(1:text_length) // ".esp"
    ump_fn        = jobnam(1:text_length) // ".ump"
    mep_fn        = jobnam(1:text_length) // ".mep"
    pol_fn        = jobnam(1:text_length) // ".pol"
    gpt_fn        = jobnam(1:text_length) // ".gpt"
    esr_fn        = jobnam(1:text_length) // ".esr"
    xyz_fn        = jobnam(1:text_length) // ".xyz"
    syb_fn        = jobnam(1:text_length) // ".syb"
    cosmo_fn      = jobnam(1:text_length) // ".cos"
    if (gui) output_fn     = "OUTPUT file"
  end subroutine init_filenames
