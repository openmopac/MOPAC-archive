module molkst_C
  USE vast_kind_param, ONLY:  double 
!
!  This module contains all the scalars relating to the system being calculated.
!  Every entry is unique, and has the same meaning in every subroutine.
!
!
!  This module can also be regarded as a dictionary of the scalars relating to the system.
!  Quantities which have only a one-line descriptionis are of no interest outside MOPAC.
!  The data-type for each quantity is given at the start of the statement
!
  integer :: &
  &  maxatoms, & !  Maximum number of atoms allowed = number of lines in the data set
                 !
  &  natoms,   & !  Term        Number of atoms, real plus dummy, etc., in the system
                 !  Units       Atoms
                 !  Min. value  0
                 !  Max. value  very large
                 !
  &  numat = 0,& !  Term        Number of real atoms in the system
                 !  Units       Atoms
                 !  Min. value  0
                 !  Max. value  very large
                 !
  &  norbs,    & !  Term        Number of atomic orbitals in the system
                 !  Units       atomic orbitals
                 !  Min. value  0
                 !  Max. value  very large
                 !
  &  nelecs,   & !  Term        Number of electrons
                 !  Units       Electrons
                 !  Min. value  0
                 !  Max. value  2*norbs    
                 !             
  &  ispd,     & !  0 if a s-p basis set, 1 or more if any atoms have d-orbitals
  &  ndep = 0, & !  Number of dependent coordinates, related by symmetry
  &  nvar = 0, & !  Number of coordinate that are to be optimized
                 !
  &  nclose,   & !  Term        Number of doubly-occupied M.O.s
                 !  Units       Molecular orbitals
                 !  Min. value  0
                 !  Max. value  nelecs/2  
                 !
  &  nopen,    & !  Term        Upper bound of active space in ROHF - C.I.
                 !  Units       Molecular orbitals
                 !  Definition  nclose + number of M.O.s in the active space.
                 !  Min. value  0
                 !  Max. value  norbs     
                 !  Default     nclose
                 !  
  &  nalpha,   & !  Term        Number of alpha singly-occupied M.O.s
                 !  Units       Molecular orbitals
                 !  Min. value  0
                 !  Max. value  min(norbs, nelecs)
                 !  Default     0 (RHF) (nelecs + 1)/2 (UHF)
                 !
  &  nbeta,    & !  Term        Number of beta singly-occupied M.O.s
                 !  Units       Molecular orbitals
                 !  Min. value  0
                 !  Max. value  min(norbs, nalpha - nelecs)
                 !  Default     0 (RHF) nelecs/2 (UHF)
                 !
  &nalpha_open,& !  Term        Number of alpha fractionally-occupied M.O.s
                 !  Units       Molecular orbitals
                 !  Min. value  0
                 !  Max. value  norbs
                 !  Default     nalpha
                 !
  & nbeta_open,& !  Term        Number of beta fractionally-occupied M.O.s
                 !  Units       Molecular orbitals
                 !  Min. value  0
                 !  Max. value  norbs
                 !  Default     nbeta
                 !
  &  numcal=0, & !  Number of the calculation.  The first molecule calculated will have
                 !  numcal = 1.  The second molecule calculated will have numcal = 2, and so on.
                 !
   step_num=0, & !  The unique number of the stage or step within a calculation of a molecule.
                 !  The first stage of the first calculation will have step_num = 1. 
                 !  If that calculation involves a second stage, then step_num will be incremented.
                 !  When a new calculation starts, step_num will be incremented.
                 !  Each stage is limited to the same electronic structure.  Most calculations
                 !  will only have one stage, e.g. geometry optimization or force constants.
                 !
  &  mpack,    & !  Number of elements in a lower-half-triangle = (norbs*(norbs+1))/2
  &  n2elec,   & !  Number of two-electron integrals
  &  nscf,     & !  Number of SCF calculations done
  &  iscf,     & !  Index of message of how the SCF ended
  &  iflepo,   & !  Index of message of how the geometry operation ended
  &  maxtxt,   & !  Maximum number of characters in labeled atoms (e.g. C(on Si))
  &  last,     & !  
  &  na1,      & !  99 if coordinates must be Cartesian, 0 otherwise
  &  lm61,     & !
                 !
  &  no_pKa,   & !  Number of ionizable hydrogen atoms used in pKa calculation
  &  id,       & !  Term        Dimensionality of system
                 !  Definition  Number of infinite dimensions of the system
                 !  Units       none
                 !  Min. value  0  (A discrete molecule or ion)
                 !  Max. value  3  (A regular solid)
                 !
  &  l1u,      & !  Number of unit cells in the first dimension used in solid-state work (0 for a molecule)
  &  l2u,      & !  Number of unit cells in the second dimension (0 for a molecule or polymer)
  &  l3u,      & !  Number of unit cells in the third dimension (0 for a molecule, polymer, or layer system)
  &  l123,     & !  Total number of unit cells (=1 for a molecule = (2*l1u + 1)*(2*l2u + 1)*(2*l3u + 1))
  &  l11,      & !  1 if l1u > 0, 0 otherwise
  &  l21,      & !  1 if l2u > 0, 0 otherwise
  &  l31,      & !  1 if l3u > 0, 0 otherwise
  &  msdel,    & !  Magnetic component of spin
  &  Run = 1,  & !  Run-number (=1 if a single job)
  &  ijulian   & !  Number of days that this program has left before it stops working
  &  = 100000, & !  Be generous
  &  site_no,  & !  The number of this site
  &  ncomments,& !  Number of lines of comment
  &  mers(3),  & !  Number of mers in each direction, in solids
  &  itemp_1,  & !  Used for very temporary transfer of information
  &  itemp_2,  & !  Used for very temporary transfer of information
  &  lpars,    & !  Number of parameters read from parameter files
  &  n_screen, & !  Used to pass information about type of output to "to_screen"
  &  nl_atoms, & !  number of atoms to be printed.
  &  old_chrge,& !  Net charge from the previous calculation, if it exists.
  &  dummy
  real(double) ::  &
  &  escf,         & !  Term        Heat of formation at 298 K
                     !  Units       kcal/mole
                     !  Unit type   Heat of Formation in kcal/mol
                     !  Min. value  ~-20000
                     !  Max. value  ~+20000
                     !
  &  emin,         & !  Lowest heat of formation calculated.
                     !
  &  elect,        & !  Term        Electronic energy 
                     !  Units       eV
                     !  Definition  Total electronic energy of the system in electron volts
                     !  Min. value  Very negative
                     !  Max. value  0.0
                     !
  &  enuclr,       & !  Term        Nuclear energy
                     !  Units       eV
                     !  Definition  Total core-core repulsion energy in eV
                     !  Min. value  0.0
                     !  Max. value  Very large
                     ! 
  &  fract,        & !  Term        Fractional occupancy of M.O.s in active space
                     !  Units       Electrons
                     !  Definition  Number of electrons in active space divided by number of M.O.s in active space
                     !  Min. value  0.0
                     !  Max. value  2.0
                     !  Default     0.0
                     !
  &  gnorm,        & !  Term        Scalar of gradient vector
                     !  Units       kcal/mol/(Angstrom - Radians)
                     !  Definition  The square root of the sum of the squares of the gradient components
                     !
  &  mol_weight,   & !  Term        Molecular weight
                     !  Units       Atomic mass units (Hydrogen = 1.00794 on this scale)
                     !  Min. value  0.0
                     !  Max. value  Very large
                     !
  &  time0 = 0.d0, & !  The start of time, or when the job began (seconds)
  &  tleft,        & !  Number of seconds the job has left before running out of time
  &  tdump = 0.d0, & !  Time between checkpoint dumps (seconds)
  &  cosine,       & !  Angle between previous and current gradient vectors
  &  ux, uy, uz,   & !  Dipole components (?)
  &  step,         & !  "step" in SADDLE calculation
  &  rjkab1,       & !  "J" and "K" integrals for correction of doublet I.P.
  &  atheat,       & !
                     !
  &  efield(3),    & !  Term        Electric field
                     !  Definition  Applied external electric field components in X, Y, and Z.
                     !  Units       Atomic units per Bohr
                     !  Min. value  Very negative
                     !  Max. value  Very positive
                     !  Default     0.0, 0.0, 0.0
                     !
  &  cutofp,       & !  Cutoff distance for NDDO approximation in solid state 
                     !  Default     10**(10) for molecules, 30 for polymers, layers, solids
  &  clower,       & !  Lower bound for transition to point charges in solid-state 
                     !  Default     13 Angstroms
  &  cupper,       & !  Upper bound for transition to point charges in solid-state 
                     !  Default     cutofp
                     !
  &  pressure,     & !  Term        Pressure or stress for solid-state and polymer work
                     !  Definition  Applied isotropic pressure (for solids) or pull (for polymers)
                     !  Units       Pascals (Newtons per square meter) (solids) or Newtons per Mole (polymers)
                     !  Default     0.0
  &  zpe,          & !  Term        Zero point energy
                     !  Definition  Half the addition of the vibrational energies
                     !  Units       kcal/mol
  &  density,      & !  Term        Density of solid
                     !  Definition  Weight of cluster divided by its volume
                     !  Units       grams per cubic centimeter
  &  stress,       & !  Term        Energy due to distortion from reference geometry, used with GEO-REF=
                     !  Units       Kcal/mol
                     !
  &  press(3),     & ! Type         Pressure on crystal faces
                     ! Definition   Pressure required to stop crystal expanding
                     ! Units        Gigapascals
  &  E_disp,       & ! Dispersion term
  &  E_hb,         & ! Hydrogen-bond term
  &  Rab,          & ! Distance between two atom, calculated in "connected"
  &  temp_1,       & !  Used for very temporary transfer of information
  &  temp_2,       & !  Used for very temporary transfer of information
  &  sz,           & ! Spin component
  &  ss2,          & ! Total spin
  param_constant,  & ! Used by PARAM only
   trunc_1,        &
   trunc_2,        &
  &  rdummy
!
   character ::     &
  & ltxt,          & !
  & formula*100,   & !  Type          Empirical formula
                     !  Definition    Type and number count of each element in the system
                     !  Units         Text
 &verson*7= "00.000X"!  Term         Version number
                     !  Definition   Version number for this copy of MOPAC
                     !  Pattern      "\d\.\d\d\d[X|W|L]"
                     !  Description  Year.Julian date. Operating System [X = placeholder]
  character ::     &
  & jobnam*240 = ' ', &!
  & line*400         !
  character (len=241) :: keywrd, koment, title, refkey(6)
  character :: errtxt*200, program_name*17="GUI             ", dh*4
!
  logical ::            &
     moperr,            & !
     Academic = .true., & !  TRUE is this is an academic licence.
     uhf,               & !  Term        Should the Unrestricted Hartree Fock method be used?
                          !  Definition  True if the calculation is Unrestricted Hartree-Fock
                          !  Default    = .false.
     rhf,               & !  Term        Should the Restricted Hartree Fock method be used?
                          !  Definition  True if the calculation is Restricted Hartree-Fock
                          !  Default    = .false.
     isok,              & !
     mozyme,            & !  TRUE if any of the MOZYME functions are to be used 
     limscf,            & !  Convergence criterion for SCF: if TRUE, then exit the SCF
                          !  if the energy changes a lot (useful in geometry optimization)
                          !  if FALSE, then converge the SCF to the default criterion
     gui = .true.,      & !  By default, output information for a Graphical User Interface
     in_house_only,     & !  TRUE only if run at Stewart Computational Chemistry. Used in debugging, etc.
     lxfac,             & !  TRUE if a diatomic is being used to define the values of XFAC and ALPB
     MM_corrections(20),& !  
     N_3_present,       & !
     Si_O_H_present,    & !
     dummy_present,     & !
     is_PARAM=.false.     !  This will be set "TRUE" in a PARAM run
  equivalence  &
    (MM_corrections(1), N_3_present),    & ! TRUE if the system contains at least one N bonded to three ligands
                                           ! and at least two are not hydrogen atoms
    (MM_corrections(2), Si_O_H_present), & ! TRUE if the system contains at least one Si-O-H group
    (MM_corrections(20), dummy_present)    ! If needed, then increase the size of the MM_corrections array.
!
  logical ::      &
       & method_mndo,          &
       & method_am1,           &
       & method_pm3,           &
       & method_pm6,           &
       & method_pm6_dh_plus,   &
       & method_pm6_dh2,       &
       & method_rm1,           &
       & method_mndod,         &
       & method_pm7_ts,        &
       & method_pm7,           &
       & method_pm6_plus,      &
       & method             !  Default method = PM6

end module molkst_C
