module MOZYME_C 
!
!  This module holds all the data that is specific to the MOZYME Localized
!  Molecular Orbital (LMO) procedure.
!
    implicit none
!
!                               Data for the system
! 
  integer :: &
    noccupied,   & ! Number of occupied molecular orbitals
    nvirtual,    & ! Number of virtual molecular orbitals
    nres,        & ! Number of amino-acid residues in a polypeptide or protein
    morb,        & ! Maximum number of orbitals on any atom, usually 4 or 9
    ipad2,       & ! Estimate of the average number of atoms in a LMO
    ipad4,       & ! Estimate of the average number of atomic orbital coefficients in a LMO
    Lewis_tot      ! Total number of Lewis elements (occupied plus virtual)
!
!                              Data on atoms
!
  integer, allocatable, dimension (:) :: &
    iorbs,       &
    jopt 
!
!                              Data on diatomic interactions
!
  integer ::     &
    ij_dim         ! Number of interactions treated by NDDO approximations
                   ! = size of arrays iijj and ijall
  logical :: &
    lijbo          ! TRUE if array ijbo is to be used, 
                   ! FALSE if the ijbo function is to be used
  integer, dimension (:), allocatable :: &
    iijj,        & !
    iii,         &
    iij,         &
    numij,       &
    ijall          !
  integer, allocatable, dimension (:,:) :: &
    nijbo          ! If it exists, nijbo holds the statring address of array elements
                   ! for atoms i and j in otomic orbital arrays such as P, H, F, etc.
!
!  Protein-specific data
!
  integer, parameter :: &
    maxres = 2000    !  Maximum number of residues allowed in a protein

  integer, dimension(:), allocatable :: &    
                  !
                  ! The unique residue number of atom i is at_res(i)
    at_res,     & ! This number is independent of the residue number printed
                  ! and is only used inside MOPAC - it is never printed out
    iz,         & ! Number of valence electrons available for Lewis structure, on each atom 
    ib,         & ! Number of valence orbitals available for Lewis structure, on each atom 
    ions          ! Charge on ionized atoms, cations = 1 neutral = 0
   
                  !
  integer, dimension(:,:), allocatable :: &
    Lewis_elem       ! Atoms and atom pairs involved in making the Lewis structure
  integer ::     &
    iatom,       & ! Used in working out residue names
    jatom,       & ! Used in working out residue names
    mxeno,       & ! Used in working out residue names
    k,           &
    loop,        &     
    icharges       ! Number of charged sites
   integer :: &
    nxeno(4,11), & ! Up to 10 xeno groups allowed. nxeno(1,*) = number of C, 
                   ! 2 = No. on N, 3 = #O, 4 = #S
    nbackb(4),   & ! for each peptide line, nbackb(1) = atom number of C of CHR,
                   ! 2 = # of C of C=O, 3 = # of O of C=O, 4 = # of N
                        !
                        ! For each entry in in_res, res_start contains the atom
    res_start(maxres),& ! number of the first atom in the residue. It is
                        ! only used inside MOPAC - it is never printed out
                        !
    bbone(3,0:maxres)! Atom numbers of the backbone atoms in a polypeptide

   double precision :: &
    angles(3, maxres) !  Phi, Psi, and Omega for backbone angles (Ramachandran)


   character (len=4), dimension (-2:maxres) :: &
     allres        ! Names of all the residues

   integer, parameter :: size_mres = 23
   character (len=3) :: &
     tyres(size_mres)

   character (len=40), dimension (11) :: &
     txeno         ! Names of xeno groups

!
!   Arrays that hold the localized molecular orbitals
!
!                               Occupied set
!
  integer ::     &
    cocc_dim,    & ! Size of the array cocc
    icocc_dim      ! Size of the array icocc
  integer, dimension (:), allocatable :: &
    ncf,         & ! Number of atoms involved in the LMO
    nncf,        & !  Starting address of the atom numbers of the atoms in the LMO
                   !  nncf(1) = 0
    icocc,       & ! Atom numbers of the atoms in the LMO's
    ncocc          ! Starting address of te atomic orbital coefficients in each LMO
  double precision, dimension (:), allocatable :: &
    cocc           ! Atomic orbital coefficients of the LMO's
!
!                                Virtual set
!
  integer ::     &
    cvir_dim,    & ! Size of the array cvir
    icvir_dim      ! Size of the array icvir
  integer, dimension (:), allocatable :: &
    nce,         & ! Number of atoms involved in the LMO
    nnce,        & ! Starting address of the atom numbers of the atoms in the LMO
                   ! nncf(1) = 0
    icvir,       & ! Atom numbers of the atoms in the LMO's
    ncvir          ! Starting address of te atomic orbital coefficients in each LMO
  double precision, dimension (:), allocatable :: &
    cvir           ! Atomic orbital coefficients of the LMO's
!
!                            Data for SCF and diagonalization
!
  integer, dimension (:,:), allocatable :: &
    ifmo           !
  integer, dimension (:), allocatable :: &
    idiag,       & !
  
    nfmo           ! Number of filled LMO's that interact with a virtual LMO     
  double precision, dimension (:), allocatable :: &    
    partf,       & ! 
    p1,          & !
    p2,          & !
    p3,          & !
    fmo,         & !
    partp,       & !
    parth,       & !
    ws             !      
       
!
  integer ::     &
    nelred,      & ! Number of electrons to be used in the SCF during a geometry optimization.  
                   ! In the first SCF, numred = nelecs
    norred,      & ! Number of atomic orbitals to be used in the SCF during a geometry optimization
                   ! In the first SCF, norred = norbs
    numred,      & ! Number of atoms to be used in the SCF during a geometry optimization
                   ! In the first SCF, numred = numat
    fmo_dim,     & ! Size of arrays holding the occupied M.O. - virtual M.O. interactions
    mode,        & !  0 for a simple MOZYME, 
                   ! -1 if depleted arrays are to be constructed (RAPID only)
                   ! +1 if arrays are to be built using depleted arrays (RAPID only)
    ijc
  double precision, allocatable, dimension (:,:) ::  &
     part_dxyz    
  double precision :: &
    cutofs,      & ! Overlap cut-off distance (normally 7 A)
    thresh,      & ! Criterion for deciding to do Euler rotation to annihilate
                   ! LMO occupied - virtual interaction
    energy_diff, &
    tiny,        &
    sumt,        &
    sumb,        &
    shift,       &
    pmax,        &
    ovmax,       &
    scfref,      &
    refnuc,      &
    refout,      &
    unused
  logical :: &
    rapid,       &  !  True if RAPID technique to be used
    use_three_point_extrap
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!                    Delete or move everything below here
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    logical ::     &
      direct,      &
      lredop,      &
      semidr

    integer, allocatable, dimension (:) :: &
      isort,       &
      kopt,        &      
      iopt   
    data tyres / "GLY", "ALA", "VAL", "LEU", "ILE", "SER", "THR", "ASP", &
         & "ASN", "LYS", "GLU", "GLN", "ARG", "HIS", "PHE", "CYS", "TRP", &
         & "TYR", "MET", "PRO", "PRO", "PRO", "UNK" /   
    save
end module MOZYME_C
