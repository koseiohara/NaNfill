module globals

    implicit none


    integer, parameter :: kp = 4

    !integer, save :: nx
    integer, save :: ny
    integer, save :: nz

    real(kp), save :: latdelta
    real(kp), save, allocatable :: pres(:)
    integer, save :: yrev
    integer, save :: zrev

    integer, save :: VARS
    integer, save :: REC_INI
    integer, save :: nt

    character(128), save :: ifile
    character(128), save :: ofile

end module globals

