module namelist

    !use globals, only : kp, nx, ny, nz, &
    use globals, only : kp, ny, nz, &
                      & latdelta, pres, yrev, zrev, &
                      & VARS, REC_INI, nt, &
                      & ifile, ofile, &
                      & pressure

    implicit none

    private
    public :: read_nml


    contains


    subroutine read_nml()
        integer :: input
        character(128), parameter :: nml_file='../nml/input.nml'

        namelist / grid / ny, nz
        namelist / coordinate / latdelta, pressure, yrev, zrev
        namelist / record / VARS, REC_INI, nt
        namelist / files / ifile, ofile

        !nx = 0
        ny = 0
        nz = 0

        latdelta = 0
        pressure(1:100) = 0.
        yrev = 1                ! Default is True
        zrev = 0                ! Default is False

        VARS = 0
        REC_INI = 0
        nt = 0

        ifile = ''
        ofile = ''

        call open_nml(input, nml_file)
        
        read(input, nml=grid)
        read(input, nml=coordinate)
        read(input, nml=record)
        read(input, nml=files)

        close(input)

        call checker()

        !allocate(pres(nz))
        !pres(1:nz) = pressure(1:nz)

    end subroutine read_nml


    subroutine open_nml(unit, fname)
        integer     , intent(out) :: unit
        character(*), intent(in)  :: fname

        integer :: stat

        open(NEWUNIT=unit  , &  !! OUT
           & FILE   =fname , &  !! IN
           & ACTION ='read', &  !! IN
           & IOSTAT =stat    )  !! OUT

        if (stat/=0) then
            write(*,'(a)') 'IO ERROR-------------------------------------------------'
            write(*,'(a)') '|   Failed to open namelist file'
            write(*,'(a)') '|   FILE : ' // trim(fname)
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

    end subroutine open_nml


    subroutine checker()

        !if (nx <= 0) then
        !    write(*,'(a)')    'Value Error ---------------------------------------------'
        !    write(*,'(a,i0)') '|   nx must be 1 or more, but input is ', nx
        !    write(*,'(a)')    '---------------------------------------------------------'
        !    error stop
        !endif

        if (ny <= 0) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   ny must be 1 or more, but input is ', ny
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif
        
        if (nz <= 0) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   nz must be 1 or more, but input is ', nz
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif

        if (nz >= 100) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   nz must be less than 100, but input is ', nz
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif

        if (latdelta <= 0) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   latdelta must be 1 or more, but input is ', latdelta
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif

        if (pressure(1) > 1200. .OR. pressure(nz) > 1200.) then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   surface pressure is too large'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (pressure(1) <= 0. .OR. pressure(nz) <= 0.) then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   pressure at the top layer is less than 0'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (pressure(nz+1) /= 0.) then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   nz and pressure size are not matching'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (yrev /= 0 .AND. yrev /= 1) then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   unexpected yrev value. input must be 1 (True) or 0 (False)'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (zrev /= 0 .AND. zrev /= 1) then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   unexpected zrev value. input must be 1 (True) or 0 (False)'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (zrev /= 0 .AND. zrev /= 1) then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   unexpected zrev value. input must be 1 (True) or 0 (False)'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (VARS <= 0) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   VARS must be i or more, but input is ', VARS
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif

        if (REC_INI <= 0) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   REC_INI must be i or more, but input is ', REC_INI
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif

        if (nt <= 0) then
            write(*,'(a)')    'Value Error ---------------------------------------------'
            write(*,'(a,i0)') '|   nt must be i or more, but input is ', nt
            write(*,'(a)')    '---------------------------------------------------------'
            error stop
        endif

        if (ifile == '') then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   ifile is not specified'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

        if (ofile == '') then
            write(*,'(a)') 'Value Error ---------------------------------------------'
            write(*,'(a)') '|   ofile is not specified'
            write(*,'(a)') '---------------------------------------------------------'
            error stop
        endif

    end subroutine checker


end module namelist

