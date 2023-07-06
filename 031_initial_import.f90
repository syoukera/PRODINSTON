
subroutine initial_import()

    use main_variables
    implicit none
        
    ! paramter    
    ! integer, parameter :: nmax = 300     ! number of grid in csv file\
    ! logical, parameter :: is_ion  = .false.  ! flag for using gri30_ion.yaml
    
    ! integer ncsv     ! number of columns in csv file
    ! integer nsp ! number of species
    integer i, j
    
    ! ! array for reading values
    ! real*8 xscl(nmax)
    ! real*8 vel(nmax)
    ! real*8 pres(nmax)
    ! real*8 temp(nmax)
    ! real*8 dens(nmax)
    ! real*8 eField(nmax)
    ! real*8, allocatable :: m_chsp(:, :)

    ! if (is_ion .eqv. .true.) then
    !     ! gri30_ion.yaml
    !     ! ncsv = 61 ! number of columns in csv file
    !     nsp = 56 ! number of species
    ! else
    !     ! gri30.yaml
    !     ! ncsv = 57     ! number of columns in csv file
    !     nsp = 53 ! number of species
    ! endif
    
    ! allocate(m_chsp(nmax, nsp))
    
    ! open csv file containing result of cantera
    ! you should select same grid number as nmax
    open (17, file='cantera_csv/adiabatic_flame_300.csv', status='old')
    
    ! skip header
    read (17, '()')
    
    ! print each data
    do i = 1, nmax
        
        ! read row and asign values
        if (is_ion .eqv. .true.) then
            ! gri30_ion.yaml
            read (17, *) xscl(i), vel(i), eField(i), temp(i), dens(i), &
                    (m_chsp(i, j), j = 1, nsp)
        else
            ! gri30.yaml
            read (17, *) xscl(i), vel(i), temp(i), dens(i), &
                    (m_chsp(i, j), j = 1, nsp)
        endif
            
        ! print *, xscl(i), vel(i), temp(i), dens(i), m_chsp(i, 7)
    end do
    
    close (17)

end subroutine initial_import
    
    