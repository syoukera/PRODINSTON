!   modules
module main_variables
implicit none
!   grid parameter
    real*8,parameter   :: pai  =3.1415926535d0
    integer, parameter :: nmax = 300            ! grid number [-]
    integer, parameter :: nsp = 53              ! number of chemical species [-]
    real*8 xscl(nmax), xvel(nmax)                ! grid position, xscl:scalar, xvel:velocity [m]
!
!   time parameter
    real*8, parameter :: delt_t   = 2.0e-6      ! delta t [s]
    real*8, parameter :: time_end = 5.0e-2     ! end time [s]
!
!   const parameter
    real*8, parameter :: pres0    = 0.1e6       ! static pressure [Pa]
    real*8, parameter :: gas_const= 8.3145      ! gas constant [J/mol K]
    real*8, parameter :: ave_mol_w= 28.8d-3     ! mean molecular weight of air [kg/mol]
!
!   flag parameter
    logical, parameter :: is_flat = .true.        ! flag for solving flat flame or spherical flame
    logical, parameter :: is_ion  = .false.       ! flag for using gri30_ion.yaml, not impremented.
    logical, parameter :: use_simple  = .false.    ! flag for using simple method, else use continuas eq.
    logical, parameter :: start_from_csv = .true. ! flag for importing csv file as initial conditions
    logical, parameter :: make_output = .false.   ! flag for output in chemkin
!
!   variables
    real*8 vel(nmax)                             ! velocity [m/s]
    real*8 pres(nmax)                            ! pressure (fluid motion) [Pa]
    real*8 temp(nmax)                            ! teperature [K]
    real*8 dens(nmax)                            ! density [kg/m3]
    real*8 enth(nmax)                            ! enthalpy [J/kg]
    real*8 eField(nmax)                          ! electric field [V/m]
    real*8 m_chsp(nmax,nsp)                      ! mass fraction of chemical species [-]
    real*8 x_mu(nmax)                            ! viscosity []
    real*8 x_D(nmax,nsp)                         ! diffusion coef. []
    real*8 T_D(nmax)                             ! temp. diffusion coef.[]
!
!   variable for SIMPLE method
    real*8 p_star(nmax)                          ! estimated pressure
    real*8 p_dash(nmax)                          ! correction of pressure
    real*8 u_star(nmax)                          ! estimated velocity
    real*8 a_moment(nmax)                        ! coefficient for discretised momentum eq.
!
!   array for old value
    real*8 o_vel(nmax)                           ! old value of velocity
    real*8 o_pres(nmax)                          ! old value of pressure 
    real*8 o_temp(nmax)                          ! old value of teperature 
    real*8 o_dens(nmax)                          ! old value of density
    real*8 o_enth(nmax)                          ! old value of enthalpy
    real*8 o_m_chsp(nmax,nsp)                    ! old value of mass fraction of chemical species
!
! 
end module

     CHARACTER(6) FUNCTION NUM2STR(NUM)
		! �X�e�b�v����6���̕�����ɕϊ��i����0�p�f�B���O�j
	    IMPLICIT NONE
      INTEGER,INTENT(IN) :: NUM
      CHARACTER*6 STR
      INTEGER I
      !////--------------------------------------------------------------------
      WRITE(STR,'(I6)') NUM
      DO I=1,6
				IF( STR(I:I) == ' ' ) STR(I:I) = '0'
      END DO
      NUM2STR = STR
	    !--------------------------------------------------------------------////
    END FUNCTION
