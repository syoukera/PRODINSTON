!   modules
module main_variables
implicit none
!   grid parameter
    real*8,parameter   :: pai  = 3.1415926535d0
    integer, parameter :: nmax = 400            ! grid number [-]
    integer, parameter :: nsp = 10               ! number of chemical species [-]
    real*8 xscl(nmax), xvel(nmax)                ! grid position, xscl:scalar, xvel:velocity [m]
!
!   time parameter
    real*8, parameter :: delt_t   = 2.0e-7      ! delta t [s]
    real*8, parameter :: time_end = 5.0e-2     ! end time [s]
!
!   const parameter
    real*8, parameter :: pres0    = 0.1e6       ! static pressure [Pa]
    real*8, parameter :: gas_const= 8.3145      ! gas constant [J/mol K]
    real*8, parameter :: ave_mol_w= 28.8d-3     ! mean molecular weight of air [kg/mol]
!
!   variables
    real*8 vel(nmax)                             ! velocity [m/s]
    real*8 pres(nmax)                            ! pressure (fluid motion) [Pa]
    real*8 temp(nmax)                            ! teperature [K]
    real*8 dens(nmax)                            ! density [kg/m3]
    real*8 enth(nmax)                            ! enthalpy [J/kg]
    real*8 m_chsp(nmax,nsp)                      ! mass fraction of chemical species [-]
    real*8 x_mu(nmax)                            ! viscosity []
    real*8 x_D(nmax,nsp)                         ! diffusion coef. []
    real*8 T_D(nmax)                             ! temp. diffusion coef.[]
!
    real*8 o_vel(nmax)                           ! old value of velocity
    real*8 o_pres(nmax)                          ! old value of pressure 
    real*8 o_temp(nmax)                          ! old value of teperature 
    real*8 o_dens(nmax)                          ! old value of density
    real*8 o_enth(nmax)                          ! old value of enthalpy
    real*8 o_m_chsp(nmax,nsp)                    ! old value of mass fraction of chemical species
!
!   variable for flame position
    integer n_flame                              ! Index of flame position in current state
    integer, parameter :: n_flame_fix = nmax/2   ! Index of flame position should be fixed
    real*8, parameter :: temp_flame = 1000       ! Temperature set as flame position [K]
    integer :: n_disp = 0                        ! Count of displacement operation

    real*8 mf_chem(nsp), mb_phi1(nsp)
!
    ! reaction mechanism is h2o2.yaml
    ! upstream mixuture
    data mf_chem /2.8511094700E-02, 0.0000000000E+00, 0.0000000000E+00, 2.2627677800E-01, &
                  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00, &
                  0.0000000000E+00, 7.4521212700E-01 /
    ! burned mixture
    data mb_phi1 /1.1990037600E-03, 9.2745523600E-05, 5.8629498400E-04, 1.0785271500E-02, &
                  5.9656132200E-03, 2.3632841500E-01, 2.6111932600E-06, 3.0007698200E-07, &
                  0.0000000000E+00, 7.4503974400E-01 /
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
