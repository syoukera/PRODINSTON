!   modules
module main_variables
implicit none
!   grid parameter
    real*8,parameter   :: pai  = 3.1415926535d0
    integer, parameter :: nmax = 600            ! grid number [-]
    integer, parameter :: nsp = 53               ! number of chemical species [-]
    real*8 xscl(nmax), xvel(nmax)                ! grid position, xscl:scalar, xvel:velocity [m]
!
!   time parameter
    real*8, parameter :: delt_t   = 1.0e-6      ! delta t [s]
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

    real*8 mf_unburned(nsp), mf_burned(nsp)
!
    ! reaction mechanism is gri30.yaml
    ! upstream mixuture
    data mf_unburned /0.        , 0.        , 0.        , 0.22014124, 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        , 0.05518667, 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        , 0.        , 0.        ,    &
                      0.        , 0.        , 0.7246721 , 0.        , 0.        ,    &
                      0.        , 0.        , 0.        /
    ! ! burned mixture
    data mf_burned  /2.64932581E-04, 1.43452452E-05, 1.25793069E-04, 5.39227215E-03, &
                     1.78288712E-03, 1.20500265E-01, 6.00376158E-07, 5.67125637E-08, &
                     9.71180935E-18, 1.59555568E-18, 5.02571430E-18, 3.04608038E-19, &
                     3.41145270E-17, 1.77398836E-17, 9.17846312E-03, 1.36966419E-01, &
                     8.41043518E-10, 1.44445291E-11, 4.65488752E-17, 7.43935046E-19, &
                     4.08041020E-18, 3.35009377E-24, 8.68302176E-22, 6.82498098E-27, &
                     7.39489517E-27, 8.31073511E-32, 6.24580899E-33, 8.02741893E-20, &
                     1.08906155E-19, 1.06798925E-22, 7.27471824E-09, 1.29177002E-09, &
                     5.49656091E-10, 1.66811114E-09, 7.99479254E-10, 2.06563774E-03, &
                     5.80807076E-07, 1.60764701E-07, 3.80639442E-08, 6.42564400E-14, &
                     1.90231439E-11, 5.59834484E-18, 1.96970448E-21, 1.60031945E-16, &
                     1.77435287E-12, 6.06212168E-10, 2.36496173E-11, 7.23707535E-01, &
                     0.00000000E+00, 5.56268815E-47, 3.90410854E-48, 4.63408110E-25, &
                     9.40332180E-26/
    !
    ! ! reaction mechanism is gri30_ion.yaml
    ! ! upstream mixuture
    ! data mf_unburned /0.000000000E+00, 2.201834862E-01, 0.000000000E+00, 0.000000000E+00, 5.504587156E-02, &
    !               7.247706422E-01, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00/
    ! ! burned mixture
    ! data mf_burned /1.238532110E-01, 0.000000000E+00, 0.000000000E+00, 1.513761468E-01, 0.000000000E+00, &
    !               7.247706422E-01, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, 0.000000000E+00, &
    !               0.000000000E+00/
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
