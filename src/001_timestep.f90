subroutine timestep(ntime)
    ! proceed timestep
    ! solve equation of continuity 
    ! not solve momentum equation

    implicit none
    integer, intent(in) :: ntime
    
    call old_value_st
!
!       -- chemical kinetics calculation --
    call chem_source_cantera(ntime)
!
!       -- calculatoin of density change --
    call calc_dens
!
!       -- calculation of velocity change according to density change --
    call calc_vel_cont
!
!       -- calculation of scalar transport --
    call calc_trans_coef_cantera
    call scl_trans
    call enth_trans
    call calc_dens

!       -- set flame at same position --
    ! call fix_flame_pos(xtime)
end subroutine

! subroutine timestep_simple()
!     ! proceed timestep using simple method
!     ! solve equation of continuity and momentum equation

!     implicit none

!     real(8) :: p(nmax)      ! pressure
!     real(8) :: p_old(nmax)  ! pressure at previous time step
!     real(8) :: p_star(nmax) ! estimated pressure
!     real(8) :: p_dash(nmax) ! correction value for pressure (p = p_star + p_dash)
!     real(8) :: u(nmax)      ! velocity
!     real(8) :: u_star(nmax) ! estimated velocity

!     integer :: i_iter = 1   ! index of loop
!     integer, parameter :: num_iter = 1000 ! maximum number of loop

!     ! save pressure at previous time step
!     p_old = p

!     ! iterration of SIMPLE method
!     do while (i_iter < num_iter)

!         ! step 1
!         ! estimate p_star from previous reuslt
!         if (i_iter == 1) then
!             ! first loop
!             ! asign pressure at previous time step (p_old) as estimated pressure (p_star)
!             p_star = p_old
!         else
!             ! after first loop
!             ! set p as next initial pressure
!             p_star = p
!         endif

!         ! step 2
!         ! estimate u_stara from p_star
!         ! solve momentum equation
!         call est_u_star(p_star, u_star)

!         ! step 3
!         ! calculate correction value for pressure (p_dash)
!         ! solve equation drived from equation of continuity
!         call calc_p_dash(u_star, p_dash)

!         ! step 4
!         ! calculate pressure (p)
!         p = p_star + p_dash

!         ! step 5
!         ! calculate velocity (u)
!         ! from correction equation of velocity
!         ! u_e = u_star_e + d_e*(p_dash_P - p_dash_E)
!         call calc_u(p_dash, u) 
!         ! <should it be p?>

!         ! step 6
!         ! calculate temperature and species composition
!         call calc_T()
!         call calc_Y()
!         ! <How to couple T and Y to velocity?>

!         ! step 7
!         ! check convergence (if p_dash = 0.0)
!         if (p_dash == 0.0d0) then
!             ! exit from iteration
!             exit
!         endif

!     enddo

!     ! proceed time step
!     time = time + dt 
! end subroutine timestep_simple
