
subroutine calc_pres_coef(phi, Gamma, S_i, a_i, b_i, c_i, d_i,n_up)

    use main_variables
!
!    
    real*8, intent(in) :: phi(nmax), S_i(nmax),Gamma(nmax) 
    integer, intent(in) :: n_up
    real*8, intent(out) :: a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)

    real*8 F_E, F_P
    real*8 D_E, D_P
    real*8 Pe_E, Pe_P, APe_E, APe_P
    real*8 delt_x_E, delt_x_P
    real*8 S_E, S_P, S_sp, V_e
!
!   ------- n=1 center boundary condition ---------

        ! fixed velocity at inlet
        ! u_e = o_u_e
        ! coefficients for TDMA
        a_i(n) = 1.0d0
        b_i(n) = 0.0d0
        c_i(n) = 0.0d0
        d_i(n) = phi(n)
!    
!   ------- coef. calc. ---------
    do n=2, nmax-1

        delta_x_E = xvel(n+1) - xvel(n)
        delta_x_P = xvel(n)   - xvel(n-1)

        if (is_flat .eqv. .true.) then
            ! flat flame
            ! Control Volume is Quadrangular prism
            S_E  = 1.0d0 ! surface area of upper CV
            S_P  = 1.0d0 ! surface area of lower CV
            S_se = 1.0d0 ! surface area of center CV
            V_e  = xscl(n+1) - xscl(n) ! Volume of CV
        else 
            ! spherical flame
            ! Control Volume is Spherical shell
            S_E      = 4.0d0*pai*xscl(n+1)**2   ! area of upper surface of Control Volume
            S_P      = 4.0d0*pai*xscl(n)**2 ! area of lower surface of Control Volume
            S_se     = 4.0d0*pai*xvel(n)**2 ! area of center surface of Control Volume
            V_e      = (4.0d0/3.0d0)*pai*(xscl(n+1)**3-xscl(n)**3) ! volume of Control Volume
        endif

        F_E = dens(n+1)*0.5d0*(vel(n+1) + vel(n))*S_E ! dens_E*vel_E*S_E
        F_P = dens(n)  *0.5d0*(vel(n-1) + vel(n))*S_E ! dens_P*vel_P*S_P
        D_E = Gamma(n+1)/delta_x_E*S_E                ! Gamma_E/(delta_x_E)*S_E
        D_P = Gamma(n)  /delta_x_P*S_P                ! Gamma_P/(delta_x_P)*S_P

        ! Peclet number        
        Pe_E = F_E/D_E 
        Pe_P = F_P/D_P
        
        ! function A(|P|) for convection and diffusion
        ! see Patanker Table 5.2
        if (n_up.eq.1) then
            APe_E = 1.0d0
            APe_P = 1.0d0
        else if (n_up.eq.2) then
            APe_E = ddim(1.0d0-0.5d0*dabs(Pe_E),0.0d0)
            APe_P = ddim(1.0d0-0.5d0*dabs(Pe_P),0.0d0)
        else if (n_up.eq.3) then
            APe_E = ddim((1.0d0-0.1d0*dabs(Pe_E))**5,0.0d0)
            APe_P = ddim((1.0d0-0.1d0*dabs(Pe_P))**5,0.0d0)
        else
            APe_E = 1.0d0-0.5d0*dabs(Pe_E)
            APe_P = 1.0d0-0.5d0*dabs(Pe_P)
        end if

        ! coefficients of discretized form in CV
        ! see Patanakar 5.3-2
        a_f = D_E*APe_E+ddim(-F_E,0.0d0)
        a_p = D_P*APe_P+ddim(F_P,0.0d0)
        o_a_e = o_dens_e*V_e/delta_t
        b     = o_a_e*phi(n)
        a_e   = a_f + a_e + o_a_e
        
        ! coefficients for TDMA
        a_i(n) = a_e
        b_i(n) = a_f
        c_i(n) = a_p
        d_i(n) = b + (p_star(n) - p_star(n+1))*S_se ! b + (p_star_P - p_star_E)*S_se 
!
    end do
!
!   ------- n=nmax far field boundary condition ---------
!
    ! du/dx = 0
    ! i.e. u_e = u_p
    ! [caution] this is only work for flat flame (not spherical flame)
    b_i(nmax) = 0.0d0
    c_i(nmax) = 1.0d0
    a_i(nmax) = 1.0d0
    d_i(nmax) = 0.0d0
!!
end subroutine calc_pres_coef