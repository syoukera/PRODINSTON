
subroutine calc_pres_coef(a_i, b_i, c_i, d_i)

    use main_variables
!
!    
    real*8, intent(out) :: a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)

    real*8 S_e, S_w, S_p, V_p
    real*8 dens_e, dens_w
    real*8 d_e, d_w
    real*8 a_E, a_W, a_P, b
!
!   ------- n=1 center boundary condition ---------

    ! fixed pressure at inlet
    ! p_P = o_p_P
    ! i.e. p_dash_P = 0.0
    ! coefficients for TDMA
    a_i(1) = 1.0d0
    b_i(1) = 0.0d0
    c_i(1) = 0.0d0
    d_i(1) = 0.0d0
!    
!   ------- coef. calc. ---------
    do n=2, nmax-1

        if (is_flat .eqv. .true.) then
            ! flat flame
            ! Control Volume is Quadrangular prism
            S_e      = 1.0d0 ! area of upper surface of Control Volume
            S_w      = 1.0d0 ! area of lower surface of Control Volume
            S_p      = 1.0d0 ! area of center surface of Control Volume
            V_p      = xvel(n) - xvel(n-1) ! volume of Control Volume
        else 
            ! spherical flame
            ! Control Volume is Spherical shell
            S_e      = 4.0d0*pai*xvel(n)**2   ! area of upper surface of Control Volume
            S_w      = 4.0d0*pai*xvel(n-1)**2 ! area of lower surface of Control Volume
            S_p      = 4.0d0*pai*xscl(n)**2   ! area of center surface of Control Volume
            V_p      = (4.0d0/3.0d0)*pai*(xvel(n)**3-xvel(n-1)**3) ! volume of Control Volume
        endif

        dens_e = 0.5d0*(dens(n) + dens(n+1))
        dens_w = 0.5d0*(dens(n) + dens(n-1))

        ! See Patankar eq.(6.16)
        d_e = S_e/a_moment(n)   ! S_e/a_moment_e
        d_w = S_w/a_moment(n-1) ! S_w/a_moment_w

        ! coefficients of discretized form in CV
        ! see Patanakar 5.3-2
        a_E = dens_e*d_e*S_e ! dens_e*d_e*delta_y_e
        a_W = dens_w*d_w*S_w ! dens_w*d_w*delta_y_w
        a_P = a_E + a_W
        ! b = (o_dens_P - dens_P)*delta_x*delta_y/delta_t &
        !   + (dens_w*u_star_w - dens_e*u_star_e)*delta_y
        b = (o_dens(n) - dens(n))*V_p/delta_t &
          + (dens_w*u_star(n-1) - dens_e*u_star(n))*S_p

        ! coefficients for TDMA
        a_i(n) = a_P
        b_i(n) = a_E
        c_i(n) = a_W
        d_i(n) = b
!
    end do
!
!   ------- n=nmax far field boundary condition ---------
!
    ! fixed pressure at outlet
    ! p_P = o_p_P
    ! i.e. p_dash_P = 0.0
    ! coefficients for TDMA
    a_i(nmax) = 1.0d0
    b_i(nmax) = 0.0d0
    c_i(nmax) = 0.0d0
    d_i(nmax) = 0.0d0
!!
end subroutine calc_pres_coef