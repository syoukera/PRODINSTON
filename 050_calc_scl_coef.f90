subroutine calc_scl_coef(phi, Gamma, S_i, a_i, b_i, c_i, d_i,n_up)

    use main_variables
!
!
    real*8 F_e, F_w
    real*8 D_e, D_w
    real*8 Pe_e, Pe_w, APe_e, APe_w
    real*8 delt_x_e, delt_x_w, delt_xc_e, delt_xc_w
    real*8 a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)
    real*8 phi(nmax), S_i(nmax),Gamma(nmax)
    real*8 Gamma_e, Gamma_w, S_e, S_w, V_p
    integer n_up
!
!   ------- n=1 center boundary condition ---------
    Gamma_e  = xvel(1)/xscl(2)*(Gamma(2)-Gamma(1))+Gamma(1)
    
    if (is_flat .eqv. .true.) then
        ! flat flame
        ! Control Volume is Quadrangular prism
        S_p      = 1.0d0
        S_e      = 1.0d0
        V_p      = xvel(1)
    else
        ! spherical flame
        ! Control Volume is Spherical shell
        S_p      = 0.0d0
        S_e      = 4.0d0*pai*xvel(1)**2
        V_p      = (4.0d0/3.0d0)*pai*xvel(1)**3
    endif
!
    F_p = dens(1)                                      *vel(1)*S_p ! temporary use vel(1) for vel_p
    F_e = ((xvel(1)/xscl(2))*(dens(2)-dens(1))+dens(1))*vel(1)*S_e
    D_e = Gamma_e/xvel(1)                                     *S_e
!
    a_i(1) = (dens(1)/delt_t)*V_p + D_e + 0.5d0*F_e - F_p
    b_i(1) = D_e-0.5d0*F_e
    c_i(1) = 0.0d0
    d_i(1) = (phi(1)*o_dens(1)/delt_t)*V_p   
!
!    
!   ------- coef. calc. ---------
    do n=2, nmax-1
!
        delt_x_e = xscl(n+1)-xscl(n)
        delt_x_w = xscl(n)-xscl(n-1)
        delt_xc_e= xvel(n)-xscl(n)
        delt_xc_w= xscl(n)-xvel(n-1)
        Gamma_e  = (delt_xc_e/delt_x_e)*(Gamma(n+1)-Gamma(n))+Gamma(n)
        Gamma_w  = (delt_xc_w/delt_x_w)*(Gamma(n)-Gamma(n-1))+Gamma(n-1)

        if (is_flat .eqv. .true.) then
            ! flat flame
            ! Control Volume is Quadrangular prism
            S_e      = 1.0d0 ! area of upper surface of Control Volume
            S_w      = 1.0d0 ! area of lower surface of Control Volume
            V_p      = xvel(n) - xvel(n-1) ! volume of Control Volume
        else 
            ! spherical flame
            ! Control Volume is Spherical shell
            S_e      = 4.0d0*pai*xvel(n)**2   ! area of upper surface of Control Volume
            S_w      = 4.0d0*pai*xvel(n-1)**2 ! area of lower surface of Control Volume
            V_p      = (4.0d0/3.0d0)*pai*(xvel(n)**3-xvel(n-1)**3) ! volume of Control Volume
        endif
!
        F_e = ((delt_xc_e/delt_x_e)*(dens(n+1)-dens(n))+dens(n))  *vel(n)    *S_e
        F_w = ((delt_xc_w/delt_x_w)*(dens(n)-dens(n-1))+dens(n-1))*vel(n-1)  *S_w
        D_e = Gamma_e/delt_x_e                                               *S_e
        D_w = Gamma_w/delt_x_w                                               *S_w
        Pe_e =(F_e/S_e)*(xvel(n)-xvel(n-1))/Gamma_e
        Pe_w =(F_w/S_w)*(xvel(n)-xvel(n-1))/Gamma_w
!
        if (n_up.eq.1) then
            APe_e = 1.0d0
            APe_w = 1.0d0
        else if (n_up.eq.2) then
            APe_e = ddim(1.0d0-0.5d0*dabs(Pe_e),0.0d0)
            APe_w = ddim(1.0d0-0.5d0*dabs(Pe_w),0.0d0)
        else if (n_up.eq.3) then
            APe_e = ddim((1.0d0-0.1d0*dabs(Pe_e))**5,0.0d0)
            APe_w = ddim((1.0d0-0.1d0*dabs(Pe_w))**5,0.0d0)
        else
            APe_e = 1.0d0-0.5d0*dabs(Pe_e)
            APe_w = 1.0d0-0.5d0*dabs(Pe_w)
        end if
        b_i(n) = D_e*APe_e+ddim(-F_e,0.0d0)
        c_i(n) = D_w*APe_w+ddim(F_w,0.0d0)
        a_i(n) = (dens(n)/delt_t)*V_p+b_i(n)+c_i(n)+(F_e-F_w)
        d_i(n) = (phi(n)*o_dens(n)/delt_t)*V_p
!
    end do
!
!   ------- n=nmax far field boundary condition ---------
!
    if (is_flat .eqv. .true.) then
        ! flat flame
        V_p      = xvel(nmax) - xvel(nmax-1) ! volume of Control Volume
    else
        ! spherical flame
        V_p      = (4.0d0/3.0d0)*pai*(xvel(nmax)**3-xvel(nmax-1)**3) ! volume of Control Volume
    endif
    b_i(nmax) = 0.0d0
    c_i(nmax) = 0.0d0
    a_i(nmax) = (dens(nmax)/delt_t)*V_p
    d_i(nmax) = (phi(nmax)*o_dens(nmax)/delt_t)*V_p
!!
end subroutine