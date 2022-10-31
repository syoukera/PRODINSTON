subroutine enth_trans
!
    use main_variables
    real*8 a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)
    real*8 phi(nmax), S_i(nmax), Gamma(nmax)
    integer n_up
    real*8, parameter :: C_p = 1006.0      !!temporary specific heat   (N2@300K[J/kgK]) 
!   --------- n_up: upwind switch u_up=1 ----------------
    n_up = 2
!       
    do n=1, nmax
        phi(n) = temp(n)
        S_i(n) = 0.0d0
        Gamma(n) = T_D(n)
    end do
!
    call calc_scl_coef(phi, Gamma, S_i, a_i, b_i, c_i, d_i,n_up)
!
    call calc_tdma(phi, a_i, b_i, c_i, d_i)
!
    do n=1, nmax
        temp(n) = phi(n)
    end do
!
end subroutine