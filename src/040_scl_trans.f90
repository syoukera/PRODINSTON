subroutine scl_trans
!
    use main_variables
    real*8 a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)
    real*8 phi(nmax), S_i(nmax), Gamma(nmax), Beta(nmax)
    integer n_up
!   --------- n_up: upwind switch u_up=1 ----------------
    n_up = 2
!
    do i=1, nsp
        do n=1, nmax
            phi(n)   = o_m_chsp(n,i)
            S_i(n)   = 0.0d0
            Gamma(n) = dens(n)*x_D(n,i)
            Beta(n)  = charge(n)*dens(n)*mobility(n, i)*efield(n)
       end do
!
        call calc_scl_coef(phi, Gamma, Beta, S_i, a_i, b_i, c_i, d_i,n_up)
!
        call calc_tdma(phi, a_i, b_i, c_i, d_i)
!
        do n=1, nmax
            m_chsp(n,i) = phi(n)
        end do
    end do
!
end subroutine