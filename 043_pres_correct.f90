subroutine pres_correct
    !
        use main_variables

        real*8 a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)
        real*8 phi(nmax), S_i(nmax), Gamma(nmax)
        integer n_up
    !   --------- n_up: upwind switch u_up=1 ----------------
        n_up = 2
    !
        do n=1, nmax
            phi(n)   = vel(n)
            S_i(n)   = 0.0d0
            Gamma(n) = dens(n)*x_mu(n,i)
        end do
!
        call calc_vel_coef(phi, Gamma, S_i, a_i, b_i, c_i, d_i,n_up)
!
        call calc_tdma(phi, a_i, b_i, c_i, d_i)
!
        do n=1, nmax
            u_star(n,i) = phi(n)
        end do
    !
end subroutine pres_correct