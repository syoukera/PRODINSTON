subroutine pres_correct()
    ! get p_dash from eq. (6.22)
    ! pressure correction equation
    
    use main_variables

    real*8 a_i(nmax), b_i(nmax), c_i(nmax), d_i(nmax)
    real*8 phi(nmax)

    do n=1, nmax
        phi(n)   = p_star(n)
    end do
!
    call calc_pres_coef(a_i, b_i, c_i, d_i)
!
    call calc_tdma(phi, a_i, b_i, c_i, d_i)
!
    do n=1, nmax
        p_dash(n,i) = phi(n)
    end do
    !
end subroutine pres_correct