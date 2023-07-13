subroutine vel_correct()
    ! get u from velocity correction equation

    use main_variables
    implicit none

    integer i

    ! inlet
    ! fixed velocity
    vel(1) = u_star(1)

    ! inner grid
    do i = 2, nmax-1
        ! u_e = u_star_e + d_e*(p_dash_P - p_dash_E)
        vel(n) = u_star(n) + d(n)*(p_dash(n) - p_dash(n+1))
    enddo

    ! outlet
    ! p_dash = 0 due to fixed pressure
    vel(nmax) = u_star(nmax)

end subroutine vel_correct