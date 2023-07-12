subroutine timestep_simple

    implicit none

    ! SIMPLE algorism
    ! see Patankar p.130

    ! step 1
    ! estimate p_star (already done by step 7)
    ! p_star = p

    ! step 2
    ! get u_star from momentum equation
    call vel_trans()

    ! step 3
    ! get p_dash from eq. (6.22)
    ! pressure correction equation
    call pres_correct()

    ! check conversion
    call calc_mass_generation_term(b)
    if (b < 1.0d-8) then 
        exit
    endif 

    call get_p_dash(p_dash, u_star)

    ! step 4
    ! get p (with relaxation 0.8)
    p = p_dash + 0.8d0*p_star

    ! step 5
    ! get u from correction equation of velocity
    call get_u(u, p)

    ! step 6
    ! update other variables
    call update_temp()
    call update_chsp()

    ! step 7
    ! set p as p_star
    p_star = p

end subroutine timestep_simple