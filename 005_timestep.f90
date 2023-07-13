subroutine timestep_simple

    use main_variables
    implicit none

    real*8 b_sum
    integer i

    ! SIMPLE algorism
    ! see Patankar p.130

    ! set initial mass generation term
    b_sum = 1.0d0

    ! conversion loop 
    ! until mass generation term b => 0.0
    do while (b_sum < 1.0d-8)

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

        ! summation mass generation term
        b_sum = 0.0d0
        do i = 2, nmax-1
            b_sum = b_sum + b_mass(i)
        enddo
        
        ! step 4
        ! get p (with relaxation 0.8)
        p = p_star + 0.8d0*p_dash

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

    enddo

end subroutine timestep_simple