subroutine timestep_simple

    use main_variables
    implicit none

    real*8 b_sum
    integer n

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
        do n = 2, nmax-1
            b_sum = b_sum + b_mass(n)
        enddo
        
        ! step 4
        ! get p (with relaxation 0.8)
        do n = 1, nmax
            pres(n) = p_star(n) + 0.8d0*p_dash(n)
        enddo

        ! step 5
        ! get u from correction equation of velocity
        call vel_correct()

        ! step 6
        ! update T and Y
        
        ! update sourche term T and Y by chemkin
        call chem_source()

        ! solve transport on T and Y
        call calc_trans_coef()
        call scl_trans()
        call enth_trans()
        call calc_dens

        ! step 7
        ! set p as p_star
        do n = 1, nmax
            p_star(n) = pres(n)
        end do

    enddo

end subroutine timestep_simple