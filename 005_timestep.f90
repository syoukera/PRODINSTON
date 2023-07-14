subroutine timestep_simple

    use main_variables
    implicit none

    real*8 b_sum
    integer n, j, n_out

    ! SIMPLE algorism
    ! see Patankar p.130

    ! set initial mass generation term
    b_sum = 1.0d0
    
    ! set initial value of output index
    n_out = 0
    
    print *, "SIMPLE Loop start"

    ! conversion loop 
    ! until mass generation term b => 0.0
    do while (b_sum > 1.0d-8)

        ! step 1
        print *, "step 1"
        
        ! estimate p_star (already done by step 7)
        do n = 1, nmax
            p_star(n) = pres(n)
        end do
        print *, "p_star: ", p_star
    
        call calc_dens
        print *, "dens: ", dens

        ! step 2
        print *, "step 2"
        ! get u_star from momentum equation
        call vel_trans()
        print *, "u_star: ", u_star
        

        ! step 3
        print *, "step 3"
        ! get p_dash from eq. (6.22)
        ! pressure correction equation
        call pres_correct()
        print *, "p_dash: ", p_dash

        ! summation mass generation term
        b_sum = 0.0d0
        do n = 2, nmax-1
            b_sum = b_sum + abs(b_mass(n))
        enddo
        print *, "b = ", b_sum
        
        ! step 4
        print *, "step 4"
        ! get p (with relaxation 0.8)
        do n = 1, nmax
            pres(n) = p_star(n) + 0.8d0*p_dash(n)
        enddo

        ! step 5
        print *, "step 5"
        ! get u from correction equation of velocity
        call vel_correct()
        print *, "u: ", vel

        ! step 6
        print *, "step 6"
        ! update T and Y
        
        ! update sourche term T and Y by chemkin
        call chem_source()
        print *, "Y: ", m_chsp 
        print *, "T: ", temp

        ! solve transport on T and Y
        call calc_trans_coef()
        call scl_trans()
        print *, "Y: ", m_chsp 
        call enth_trans()
        print *, "T: ", temp

        ! call calc_dens

        ! step 7
        ! set p as p_star
        ! do n = 1, nmax
        !     p_star(n) = pres(n)
        ! end do
        
        ! output
        n_out = n_out+1
        call simple_output(n_out)

    enddo

end subroutine timestep_simple