subroutine timestep_simple()

    use main_variables, only: p_star, p_dash, u_star, nmax
    implicit none

    integer n, k
    integer, parameter :: max_loop = 1000
    real*8, parameter :: torelance = 1.0e-8
    real*8 p_sum

    ! SIMPLE loop start
    do k = 1, max_loop

        ! step 1 
        ! estimate pressure
        do n = 1, nmax
            p_dash(n) = pres(n)
        end do

        print *, 'p_star: ', p_star

        ! step 2
        ! estimate velocity
        call estimate_vel()

        print *, 'u_star: ', u_star

        ! step 3
        ! correct pressure
        call correct_pres()

        print *, 'p_dash: ', p_dash

        ! step 4
        ! calculate pressure
        call calc_pres()

        print *, 'pres: ', pres

        ! step 5
        ! calculate velocity
        call calc_vel()

        print *, 'vel: ', vel

        ! step 6
        ! update temperatura and composition of chemical species
        ! and density

        ! === not impremented ===
        ! call calc_temp()
        ! call calc_chsp()

        ! update density
        call calc_dens()

        ! step 7
        ! check conversion
        do n = 1, nmax
            p_sum = p_sum + p_dash(n)
        end do

        ! exit SIMPLE loop when p_dash is small
        if (p_sum .le. torelance) then
            exit
        end if

    end do

    ! stop program when SIMPLE loop is not converged
    if (k == max_loop) then
        print *, 'SIMPLE loop is not converged in maximum number of loop: ', max_loop
        stop
    end if

end subroutine timestep_simple