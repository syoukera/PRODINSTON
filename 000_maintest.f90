! test program for spherical flame propagation
!
!
    use main_variables
    use chemkin_params, only: initialize_chemkin_workarray
    use output, only: make_output
    real*8 xtime
    real*8 before_chk(nmax), after_chk(nmax)
    real*8 x_residue, x_limit, x_max, x_dump
    integer out_step, log_step
    integer n_simple_max
    integer int_time, n_cont, n_out, ntest, nl_file
!
!   -------- chemkin data open -----------
    make_output = .false.
    call initialize_chemkin_workarray()
!
!   -------- data in out ----------
    out_step  = 200
    x_dump    = 0.3d0
!
!   -------- log file -------------
    log_step  = 20
    nl_file   = 4
!   -------- initial setting ------------
    call grid_set
    call initial_set
    n_simple_max = 200
    x_limit = 1.0d-6
!
    read (5,*) n_cont
!
!   -------- time loop --------------
    call data_output(0.0d0,0)
!
    if (n_cont.eq.1) then
        call cont_input(xtime,n_out)
    else
        xtime = 0.0d0
        n_out  = 0
    end if
!
    do ntime = 1, 9999999
        xtime = xtime+delt_t
!
        call old_value_st
!
!       -- chemical kinetics calculation --
        call chem_source(ntime)
!
!       -- calculatoin of density change --
        call calc_dens
!
!       -- calculation of velocity change according to density change --
        call calc_vel_cont
!
!       -- calculation of scalar transport --
        call calc_trans_coef
        call scl_trans
        call enth_trans
        call calc_dens
!
        write (6,'("time = ",e12.4)') xtime

!       -- set flame at same position --
        call fix_flame_pos
!
!       -- data output --
!
        if (mod(ntime,out_step).eq.0) then
            n_out = n_out+1
            call data_output(xtime, n_out)
        end if
        if (mod(ntime,log_step).eq.0) then
            call log_output(xtime,nl_file)
        end if
!
        call cont_output(xtime,n_out)
!
        if (xtime.ge.time_end) exit
    end do
    close (nl_file)
!
    end
   