! Fix flame at same position
subroutine fix_flame_pos()
    implicit none

    call get_flame_pos
    ! call calc_flame_vel
    call disp_flame_pos

end subroutine

subroutine get_flame_pos()
    use main_variables, only: nmax, xscl, temp, temp_flame, n_flame
    implicit none

    integer n

    do n = 1, nmax
        if (temp(n) < temp_flame) then
            n_flame = n
            exit
        endif

        if (n == nmax) then
            write (6, *) 'Error: Flame position was not found'
            stop
        endif
    enddo

    write (6,'("flame position = ",e12.4)') xscl(n_flame)

end subroutine

subroutine calc_flame_vel()
    implicit none

end subroutine

subroutine disp_flame_pos()
    use main_variables, only: nmax, nsp, n_flame, n_flame_fix, &
                              temp, vel, pres, dens, m_chsp, n_disp
    implicit none
    
    integer :: n, nn, i, n_diff
    real*8 mf_chem(nsp)
    
    n_diff = n_flame - n_flame_fix

    ! Mass fraction for upstream mixture
    ! This composition is same as used in initial_set
    data mf_chem  /2.85110947E-02, 0.00000000E+00, 2.26276778E-01, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 7.45212127E-01/


    ! Dixplace flame position to center when flame moved
    if (n_diff > 0) then

        ! Save solutions before displace
        n_disp = n_disp+1
        call data_output_disp()
        ! call log_output_disp(xtime, nl_file)

        ! asign upstream (larger index) value
        ! Discpace grid data for n_diff
        do n = 1, nmax-n_diff
            nn = n + n_diff

            temp(n) = temp(nn)
            vel(n) = vel(nn)
            pres(n) = pres(nn)
            dens(n) = dens(nn)
            do i = 1, nsp
                m_chsp(n, i) = m_chsp(nn, i)
            enddo
        enddo

        ! assign umbured mixture
        do n = nmax-n_diff+1, nmax
            temp(n) = 300.0d0
            do i = 1, nsp
                m_chsp(n, i) = mf_chem(i)
            enddo
        enddo
    endif

end subroutine

subroutine data_output_disp()
    !
        use main_variables
        integer n_file
        real*8 x_mm
        character*6 data_n, num2str
        n_file = 1000+n_disp
        data_n = num2str(n_disp)
    !
        open(unit=n_file, file='disp_data_'//data_n//'.csv' ,status='unknown')
        ! write (n_file,*) x_time
        write (n_file,'(30(A,","))') '  R(mm)','T(K)','mf(-)','mH2O(-)','v(m/s)','rho(kg/m3)','enthalpy(J/kg)'
        do n=1, nmax
            x_mm = xscl(n)*1.0d3
            write (n_file,'(30(E12.4,","))') x_mm, temp(n),m_chsp(n,1),m_chsp(n,8),vel(n),dens(n),enth(n)
        end do
        close(n_file)
    !
end subroutine