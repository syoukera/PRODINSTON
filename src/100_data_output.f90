subroutine data_output(x_time, n_out)
!
    use main_variables
    integer n_out,n_file, i
    real*8 x_mm, x_time
    character*6 data_n, num2str
    n_file = 10+n_out
    data_n = num2str(n_out)
!
    open(unit=n_file, file='data_'//data_n//'.prm' ,status='unknown')
    write (n_file,*) x_time
    write (n_file, "(A)", advance='no') 'R(mm) T(K) v(m/s) rho(kg/m3) enthalpy(J/kg)'
    write (n_file, *) (i, i = 1, nsp)
    do n=1, nmax
        x_mm = xscl(n)*1.0d3
        write (n_file, *) x_mm, temp(n), vel(n),dens(n),enth(n), (m_chsp(n, i), i = 1, nsp)
    end do
    close(n_file)
!
end subroutine