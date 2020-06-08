subroutine data_output(x_time, n_out)
!
    use main_variables
    integer n_out,n_file
    real*8 x_mm, x_time
    character*6 data_n, num2str
    nfile = 10+n_out
    data_n = num2str(n_out)
!
    open(unit=n_file, file='data_'//data_n//'.csv' ,status='unknown')
    write (n_file,*) x_time
    write (n_file,'(30(A,","))') '  R(mm)','T(K)','mf(-)','mCO2(-)','v(m/s)','rho(kg/m3)','enthalpy(J/kg)'
    do n=1, 400
        x_mm = xscl(n)*1.0d3
        write (n_file,'(30(E12.4,","))') x_mm, temp(n),m_chsp(n,14),m_chsp(n,16),vel(n),dens(n),enth(n)
    end do
    close(n_file)
!
end subroutine