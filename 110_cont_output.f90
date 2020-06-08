subroutine cont_output(xtime,n_out)
!
    use main_variables
    real*8 xtime
    integer n_out
!
    open(unit=3, file='data_cont.dat' ,status='unknown')
    write (3,*) n_out, xtime, vel, pres, temp, dens, enth, m_chsp
    close(3)
!
end subroutine