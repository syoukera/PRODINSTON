subroutine cont_input(xtime,n_out)
!
    use main_variables
    real*8 xtime
    integer n_out
!
    open(unit=4, file='data_cont.dat' ,status='unknown')
    read (4,*) n_out, xtime, vel, pres, temp, dens, enth, m_chsp
    close(4)
!
end subroutine