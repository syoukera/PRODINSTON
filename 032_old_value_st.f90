subroutine old_value_st
    use main_variables
!
    do n=1, nmax
        o_vel(n) = vel(n)
        o_pres(n) = pres(n)                         
        o_temp(n) = temp(n)
        o_dens(n) = dens(n)
        o_enth(n) = enth(n)
        do i=1, nsp
            o_m_chsp(n,i) = m_chsp(n,i)
        end do      
    end do
!
end subroutine