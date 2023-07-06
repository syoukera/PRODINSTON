! chemical kinetics calc. subroutine
subroutine chem_source(ntime)
!
    use main_variables
    use chemkin_params, only: get_next_TY

    integer ntime
    real*8 t_cell
    real*8 mf_chem(nsp),hbms
    real*8 chem_t
    real*8 :: tols(4)
    data tols /1.E-8, 1.E-20, 1.E-5, 1.E-5/
!
    do n=1,nmax
        t_cell = o_temp(n)
        chem_t=0.0d0
        do i=1, nsp
            mf_chem(i) = o_m_chsp(n,i)
            if (mf_chem(i).le.0.0d0) mf_chem(i) = 0.0d0
            chem_t=chem_t+mf_chem(i)
        end do
! 
        do i=1,nsp
            mf_chem(i) = mf_chem(i)/chem_t
        end do
!
       call get_next_TY(pres0, t_cell, mf_chem, delt_t, tols)
!
        temp(n) = t_cell
        do i=1, nsp
            o_m_chsp(n,i) = mf_chem(i)
            m_chsp(n,i)   = mf_chem(i) 
        end do
    end do      
!    
end subroutine