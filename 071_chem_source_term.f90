! chemical kinetics calc. subroutine
subroutine chem_source()
    !
        use main_variables
        use chemkin_params, only: get_next_TY
    
        real*8 t_cell        ! temperature, variable for input/output in chemkin
        real*8 mf_chem(nsp)  ! mass fraction, variable for input/output in chemkin 
        real*8 chem_t        ! total mass fraction
        real*8 :: tols(4)    ! tolerance for CHEMKIN
        data tols /1.E-8, 1.E-20, 1.E-5, 1.E-5/
    !
        do n=1,nmax
    
            ! asign old temperature to t_cell
            t_cell = o_temp(n)
    
            ! initialize total mass fraction as 0.0
            chem_t=0.0d0
    
            ! remove negative value and summarize mass fraction
            do i=1, nsp
                ! asign old mass fraction to mf_chem
                mf_chem(i) = o_m_chsp(n,i)
                ! remove negative value of mf_chem
                if (mf_chem(i).le.0.0d0) mf_chem(i) = 0.0d0
                ! add mf_chem to chem_t
                chem_t=chem_t+mf_chem(i)
            end do
    ! 
            ! normalize mass fraction by total mass fraction
            do i=1,nsp
                mf_chem(i) = mf_chem(i)/chem_t
            end do
    !
            ! update temperature and mass fraction after delt_t
            call get_next_TY(pres0, t_cell, mf_chem, delt_t, tols)
    !
            ! return updated temperature
            temp(n) = t_cell
    
            do i=1, nsp
                ! return updated temperature
                m_chsp(n,i)   = mf_chem(i) 
            end do
        end do      
    !    
    end subroutine