! chemical kinetics calc. subroutine using Cantera
subroutine chem_source_cantera(ntime)
!
    use main_variables
    use cantera_params, only: getnextty
    use output, only: make_output
    !$ use omp_lib

    integer ntime
    real*8 t_cell
    real*8 mf(nsp),hbms
    real*8 chem_t
    real*8 :: tols(4)
    data tols /1.E-8, 1.E-20, 1.E-5, 1.E-5/

    !$omp parallel
    do n=1,nmax
        t_cell = o_temp(n)
        chem_t=0.0d0
        do i=1, nsp
            mf(i) = o_m_chsp(n,i)
            if (mf(i).le.0.0d0) mf(i) = 0.0d0
            chem_t=chem_t+mf(i)
        end do
! 
        do i=1,nsp
            mf(i) = mf(i)/chem_t
        end do
!
        call getnextty(mf, t_cell, delt_t)
!
        temp(n) = t_cell
        do i=1, nsp
            o_m_chsp(n,i) = mf(i)
            m_chsp(n,i)   = mf(i) 
        end do
    end do      
    !$omp end parallel
!    
end subroutine

! chemical kinetics calc. subroutine using CHEMKIN
subroutine chem_source_chemkin(ntime)
    !
        use main_variables
        PARAMETER ( LENIWK = 1000000, LENRWK = 20000000, LENCWK = 10000, LENSYM = 16)
        integer IWORK (LENIWK),ntime
        real*8 RWORK (LENRWK)
        real*8 t_cell
        real*8 mf_chem(nsp),hbms
        real*8 chem_t
        real*8 :: tols(4)
        logical ::make_output = .false.
        data tols /1.E-8, 1.E-20, 1.E-5, 1.E-5/
        COMMON /POINT/ IPICK, IPRCK, IPWT, IPWDOT, IPU, IPRD
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
            call driver(t_cell, pres0, mf_chem, delt_t, tols, make_output,iwork,rwork)
    !
            temp(n) = t_cell
            do i=1, nsp
                o_m_chsp(n,i) = mf_chem(i)
                m_chsp(n,i)   = mf_chem(i) 
            end do
        end do      
    !    
end subroutine