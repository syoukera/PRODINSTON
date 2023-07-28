subroutine calc_trans_coef
!
    use main_variables
    use chemkin_params, only: get_tranport_data
    real*8 chem_t, mf_chem(nsp), t_cell
    real*8 D_mix(nsp)      !diffusion coefficient  [cm^2/s]
    real*8 Lambda_mix      !thermal conductivity   [erg/(cm*K*s)]
    real*8 c_p             !specific heat          [erg/(g*K)]
!
!
    do n=1, nmax
        do i = 1, nsp
            mf_chem(i) = m_chsp(n, i)
        end do
        chem_t=0.0d0
        do i=1, nsp
            if (mf_chem(i).le.0.0d0) mf_chem(i) = 0.0d0
            chem_t=chem_t+mf_chem(i)
        end do
! 
        do i=1,nsp
            mf_chem(i) = mf_chem(i)/chem_t
        end do
        t_cell = o_temp(n)
!
        call get_tranport_data(t_cell, pres(n)+pres0, mf_chem, D_mix, Lambda_mix, c_p)
!
        T_D(n) = Lambda_mix/c_p*1.0d-1
        write (6,*) n, Lambda_mix, c_p
        do i=1, nsp
            x_D(n,i) = D_mix(i)*1.0d-4
        end do
    end do

end subroutine