subroutine calc_dens_ck()

    ! calculate density using CHEMKIN subroutine
    ! can consider mixture averaged Molcular weight

    !!! NOT IMPREMENTED CORRECTRY
    ! output density is not guaranteed
    ! please use calc_dens()

    use main_variables
    use chemkin_params, only: get_density
!
    implicit none

    integer n, i
    real*8 density
    real*8 chem_t       ! total amount of chsp
    real*8 mf_chem(nsp) ! variable array of m_chsp for chemkin

    do n = 1, nmax

        ! pre process for chemkin 
        ! assign composition of chemical species
        do i = 1, nsp
            mf_chem(i) = m_chsp(n, i)
        end do

        ! initialize total value
        chem_t=0.0d0
        do i=1, nsp
            ! remove negative values
            if (mf_chem(i).le.0.0d0) mf_chem(i) = 0.0d0
            ! add non-negative values to total value
            chem_t=chem_t+mf_chem(i)
        end do
! 
        ! normalize chemical composition by total value
        do i=1,nsp
            mf_chem(i) = mf_chem(i)/chem_t
        end do

        ! calculate density using chemkin subroutine
        call get_density(temp(n), pres(n)+pres0, mf_chem, density)
        ! assign obtained density in variable array
        dens(n) = density

    end do

end subroutine calc_dens_ck

subroutine calc_dens()

    ! calculate density from gas equation
    ! use averaged molcular weight
    ! initialy usign value of Nitrogen

    use main_variables
    implicit none
    integer n

    call calc_mean_wt()

    do n=1, nmax
        dens(n)   = (pres(n)+pres0)*mean_wt(n)/(gas_const*temp(n))
        ! dens(n)   = (pres(n)+pres0)*ave_mol_w/(gas_const*temp(n))
    end do

end subroutine calc_dens

subroutine calc_mean_wt()

    use main_variables
    use chemkin_params, only: get_mean_wt
    implicit none

    integer n, i
    real*8 wt           ! mean molecular weight from chemkin
    real*8 chem_t       ! total amount of chsp
    real*8 mf_chem(nsp) ! variable array of m_chsp for chemkin

    do n = 1, nmax

        ! pre process for chemkin 
        ! assign composition of chemical species
        do i = 1, nsp
            mf_chem(i) = m_chsp(n, i)
        end do

        ! initialize total value
        chem_t=0.0d0
        do i=1, nsp
            ! remove negative values
            if (mf_chem(i).le.0.0d0) mf_chem(i) = 0.0d0
            ! add non-negative values to total value
            chem_t=chem_t+mf_chem(i)
        end do
! 
        ! normalize chemical composition by total value
        do i=1,nsp
            mf_chem(i) = mf_chem(i)/chem_t
        end do

        ! calculate value using chemkin subroutine
        call get_mean_wt(mf_chem, wt)
        ! assign obtained value in variable array
        mean_wt(n) = wt

    end do

end subroutine calc_mean_wt
