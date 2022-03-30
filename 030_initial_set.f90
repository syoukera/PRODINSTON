subroutine initial_set
!
    use main_variables
    real*8 mf_chem(nsp),mf_air(nsp),mf_N2(nsp),mb_phi1(nsp),mb_comp1(nsp)
!
    data mf_chem  /2.85110947E-02, 0.00000000E+00, 2.26276778E-01, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 7.45212127E-01/
    data mf_air   /0.00000000E+00, 0.00000000E+00, 2.32917511E-01, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 7.67082489E-01/
    data mf_N2    /0.00000000E+00, 0.00000000E+00, 2.32917511E-01, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 7.67082489E-01/ 
    data mb_phi1  /1.19900376E-03, 9.27455236E-05, 1.07852715E-02, 5.86294984E-04, 5.96561322E-03, &
                   2.61119326E-06, 3.00076982E-07, 2.36328415E-01, 7.45039744E-01/
    data mb_comp1 /0.00000000E+00, 0.00000000E+00, 2.32917511E-01, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 7.67082489E-01/
!
!   --------- scalar values ----------
    do n=1, nmax
        if (n.le.5) then
            temp(n) = 5000.0d0
            enth(n) = 5902518.0d0
            do i=1, nsp
                m_chsp(n,i) = mb_phi1(i)
            end do
        else if (n.le.400) then
            temp(n) = 300.0d0
            enth(n) = -23690.527d0
            do i=1, nsp
                m_chsp(n,i) = mf_chem(i)
            end do
        else
            temp(n) = 300.0d0
            enth(n) = 1913.755d0
            do i=1, nsp
                m_chsp(n,i) = mf_air(i)
            end do          
        end if
        vel(n)      = 0.0d0
        pres(n)     = 0.0d0
        dens(n)     = pres0*ave_mol_w/(gas_const*temp(n))
        x_mu(n)     = 1.767d-5       !!temporary N2@300K [Pa s]
        T_D(n)      = 0.0257/1006.0  !!temporary heat trans coef (N2@300K[W/mK]) / specific heat   (N2@300K[J/kgK])
       do i=1, nsp
            x_D(n,i) = 0.205d-4      !!temporary diffusion coef (self N2[m2/s])
        end do
    end do
end subroutine
      
  