subroutine initial_set
!
    use main_variables
    real*8 mf_chem(nsp),mf_air(nsp),mf_N2(nsp),mb_phi1(nsp),mb_comp1(nsp)
!
    data mf_chem  /2.85110947E-02, 0.00000000E+00, 2.26276778E-01, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 7.45212127E-01/
    data mf_air   /0.00000000E+00, 0.00000000E+00, 7.46863515E-02, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 9.25313648E-01/
    data mf_N2    /0.00000000E+00, 0.00000000E+00, 2.10051074E-02, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 9.78994893E-01/ 
    data mb_phi1  /0.00000000E+00, 0.00000000E+00, 2.71838006E-04, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 9.99728162E-01/
    data mb_comp1 /0.00000000E+00, 0.00000000E+00, 7.22751604E-05, 0.00000000E+00, 0.00000000E+00, &
                   0.00000000E+00, 0.00000000E+00, 0.00000000E+00, 9.99927725E-01/
!
!   --------- scalar values ----------
    do n=1, nmax
        if (n.le.100) then
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
      
  