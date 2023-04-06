subroutine initial_set
!
    use main_variables
                   !
!   --------- scalar values ----------
    do n=1, nmax
        if (n.le.nmax/6) then
            temp(n) = 5000.0d0
            enth(n) = 5902518.0d0
            do i=1, nsp
                m_chsp(n,i) = mf_burned(i)
            end do
        else
            temp(n) = 300.0d0
            enth(n) = -23690.527d0
            do i=1, nsp
                m_chsp(n,i) = mf_unburned(i)
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
      
  