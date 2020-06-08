subroutine calc_dens
    use main_variables
!
    do n=1, nmax
        dens(n)   = pres0*ave_mol_w/(gas_const*temp(n))
    end do
end subroutine
