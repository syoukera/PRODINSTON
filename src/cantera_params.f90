module cantera_params
    use iso_c_binding
    use main_variables, only: nsp
    implicit none

    interface 

        subroutine getnextty(y, temperature, dt) bind(C)
            import 
            real(c_double), intent(inout) :: y(nsp)
            real(c_double), intent(inout) :: temperature
            real(c_double), intent(in) :: dt
        end subroutine

        subroutine getproperties(y, temperature, diff, lambda, cp, mobility) bind(C)
            import 
            real(c_double), intent(inout) :: y(nsp)
            real(c_double), intent(inout) :: temperature
            real(c_double), intent(out) :: diff(nsp)
            real(c_double), intent(out) :: lambda
            real(c_double), intent(out) :: cp
            real(c_double), intent(out) :: mobility(nsp)
        end subroutine

    end interface
    ! real(c_double) :: y(10)
    ! real(c_double) :: temperature = 1000.0d0
    ! integer(c_int) ierr

    ! real(c_double) :: diff(10)
    ! real(c_double) :: lambda
    ! real(c_double) :: cp

    ! ! data y / 0,0,0,0.220183486,0,0,0,0,0,0,0,0,0,0.055045872,0,0,0,0,0,0,0,0,0,0, &
    ! !     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.724770642,0,0,0,0,0 / 
    
    ! data y / 0.028301887, 0, 0, 0.226415094, 0, 0, 0, 0, 0, 0.745283019 / 
    
    ! call getnextty(y, temperature)

    ! write (*, *) 'Temperature: ', temperature
    ! write (*, *) 'Mass Fraction: ', y

    ! call getproperties(y, temperature, diff, lambda, cp)

    ! write (*, *) 'Heat capacity: ', cp
    ! write (*, *) 'Thermal conductivity: ', lambda
    ! write (*, *) 'Diffusion coefficient: ', diff

end module