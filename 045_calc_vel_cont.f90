subroutine calc_vel_cont
!
    use main_variables
!
    if (is_flat .eqv. .true.) then
        ! flat flame
        ! Control Volume is Quadrangular prism
        S_cp = 1.0d0  ! cp = capital p
        S_p = 1.0d0   ! area of surface of sphere
        V_p = xvel(1) ! volume of Control Volume (= delta_x)
    else 
        ! spherical flame
        ! Control Volume is sphere with radius of delta_x
        S_cp = 0.0d0
        S_p = 4.0d0*pai*xvel(1)**2         ! area of surface of sphere
        V_p = 4.0d0/3.0d0*pai*(xvel(1)**3) ! volume of Control Volume
    endif
    
! 
    vel(1) = -(dens(1)-o_dens(1))*V_p/delt_t + dens(1)*vel(1)*S_cp &
             /(0.5d0*(dens(1)+dens(2))*S_p)
! 
    do n=2,nmax-1
!   
        if (is_flat .eqv. .true.) then
            ! flat flame
            ! Control Volume is Quadrangular prism
            S_p = 1.0d0 ! area of upper surface of Control Volume
            S_m = 1.0d0 ! area of lower surface of Control Volume
            V_p = xvel(n)-xvel(n-1) ! volume of Control Volume (= delta_x)
        else 
            ! spherical flame
            ! Control Volume is spherical shell
            S_p = 4.0d0*pai*xvel(n)**2   ! area of upper surface of Control Volume
            S_m = 4.0d0*pai*xvel(n-1)**2 ! area of lower surface of Control Volume
            V_p = 4.0d0/3.0d0*pai*(xvel(n)**3-xvel(n-1)**3) ! volume of Control Volume
        endif
!
        vel(n) = (0.5d0*(dens(n)+dens(n-1))*S_m*vel(n-1)-(dens(n)-o_dens(n))*V_p/delt_t) &
                 /(0.5d0*(dens(n)+dens(n+1))*S_p)
    end do
!
    if (is_flat .eqv. .true.) then
        ! flat flame
        ! Control Volume is Quadrangular prism
        S_p = 1.0d0 ! area of upper surface of Control Volume
        S_m = 1.0d0 ! area of lower surface of Control Volume
        V_p = xvel(nmax)-xvel(nmax-1) ! volume of Control Volume (= delta_x)
    else 
        ! Control Volume is spherical shell
        S_p = 4.0d0*pai*xvel(nmax)**2   ! area of upper surface of Control Volume
        S_m = 4.0d0*pai*xvel(nmax-1)**2 ! area of lower surface of Control Volume
        V_p = 4.0d0/3.0d0*pai*(xvel(nmax)**3-xvel(nmax-1)**3) ! volume of Control Volume
    endif
!
    vel(nmax) = (0.5d0*(dens(nmax)+dens(nmax-1))*S_m*vel(nmax-1)-(dens(nmax)-o_dens(nmax))*V_p/delt_t) &
                 /(dens(nmax)*S_p)
!
end subroutine