subroutine calc_vel_cont
!
    use main_variables
!
    S_p = 4.0d0*pai*xvel(1)**2
    V_p = 4.0d0/3.0d0*pai*(xvel(1)**3)
! 
    vel(1) = -(dens(1)-o_dens(1))*V_p/delt_t/(0.5d0*(dens(1)+dens(2))*S_p)
! 
    do n=2,nmax-1
!   
        S_p = 4.0d0*pai*xvel(n)**2
        S_m = 4.0d0*pai*xvel(n-1)**2
        V_p = 4.0d0/3.0d0*pai*(xvel(n)**3-xvel(n-1)**3)
!
        vel(n) = (0.5d0*(dens(n)+dens(n-1))*S_m*vel(n-1)-(dens(n)-o_dens(n))*V_p/delt_t) &
                 /(0.5d0*(dens(n)+dens(n+1))*S_p)
    end do
!
    S_p = 4.0d0*pai*xvel(nmax)**2
    S_m = 4.0d0*pai*xvel(nmax-1)**2
    V_p = 4.0d0/3.0d0*pai*(xvel(nmax)**3-xvel(nmax-1)**3)
!
    vel(nmax) = (0.5d0*(dens(nmax)+dens(nmax-1))*S_m*vel(nmax-1)-(dens(nmax)-o_dens(nmax))*V_p/delt_t) &
                 /(dens(nmax)*S_p)
!
end subroutine