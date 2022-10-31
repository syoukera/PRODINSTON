subroutine log_output(xtime,nl_file)
!
    use main_variables
    integer nl_file
    real*8 xtime
    real*8 x_0310, x_1100, x_1900, v_0310, v_1100, v_1900
    integer i_0310, i_1100, i_1900
    real*8 x_0310o, x_1100o, x_1900o, v_0310o, v_1100o, v_1900o
    integer i_0310o, i_1100o, i_1900o
    real*8 burn_v, burn_r
    real*8 mf_T, V_p
!
    open(unit=nl_file, file='time_data.csv' ,status='unknown', position='append')
!
    x_0310 = 0.0d0
    x_1100 = 0.0d0
    x_1900 = 0.0d0
    v_0310 = 0.0d0
    v_1100 = 0.0d0
    v_1900 = 0.0d0
    i_0310 = 0
    i_1100 = 0
    i_1900 = 0
    do i=nmax,1,-1
        if (temp(i).ge.310.0d0) then
            i_0310 = i
            x_0310 = (xscl(i+1)*(temp(i)-310.0d0)+xscl(i)*(310.0d0-temp(i+1)))/(temp(i)-temp(i+1))
            v_0310 = (vel(i)-vel(i+1))*(xvel(i+1)-x_0310)/(xvel(i+1)-xvel(i))+vel(i+1)
            go to 100
        end if
    end do
100 continue
    do i=nmax,1,-1
        if (temp(i).ge.1100.0d0) then
            i_1100 = i
            x_1100 = (xscl(i+1)*(temp(i)-1100.0d0)+xscl(i)*(1100.0d0-temp(i+1)))/(temp(i)-temp(i+1))
            v_1100 = (vel(i)-vel(i+1))*(xvel(i+1)-x_1100)/(xvel(i+1)-xvel(i))+vel(i+1)
            go to 200
        end if
    end do
200 continue
    do i=nmax,1,-1
        if (temp(i).ge.1900.0d0) then
            i_1900 = i
            x_1900 = (xscl(i+1)*(temp(i)-1900.0d0)+xscl(i)*(1900.0d0-temp(i+1)))/(temp(i)-temp(i+1))
            v_1900 = (vel(i)-vel(i+1))*(xvel(i+1)-x_1900)/(xvel(i+1)-xvel(i))+vel(i+1)
            go to 300
        end if
    end do
300 continue
!
    x_0310o = 0.0d0
    x_1100o = 0.0d0
    x_1900o = 0.0d0
    v_0310o = 0.0d0
    v_1100o = 0.0d0
    v_1900o = 0.0d0
    i_0310o = 0
    i_1100o = 0
    i_1900o = 0    
    do i=nmax,1,-1
        if (o_temp(i).ge.310.0d0) then
            i_0310o = i
            x_0310o = (xscl(i+1)*(o_temp(i)-310.0d0)+xscl(i)*(310.0d0-o_temp(i+1)))/(o_temp(i)-o_temp(i+1))
            v_0310o = (o_vel(i)-o_vel(i+1))*(xvel(i+1)-x_0310o)/(xvel(i+1)-xvel(i))+o_vel(i+1)
            go to 110
        end if
    end do
110 continue
    do i=nmax,1,-1
        if (o_temp(i).ge.1100.0d0) then
            i_1100o = i
            x_1100o = (xscl(i+1)*(o_temp(i)-1100.0d0)+xscl(i)*(1100.0d0-o_temp(i+1)))/(o_temp(i)-o_temp(i+1))
            v_1100o = (o_vel(i)-o_vel(i+1))*(xvel(i+1)-x_1100o)/(xvel(i+1)-xvel(i))+o_vel(i+1)
            go to 210
        end if
    end do
210 continue
    do i=nmax,1,-1
        if (o_temp(i).ge.1900.0d0) then
            i_1900o = i
            x_1900o = (xscl(i+1)*(o_temp(i)-1900.0d0)+xscl(i)*(1900.0d0-o_temp(i+1)))/(o_temp(i)-o_temp(i+1))
            v_1900o = (o_vel(i)-o_vel(i+1))*(xvel(i+1)-x_1900o)/(xvel(i+1)-xvel(i))+o_vel(i+1)
            go to 310
        end if
    end do
310 continue
!
!   === conservation check ===
!
    mf_T = 0.0d0
    do i=1,nmax-1
        V_p = xvel(i+1) - xvel(i)
        mf_T = mf_T+m_chsp(i,1)*dens(i)*V_p
    end do
!
!
    burn_r = (x_1100-x_1100o)/delt_t
    burn_v = burn_r-v_1100
    write (6,'(30(E12.4))') x_0310,x_1100,x_1900,burn_r,v_1100,burn_v,mf_T
    write (nl_file,'(30(E14.6,","),/)') xtime,x_0310,x_1100,x_1900,burn_r,v_1100,burn_v,mf_T
    close (nl_file)
!
end subroutine