! Fix flame at same position
subroutine fix_flame_pos()
    implicit none

    call get_flame_pos
    call calc_flame_vel
    call disp_flame_pos

end subroutine

subroutine get_flame_pos()
    use main_variables, only:nmax, xscl, temp, temp_flame, pos_flame
    implicit none

    integer n

    do n = 1, nmax
        if (temp(n) < temp_flame) then
            pos_flame = xscl(n)
            exit
        endif

        if (n == nmax) then
            write (6, 'Error: Flame position was not found')
            stop
        endif
    enddo

    ! write (6,'("flame position = ",e12.4)') pos_flame

end subroutine

subroutine calc_flame_vel()
    implicit none

end subroutine

subroutine disp_flame_pos()
    implicit none

end subroutine