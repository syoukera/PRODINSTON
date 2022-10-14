cmake_minimum_required(VERSION 3.0.0)
project(test_cmake VERSION 0.1.0)

enable_language(Fortran)

# include(CTest)
# enable_testing()

add_compile_options(-fno-range-check)

set(EXECUTABLE abcd)
add_executable(${EXECUTABLE}
    000_maintest.f90
    010_module.f90
    020_grid_set.f90
    030_initial_set.f90
    032_old_value_st.f90
    035_calc_dens.f90
    038_calc_trans_coef.f90
    040_scl_trans.f90
    042_enth_trans.f90
    045_calc_vel_cont.f90
    050_calc_scl_coef.f90
    060_calc_tdma.f90
    070_chem_source.f90
    080_fix_flame_pos.f90
    100_data_output.f90
    103_log_output.f90
    105_cont_input.f90
    110_cont_output.f90
    210_chemkin_m.f90
    210_cklib.f  
    220_dasac.f  
    230_dmath.f  
    240_output_m.f90 
    250_senkin.f
    260_tranlib.f
)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
# include(CPack)
