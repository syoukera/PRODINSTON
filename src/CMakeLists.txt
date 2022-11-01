add_executable(${PROJECT_NAME}
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
    cantera_params.cpp
    cantera_params.f90
)

target_link_libraries(${PROJECT_NAME} cantera Threads::Threads)

set_target_properties(${PROJECT_NAME}
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
  )
