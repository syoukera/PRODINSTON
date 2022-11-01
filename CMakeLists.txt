cmake_minimum_required(VERSION 3.0)

enable_language(Fortran)
project(prodinston Fortran CXX)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_STANDARD 11)

find_package(Threads REQUIRED)
include_directories(include)

# include_directories("C:\Program Files\Cantera\include" "C:\Users\sakir\miniconda3\envs\ct_src\Library\include")
# link_directories("C:\Program Files\Cantera\lib")
# include_directories(include)

add_compile_options(-fno-range-check)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ..)

add_subdirectory(src)
