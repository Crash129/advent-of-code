cmake_minimum_required(VERSION 3.0.0)
project(advent-of-code VERSION 0.1.0)

set(CMAKE_CXX_FLAGS "-Ofast")

FILE(GLOB dirs "${CMAKE_SOURCE_DIR}/2020/*")
FOREACH(dir ${dirs})
    IF(IS_DIRECTORY "${dir}" AND EXISTS "${dir}/CMakeLists.txt")
        add_subdirectory("${dir}")
    ENDIF()
ENDFOREACH()
