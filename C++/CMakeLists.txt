cmake_minimum_required(VERSION 3.16)
project(engine_v4 C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# macOS: allow unresolved symbols for R
if(APPLE)
    set(CMAKE_C_VISIBILITY_PRESET default)
    set(CMAKE_VISIBILITY_INLINES_HIDDEN OFF)
endif()

# ---- Finn R include path ----
execute_process(
  COMMAND Rscript -e "cat(R.home('include'))"
  OUTPUT_VARIABLE R_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# ---- Kilder ----
set(SRC
    engine.c
    engine_R.c
    routes.c
    matrix.c
    ticks.c
)

add_library(engine_v4 SHARED ${SRC})

# ---- Include dirs ----
target_include_directories(engine_v4 PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${R_INCLUDE_DIR}
)

# ---- macOS: dynamic lookup ----
if(APPLE)
    target_link_options(engine_v4 PRIVATE "-Wl,-undefined,dynamic_lookup")
endif()

# ---- Optimalisering ----
if (CMAKE_C_COMPILER_ID MATCHES "Clang|AppleClang|GNU")
    target_compile_options(engine_v4 PRIVATE -O3 -march=native)
endif()

set_target_properties(engine_v4 PROPERTIES OUTPUT_NAME "engine_v4")

enable_testing()

add_executable(matrix_tests
    test_matrix.c
    matrix.c
)
target_include_directories(matrix_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
if(NOT WIN32)
    target_link_libraries(matrix_tests PRIVATE m)
endif()
add_test(NAME matrix_tests COMMAND matrix_tests)