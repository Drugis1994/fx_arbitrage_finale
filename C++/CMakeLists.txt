cmake_minimum_required(VERSION 3.16)
project(engine_v4 C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

option(ENABLE_SANITIZERS "Build with Address/Undefined sanitizers" OFF)

# macOS: allow unresolved symbols for R
if(APPLE)
    set(CMAKE_C_VISIBILITY_PRESET default)
    set(CMAKE_VISIBILITY_INLINES_HIDDEN OFF)
endif()

# ---- Finn R include path ----
execute_process(
  COMMAND R RHOME
  OUTPUT_VARIABLE R_HOME
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(R_INCLUDE_DIR "${R_HOME}/include")
if (NOT EXISTS "${R_INCLUDE_DIR}/R.h")
    set(R_INCLUDE_DIR "/usr/share/R/include")
endif()

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

find_package(Threads REQUIRED)

# ---- Optimalisering ----
if (CMAKE_C_COMPILER_ID MATCHES "Clang|AppleClang|GNU")
    target_compile_options(engine_v4 PRIVATE -O3 -march=native)
endif()

if (ENABLE_SANITIZERS)
    target_compile_options(engine_v4 PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
    target_link_options(engine_v4 PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
endif()

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

target_link_libraries(engine_v4 PRIVATE Threads::Threads m)

enable_testing()

add_executable(matrix_tests
    matrix.c
    tests/test_matrix.c)
target_include_directories(matrix_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(matrix_tests PRIVATE Threads::Threads m)

add_executable(engine_tests
    engine.c
    routes.c
    matrix.c
    ticks.c
    tests/test_engine_concurrency.c)
target_include_directories(engine_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(engine_tests PRIVATE Threads::Threads m)

if (ENABLE_SANITIZERS)
    target_compile_options(matrix_tests PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
    target_link_options(matrix_tests PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
    target_compile_options(engine_tests PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
    target_link_options(engine_tests PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
endif()

add_test(NAME matrix_tests COMMAND matrix_tests)
add_test(NAME engine_tests COMMAND engine_tests)

set_target_properties(engine_v4 PROPERTIES OUTPUT_NAME "engine_v4")