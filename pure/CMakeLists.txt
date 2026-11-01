# CMake build of Pure for Windows with the Microsoft compiler (cl.exe).
#
# On every other system Pure is built with configure && make; this file is
# not used there. It builds libpure.dll and pure.exe against LLVM's CMake
# package and libraries from vcpkg (x64-windows), and runs the regression
# tests with ctest, comparing each test's output with test/*.log as the
# run-tests script does.
#
#   cmake -G Ninja -B build -DCMAKE_BUILD_TYPE=Release ^
#     -DCMAKE_TOOLCHAIN_FILE=C:/vcpkg/scripts/buildsystems/vcpkg.cmake ^
#     -DLLVM_DIR=C:/llvm/lib/cmake/llvm
#   cmake --build build
#   ctest --test-dir build        (or: cmake --build build --target check)
#
# LLVM must be built with the same C runtime as Pure and the vcpkg
# libraries, i.e. the DLL runtime (/MD, CMAKE_MSVC_RUNTIME_LIBRARY=
# MultiThreadedDLL). The libraries in the official LLVM Windows release use
# the static runtime (/MT) and cannot be linked into libpure.dll.
#
# Required vcpkg ports: gmp mpfr libiconv pthreads tre dirent, and
# readline-win32 unless PURE_WITH_READLINE is OFF. flex and bison are found
# as win_flex/win_bison (winflexbison) or flex/bison.

cmake_minimum_required(VERSION 3.21)
# The version comes from configure.ac, so that there is one place to bump it.
file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/configure.ac _pure_ac_init
  REGEX "^AC_INIT\\(pure, *[0-9.]+\\)")
string(REGEX REPLACE "^AC_INIT\\(pure, *([0-9.]+)\\).*" "\\1"
  _pure_version "${_pure_ac_init}")
project(pure VERSION ${_pure_version} LANGUAGES C CXX)

if(NOT MSVC)
  message(FATAL_ERROR "This CMake build is for MSVC on Windows; use "
    "./configure && make on other systems.")
endif()
# config.h and the longjmp handling in runtime.cc assume 64-bit x86.
if(NOT CMAKE_SIZEOF_VOID_P EQUAL 8 OR
   NOT CMAKE_SYSTEM_PROCESSOR MATCHES "^(AMD64|x86_64)$")
  message(FATAL_ERROR "The MSVC build of Pure supports only x64.")
endif()

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
# Release-type builds only: libpure links the release C runtime, like the
# /MD LLVM it needs, and vcpkg's debug libraries use the debug one.
set(CMAKE_MSVC_RUNTIME_LIBRARY MultiThreadedDLL)

option(PURE_WITH_READLINE "Build pure.exe with readline (vcpkg readline-win32)" ON)
set(PURE_HOST "x86_64-pc-windows-msvc" CACHE STRING
  "Host triplet reported by Pure (and matched by --if pragmas)")

include(GNUInstallDirs)

# LLVM.
find_package(LLVM REQUIRED CONFIG)
if(LLVM_PACKAGE_VERSION VERSION_LESS 20.1)
  message(FATAL_ERROR "LLVM >= 20.1 is required, found ${LLVM_PACKAGE_VERSION}")
endif()
message(STATUS "LLVM ${LLVM_PACKAGE_VERSION} in ${LLVM_DIR}")
llvm_map_components_to_libnames(PURE_LLVM_LIBS
  core bitreader bitwriter linker orcjit passes support native)
# Directory of llc, opt and clang for batch compilation (pure -c) and inline
# code, with a trailing slash; empty to search PATH. The LLVM that libpure
# is built against may have no tools (a libraries-only build), so this
# can point at another installation of the same version.
set(PURE_TOOL_PREFIX "${LLVM_TOOLS_BINARY_DIR}/" CACHE STRING
  "Prefix of the LLVM tools (with trailing /)")

# Libraries from vcpkg.
find_path(GMP_INCLUDE_DIR gmp.h REQUIRED)
find_library(GMP_LIBRARY gmp REQUIRED)
find_path(MPFR_INCLUDE_DIR mpfr.h REQUIRED)
find_library(MPFR_LIBRARY mpfr REQUIRED)
find_package(Iconv REQUIRED)
find_package(PThreads4W REQUIRED)
find_path(TRE_INCLUDE_DIR tre/regex.h REQUIRED)
find_library(TRE_LIBRARY tre REQUIRED)
find_path(DIRENT_INCLUDE_DIR dirent.h REQUIRED)
if(PURE_WITH_READLINE)
  find_path(READLINE_INCLUDE_DIR readline/readline.h REQUIRED)
  find_library(READLINE_LIBRARY readline REQUIRED)
endif()

find_package(FLEX 2.5.31 REQUIRED)
find_package(BISON 2.3 REQUIRED)

set(GEN ${CMAKE_CURRENT_BINARY_DIR}/gen)
file(MAKE_DIRECTORY ${GEN})

# config.h (configure makes it on other systems).
if(PURE_WITH_READLINE)
  set(HAVE_LIBREADLINE 1)
  set(USE_READLINE 1)
  set(HAVE_READLINE_READLINE_H 1)
  set(HAVE_READLINE_HISTORY 1)
  set(HAVE_READLINE_HISTORY_H 1)
  set(HAVE_HISTORY_SET_HISTORY_STATE 1)
endif()
set(PURE_VERSION ${PROJECT_VERSION})
configure_file(win32/config.h.cmake ${GEN}/config.h)

# Scanner and parser.
add_custom_command(OUTPUT ${GEN}/lexer.cc
  COMMAND ${FLEX_EXECUTABLE} --wincompat -o ${GEN}/lexer.cc
    ${CMAKE_CURRENT_SOURCE_DIR}/lexer.ll
  DEPENDS lexer.ll
  COMMENT "flex lexer.ll")
add_custom_command(
  OUTPUT ${GEN}/parser.cc ${GEN}/parser.hh ${GEN}/location.hh
  COMMAND ${BISON_EXECUTABLE} -v -o ${GEN}/parser.cc
    ${CMAKE_CURRENT_SOURCE_DIR}/parser.yy
  DEPENDS parser.yy
  COMMENT "bison parser.yy")

# Installation layout, as with configure: bin/pure.exe and bin/libpure.dll,
# lib/pure/*.pure, include/pure/runtime.h.
set(PURE_LIBDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
set(PURELIB "${PURE_LIBDIR}/pure")

# Common compiler settings. LLVM's own definitions (LLVM_DEFINITIONS)
# include UNICODE, which would switch the Win32 calls in pure.cc to their
# wide-character variants, so take only its include path and add the
# settings it needs. The CRT deprecation warnings (C4996) are about the
# POSIX names and the non-_s string functions, which the portable sources
# use throughout.
add_compile_definitions(
  _CRT_SECURE_NO_WARNINGS _CRT_NONSTDC_NO_WARNINGS _SCL_SECURE_NO_WARNINGS
  __STDC_CONSTANT_MACROS __STDC_FORMAT_MACROS __STDC_LIMIT_MACROS
  NOMINMAX WIN32_LEAN_AND_MEAN)
# Warnings that are off, none of which GCC or clang give at -Wall:
#   C4244, C4267  implicit narrowing of 64 bit integers and doubles, which
#                 the sources rely on throughout (off in LLVM's build too);
#   C4146         unary minus on an unsigned value (the hash-keyed
#                 environment maps use -hash() on purpose; off in LLVM too);
#   C4101         unused name of a caught exception, catch (err &e);
#   C4805         comparing a bool with an int (symbol::priv against the
#                 tri-state priv argument of declare_extern);
#   C4068         unknown pragma (the GCC diagnostic pragmas).
# /permissive- is needed by the LLVM headers.
add_compile_options(/W3 /wd4244 /wd4267 /wd4146 /wd4101 /wd4805 /wd4068
  /utf-8 /bigobj
  "/FI${CMAKE_CURRENT_SOURCE_DIR}/win32/msvc-compat.h"
  $<$<COMPILE_LANGUAGE:CXX>:/EHsc> $<$<COMPILE_LANGUAGE:CXX>:/permissive->
  $<$<COMPILE_LANGUAGE:CXX>:/Zc:__cplusplus>)
include_directories(BEFORE ${GEN} ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/win32)
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS} ${GMP_INCLUDE_DIR}
  ${MPFR_INCLUDE_DIR} ${TRE_INCLUDE_DIR} ${DIRENT_INCLUDE_DIR})
if(BISON_VERSION VERSION_GREATER_EQUAL 3.0)
  add_compile_definitions(HAVE_BISON3=1)
endif()

# libpure.dll. All functions are exported, as the MinGW linker does by
# default, since pure.exe and modules use the C++ interface as well as the
# C API in runtime.h.
add_library(libpure SHARED
  expr.cc interpreter.cc matcher.cc printer.cc runtime.cc symtable.cc
  util.cc strptime.c win32/glob.c win32/fnmatch.c win32/posix.c
  ${GEN}/lexer.cc ${GEN}/parser.cc)
# musl's fnmatch compares wchar_t with unsigned ranges on purpose.
set_source_files_properties(win32/fnmatch.c PROPERTIES COMPILE_OPTIONS /wd4018)
set_target_properties(libpure PROPERTIES
  OUTPUT_NAME libpure WINDOWS_EXPORT_ALL_SYMBOLS ON)
target_compile_definitions(libpure PRIVATE
  PURE_BUILDING_LIBPURE
  PURELIB="${PURELIB}" LIBDIR="${PURE_LIBDIR}" LIBPURE="libpure.dll")
target_link_libraries(libpure PUBLIC
  ${GMP_LIBRARY} ${MPFR_LIBRARY} Iconv::Iconv PThreads4W::PThreads4W
  PRIVATE ${TRE_LIBRARY} ${PURE_LLVM_LIBS})

# pure.exe. 8 MB of stack, as the MinGW build links with, to back the
# default PURE_STACK limit (8192K - 128K).
if(PURE_WITH_READLINE)
  add_executable(pure pure.cc)
  target_include_directories(pure PRIVATE ${READLINE_INCLUDE_DIR})
  target_link_libraries(pure PRIVATE ${READLINE_LIBRARY})
else()
  add_executable(pure pure_norl.cc)
endif()
target_compile_definitions(pure PRIVATE PURELIB="${PURELIB}")
target_link_libraries(pure PRIVATE libpure)
target_link_options(pure PRIVATE /STACK:8388608)

# Object file for standalone programs made with pure -c.
add_library(pure_main OBJECT pure_main.c)

# Tests.
enable_testing()
file(GLOB PURE_TESTS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/test
  ${CMAKE_CURRENT_SOURCE_DIR}/test/test*.pure)
list(SORT PURE_TESTS)
set(PURE_RUN_TEST ${CMAKE_CURRENT_SOURCE_DIR}/win32/run-test.cmake)
foreach(t lib/prelude.pure ${PURE_TESTS})
  get_filename_component(name ${t} NAME)
  if(t STREQUAL "lib/prelude.pure")
    set(src ${CMAKE_CURRENT_SOURCE_DIR}/${t})
  else()
    set(src ${CMAKE_CURRENT_SOURCE_DIR}/test/${t})
  endif()
  add_test(NAME ${name}
    COMMAND ${CMAKE_COMMAND} -DPURE=$<TARGET_FILE:pure>
      -DSRCDIR=${CMAKE_CURRENT_SOURCE_DIR} -DTEST=${src}
      -DOUTDIR=${CMAKE_CURRENT_BINARY_DIR}/test -P ${PURE_RUN_TEST})
endforeach()

# The embedding contract test (make check-embed) and the run-time loading
# test (make check-dlopen).
# As with make check-embed, embed runs in test/embed.d/run next to a decoy
# copy of libpure in test/embed.d/lib, to check that libpure does not load
# a second copy of itself by name.
add_executable(embed test/embed.c)
target_link_libraries(embed PRIVATE libpure psapi)
add_custom_command(TARGET embed POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E make_directory
    ${CMAKE_CURRENT_BINARY_DIR}/test/embed.d/lib
    ${CMAKE_CURRENT_BINARY_DIR}/test/embed.d/run
  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:libpure>
    ${CMAKE_CURRENT_BINARY_DIR}/test/embed.d/lib/)
add_executable(dlopen test/dlopen.c)
target_link_libraries(dlopen PRIVATE PThreads4W::PThreads4W)
add_test(NAME embed COMMAND embed
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test/embed.d/run)
set_tests_properties(embed PROPERTIES
  ENVIRONMENT "PURELIB=${CMAKE_CURRENT_SOURCE_DIR}/lib")
add_test(NAME dlopen COMMAND dlopen $<TARGET_FILE:libpure>)
set_tests_properties(dlopen PROPERTIES
  ENVIRONMENT "PURELIB=${CMAKE_CURRENT_SOURCE_DIR}/lib")

add_custom_target(check
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -E "^(embed|dlopen)$$"
  DEPENDS pure USES_TERMINAL)
add_custom_target(check-embed
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -R "^embed$$"
  DEPENDS embed USES_TERMINAL)
add_custom_target(check-dlopen
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -R "^dlopen$$"
  DEPENDS dlopen libpure USES_TERMINAL)

# Installation.
install(TARGETS pure libpure
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES runtime.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/pure)
install(DIRECTORY lib/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/pure
  FILES_MATCHING PATTERN "*.pure")
# pure_main.c is what batch-compiled programs (pure -c) link with. The
# batch compiler still drives gcc and is not supported with MSVC yet, so
# only the source is installed.
install(FILES pure_main.c DESTINATION ${CMAKE_INSTALL_LIBDIR}/pure)
install(FILES pure.1 DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)
# The DLLs of the libraries libpure and pure.exe are linked with (gmp,
# mpfr, ...) go next to them, where Windows looks for them. The vcpkg
# toolchain copies them into the build directory, so they are found there.
install(CODE [[
  file(GET_RUNTIME_DEPENDENCIES
    EXECUTABLES "$<TARGET_FILE:pure>"
    LIBRARIES "$<TARGET_FILE:libpure>"
    DIRECTORIES "$<TARGET_FILE_DIR:pure>"
    # The system's DLLs, and the Visual C++ runtime, which is installed
    # with the Visual C++ Redistributable.
    PRE_EXCLUDE_REGEXES "^api-ms-" "^ext-ms-" "^(vcruntime|msvcp)[0-9_]*\\.dll$"
    POST_EXCLUDE_REGEXES "[/\\][Ww][Ii][Nn][Dd][Oo][Ww][Ss][/\\]"
    RESOLVED_DEPENDENCIES_VAR deps)
  file(INSTALL ${deps} DESTINATION "${CMAKE_INSTALL_PREFIX}/bin")
]])
