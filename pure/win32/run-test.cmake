# Run one Pure regression test the way run-test and run-tests do, for the
# CMake (MSVC) build: run the interpreter on the test script with
#   ./pure --norc -v7 [-n for the prelude] < test.pure
# in an environment with PURELIB=<src>/lib, PURE_INCLUDE=<src>/test,
# srcdir=<src>, and compare stdout+stderr with test/<name>.log. As with
# run-tests on MinGW (diff --strip-trailing-cr), carriage returns before a
# newline are ignored. On a mismatch the output is kept in
# <OUTDIR>/<name>.out and a diff in <OUTDIR>/<name>.diff.
#
#   cmake -DPURE=pure.exe -DSRCDIR=<src> -DTEST=<test.pure> -DOUTDIR=<dir>
#         [-DPURE_FLAGS="..."] -P run-test.cmake

foreach(v PURE SRCDIR TEST OUTDIR)
  if(NOT DEFINED ${v})
    message(FATAL_ERROR "run-test.cmake: ${v} is not set")
  endif()
endforeach()

get_filename_component(name ${TEST} NAME_WE)
set(log ${SRCDIR}/test/${name}.log)
file(MAKE_DIRECTORY ${OUTDIR})
set(out ${OUTDIR}/${name}.out)

set(ENV{srcdir} ${SRCDIR})
set(ENV{PURELIB} ${SRCDIR}/lib)
set(ENV{PURE_INCLUDE} ${SRCDIR}/test)
set(ENV{LC_ALL} C)
if(NOT DEFINED PURE_FLAGS AND DEFINED ENV{PURE_FLAGS})
  set(PURE_FLAGS "$ENV{PURE_FLAGS}")
endif()
if(DEFINED PURE_FLAGS)
  separate_arguments(flags NATIVE_COMMAND "${PURE_FLAGS}")
else()
  set(flags "")
endif()
if(name STREQUAL "prelude")
  list(APPEND flags -n)
endif()

# Output and diagnostics go to one file, in the order they were written,
# like 2>&1 in run-tests.
execute_process(COMMAND ${PURE} --norc -v7 ${flags}
  INPUT_FILE ${TEST}
  OUTPUT_FILE ${out} ERROR_FILE ${out}
  RESULT_VARIABLE rc)

file(READ ${out} got)
file(READ ${log} want)
string(REPLACE "\r\n" "\n" got "${got}")
string(REPLACE "\r\n" "\n" want "${want}")
if(got STREQUAL want)
  file(REMOVE ${out} ${OUTDIR}/${name}.diff)
  message(STATUS "${name}.pure: passed")
else()
  file(WRITE ${out} "${got}")
  find_program(DIFF diff)
  if(DIFF)
    execute_process(COMMAND ${DIFF} -u ${log} ${out}
      OUTPUT_FILE ${OUTDIR}/${name}.diff)
  endif()
  message(FATAL_ERROR "${name}.pure: FAILED (exit code ${rc}; output in ${out})")
endif()
