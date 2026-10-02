set(_sip_missing_input FALSE)
string(REPLACE "|" ";" _sip_diagnostic_files "${SIP_DIAGNOSTIC_FILES}")

message(STATUS "SIP build input diagnostics")

foreach(_sip_input IN LISTS _sip_diagnostic_files)
  if(EXISTS "${_sip_input}")
    message(STATUS "  found: ${_sip_input}")
  else()
    message(WARNING "  missing: ${_sip_input}")
    set(_sip_missing_input TRUE)
  endif()
endforeach()

if(_sip_missing_input)
  message(WARNING "One or more SIP build inputs are missing before running sip-build")
endif()

if(SIP_BUILD_EXECUTABLE)
  if(EXISTS "${SIP_BUILD_EXECUTABLE}")
    message(STATUS "  sip-build executable found: ${SIP_BUILD_EXECUTABLE}")
  else()
    message(WARNING "  sip-build executable missing: ${SIP_BUILD_EXECUTABLE}")
  endif()
endif()

if(SIP_QMAKE_EXECUTABLE)
  if(EXISTS "${SIP_QMAKE_EXECUTABLE}")
    message(STATUS "  qmake executable found: ${SIP_QMAKE_EXECUTABLE}")
    execute_process(
      COMMAND "${SIP_QMAKE_EXECUTABLE}" -query QT_VERSION
      RESULT_VARIABLE _sip_qmake_result
      OUTPUT_VARIABLE _sip_qmake_output
      ERROR_VARIABLE _sip_qmake_error
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_STRIP_TRAILING_WHITESPACE
    )
    message(STATUS "  qmake -query QT_VERSION result: ${_sip_qmake_result}")
    if(_sip_qmake_output)
      message(STATUS "  qmake QT_VERSION: ${_sip_qmake_output}")
    endif()
    if(_sip_qmake_error)
      message(WARNING "  qmake error: ${_sip_qmake_error}")
    endif()
  else()
    message(WARNING "  qmake executable missing: ${SIP_QMAKE_EXECUTABLE}")
  endif()
else()
  message(WARNING "  qmake executable was not passed to SIP diagnostics")
endif()
