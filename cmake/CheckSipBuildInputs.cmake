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
