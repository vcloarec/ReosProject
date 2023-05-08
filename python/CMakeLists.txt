file (MAKE_DIRECTORY ${REOS_PYTHON_OUTPUT_DIRECTORY})
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${REOS_PYTHON_OUTPUT_DIRECTORY})
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${REOS_PYTHON_OUTPUT_DIRECTORY})


# Macro to byte-compile a target's staged Python resource(s)
macro(PY_COMPILE TARGET_NAME RESOURCE_PATHS)
  if(WITH_PY_COMPILE)
    add_custom_command(TARGET ${TARGET_NAME}
      POST_BUILD
      COMMAND ${Python_EXECUTABLE} -m compileall -q ${RESOURCE_PATHS}
      WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
      COMMENT "Byte-compiling staged resource..."
    )
  endif()
endmacro(PY_COMPILE)

include_directories(SYSTEM
  ${Python_INCLUDE_DIRS}
  ${SIP_INCLUDE_DIR}
  ${QT_QTCORE_INCLUDE_DIR}
  ${QT_QTGUI_INCLUDE_DIR}
  ${QT_QTNETWORK_INCLUDE_DIR}
  ${QT_QTSVG_INCLUDE_DIR}
  ${QT_QTXML_INCLUDE_DIR}
  ${QTKEYCHAIN_INCLUDE_DIR}
  ${SQLITE3_INCLUDE_DIR}
  ${SPATIALINDEX_INCLUDE_DIR}
)

if (${SIP_VERSION_STR} VERSION_LESS 5)
  include_directories(SYSTEM
    ${SIP_INCLUDE_DIR}
  )
endif()

include_directories(
  ${CMAKE_BINARY_DIR}  # qgsconfig.h, qgsversion.h
  ${CMAKE_BINARY_DIR}/src/ui
)

set(REOS_PYTHON_DIR ${CMAKE_INSTALL_PREFIX}/python/reos)

file(GLOB_RECURSE sip_files_core core/*.sip core/*.sip.in)
set(SIP_EXTRA_FILES_DEPEND ${sip_files_core})
set(SIP_EXTRA_OPTIONS ${PYQT_SIP_FLAGS} -g -o -a ${CMAKE_BINARY_DIR}/python/reos.core.api)
if((${SIP_VERSION_STR} VERSION_EQUAL 4.19.11) OR (${SIP_VERSION_STR} VERSION_GREATER 4.19.11))
  set(SIP_EXTRA_OPTIONS ${SIP_EXTRA_OPTIONS} -n ${PYQT_SIP_IMPORT})
endif()
if(SIP_BUILD_EXECUTABLE)
  install(FILES ${CMAKE_BINARY_DIR}/python/core/build/_core/_core.pyi DESTINATION ${REOS_PYTHON_DIR})
elseif((${SIP_VERSION_STR} VERSION_EQUAL 4.18) OR (${SIP_VERSION_STR} VERSION_GREATER 4.18))
  set(SIP_EXTRA_OPTIONS ${SIP_EXTRA_OPTIONS} -y ${REOS_PYTHON_OUTPUT_DIRECTORY}/_core.pyi)
  install(FILES ${REOS_PYTHON_OUTPUT_DIRECTORY}/_core.pyi DESTINATION ${REOS_PYTHON_DIR})
endif()

if((${PYQT_VERSION_STR} VERSION_EQUAL 5.15) OR (${PYQT_VERSION_STR} VERSION_GREATER 5.15))
  set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} VECTOR_MAPPED_TYPE)
endif()

set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} HAVE_GUI)
set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} ANDROID)
set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} ARM)

SET(QSCI_SIP_MOD_NAME Qsci/qscimod5.sip)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/core/project.py.in ${CMAKE_CURRENT_BINARY_DIR}/core/project.py @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/core/pyproject.toml.in ${CMAKE_CURRENT_BINARY_DIR}/core/pyproject.toml @ONLY)
message("********************************************* sip files")
message(STATUS ${sip_files_core})
GENERATE_SIP_PYTHON_MODULE_CODE(reos._core core/core.sip "${sip_files_core}" cpp_files)
BUILD_SIP_PYTHON_MODULE(reos._core core/core.sip ${cpp_files} "" reosCore)
set(SIP_CORE_CPP_FILES ${cpp_files})

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # Bad hack to fix compilation with gcc 11 - for some reason it's ignoring
  # the c++ standard version set for the target in BUILD_SIP_PYTHON_MODULE!
  add_definitions(-std=c++14)
endif()

if((UNIX OR MINGW) AND SIP_VERSION_STR VERSION_GREATER_EQUAL 4.10 AND SIP_VERSION_STR VERSION_LESS 5.0)
  set(SIP_EXTRA_OPTIONS -P ${SIP_EXTRA_OPTIONS})
  add_definitions(-Dprotected=public)
elseif((UNIX OR MINGW) AND SIP_VERSION_STR VERSION_GREATER_EQUAL 5.0)
  add_definitions(-Dprotected=public)
endif()

# Plugin utilities files to copy to staging or install
set(PY_FILES
  __init__.py
)

install(FILES ${PY_FILES} DESTINATION "${REOS_PYTHON_DIR}")

SET (PY_MODULES core)

foreach(module ${PY_MODULES})
  add_custom_target(py${module} ALL)
  add_dependencies(py${module} python_module_reos__${module})

  # concat auto_additions/*.py in _module_/__init__.py
  file(GLOB PY_FILES_AUTO_ADDITIONS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${module}/auto_additions/*.py)
  set(OUTPUT_DIR ${REOS_PYTHON_OUTPUT_DIRECTORY}/${module})
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${module}/__init__.py.in ${OUTPUT_DIR}/__init__.py COPYONLY)
  foreach(pyfile ${PY_FILES_AUTO_ADDITIONS})
    file(READ ${pyfile} CONTENTS)
    file(APPEND ${OUTPUT_DIR}/__init__.py "${CONTENTS}")
  endforeach(pyfile)
  install(FILES ${OUTPUT_DIR}/__init__.py DESTINATION "${REOS_PYTHON_DIR}/${module}")

  file(GLOB PY_FILES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${module}/*.py)
  install(FILES ${PY_FILES} DESTINATION "${REOS_PYTHON_DIR}/${module}")
  file(GLOB PY_FILES_ADDITIONS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${module}/additions/*.py)
  install(FILES ${PY_FILES_ADDITIONS} DESTINATION "${REOS_PYTHON_DIR}/${module}/additions")
  set(PY_FILES ${PY_FILES} ${PY_FILES_ADDITIONS})
  foreach(pyfile ${PY_FILES})
    get_filename_component(subdir ${pyfile} DIRECTORY)
    add_custom_command(TARGET py${module}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory "${REOS_PYTHON_OUTPUT_DIRECTORY}/${subdir}"
      COMMAND ${CMAKE_COMMAND} -E copy ${pyfile} "${REOS_PYTHON_OUTPUT_DIRECTORY}/${subdir}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      DEPENDS ${PY_FILES}
    )
  endforeach(pyfile)
  PY_COMPILE(py${module} "${REOS_PYTHON_OUTPUT_DIRECTORY}/${module}")

  # install source sip files
  if(SIP_GLOBAL_INSTALL)
    file(GLOB sip_files ${CMAKE_CURRENT_BINARY_DIR}/${module}/*.sip)
    install(FILES ${sip_files} DESTINATION ${SIP_DEFAULT_SIP_DIR}/reos/${module})
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${module}/auto_generated DESTINATION ${SIP_DEFAULT_SIP_DIR}/reos/${module})
  endif()
endforeach(module)


