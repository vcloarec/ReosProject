file (MAKE_DIRECTORY ${QGIS_PYTHON_OUTPUT_DIRECTORY})
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${QGIS_PYTHON_OUTPUT_DIRECTORY})
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${QGIS_PYTHON_OUTPUT_DIRECTORY})


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

if(NOT ENABLE_TESTS)
  set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} TESTS)
endif()

if(NOT ANDROID)
  set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} ANDROID)
endif()

if(CMAKE_SYSTEM_PROCESSOR MATCHES "^arm")
else()
  set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} ARM)
endif()

SET(QSCI_SIP_MOD_NAME Qsci/qscimod6.sip)
SET(BINDING_FILES_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/PyQt6)
SET(SIP_SOURCE_ROOT_DIR ${BINDING_FILES_ROOT_DIR})
 SET(pyqtdist "PyQt6")

set(REOS_PYTHON_DIR ${CMAKE_INSTALL_PREFIX}/python/qgis)

# core module
file(GLOB_RECURSE sip_files_core ${BINDING_FILES_ROOT_DIR}/core/*.sip ${BINDING_FILES_ROOT_DIR}/core/*.sip.in)
set(SIP_EXTRA_FILES_DEPEND ${sip_files_core})
install(FILES ${CMAKE_BINARY_DIR}/python/core/build/_core/_core.pyi DESTINATION ${REOS_PYTHON_DIR})

set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} VECTOR_MAPPED_TYPE)

# sip 6.11 has support for the Movable Mapped Type Annotation
# Including the annotation with older sip versions will throw a parser error,
# so we need to generate a file without it for older versions
if(${SIP_VERSION_STR} VERSION_LESS 6.11.0)
  set(SIP_DISABLE_FEATURES ${SIP_DISABLE_FEATURES} MOVABLE_MAPPED_TYPE)
else()
  set(SIP_MOVABLE_STR "Movable,")
endif()
if(Qt6Core_VERSION_MINOR LESS 5)
# https://github.com/qgis/QGIS/issues/57760
# https://www.riverbankcomputing.com/pipermail/pyqt/2024-June/045923.html
set(SIP_QLIST_QINT64 [[
%MappedType QList<qint64>
    /TypeHintIn="Iterable[int]",
    TypeHintOut="List[int]", TypeHintValue="[]"/
{
%TypeHeaderCode
#include <QList>
    %End

    %ConvertFromTypeCode
        // Create the list.
        PyObject *l;

if ((l = PyList_New(sipCpp->size())) == NULL)
    return NULL;

// Set the list elements.
QList<qint64>::iterator it = sipCpp->begin();
for (int i = 0; it != sipCpp->end(); ++it, ++i)
{
    PyObject *tobj;

    if ((tobj = PyLong_FromLongLong(*it)) == NULL)
    {
      Py_DECREF(l);
      return NULL;
    }
    PyList_SET_ITEM(l, i, tobj);
}

return l;
%End

    %ConvertToTypeCode
    // Check the type if that is all that is required.
    if (sipIsErr == NULL)
    return PyList_Check(sipPy);

QList<qint64> *qlist = new QList<qint64>;

for (int i = 0; i < PyList_GET_SIZE(sipPy); ++i)
{
    *qlist << PyLong_AsLongLong(PyList_GET_ITEM(sipPy, i));
}

*sipCppPtr = qlist;
return sipGetState(sipTransferObj);
%End
};
]])
endif()

# Deprecated annotation supports message only since version 6.9.0 with abi-version 12.16 / 13.9 and above
set(sipproject "
[tool.sip.project]")

if(NOT ${SIP_VERSION_STR} VERSION_LESS 6.9.0)
  string(APPEND sipproject "\nabi-version = \"13.9\"")
endif()

set(toml "
# Specify the build system.
[build-system]
requires = [\"sip >=5.0.0, <7\", \"PyQt-builder >=1.6, <2\"]
build-backend = \"sipbuild.api\"
${sipproject}

")

if(${SIP_VERSION_STR} VERSION_LESS 6.8.0)
  set(toml "${toml}
[tool.sip.metadata]
name = \"REOS\"
version = \"${COMPLETE_API_VERSION}\"
summary = \"Python bindings for QGIS\"
home-page = \"\"
author = \"Vincent Cloarec\"
license = \"GPL v2\"
requires-dist = \"${pyqtdist}\"
")
else()
  if(${SIP_VERSION_STR} VERSION_LESS 6.13.0)
    set(project_license "license = {text = \"GPL v2 or later\"}")
  else()
    set(project_license "license = \"GPL-2.0-or-later\"\nlicense-files = [\"LICENSE_LEKAN.txt\"]")
  endif()

  set(toml "${toml}
[project]
name = \"REOS\"
version = \"${COMPLETE_API_VERSION}\"
description = \"Python bindings for REOS\"
authors = [ \{name = \"Vincent Cloarec\"\} ]
${project_license}
dependencies = [\"${pyqtdist}\"]

[project.urls]
Homepage = \"\"
")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/PyQt6/core/project.py.in ${CMAKE_CURRENT_BINARY_DIR}/core/project.py @ONLY)
configure_file(${CMAKE_SOURCE_DIR}/LICENSE_LEKAN.txt ${CMAKE_CURRENT_BINARY_DIR}/core/LICENSE_LEKAN.txt COPYONLY)
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/core/pyproject.toml.temp" "${toml}")
execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/core/pyproject.toml.temp ${CMAKE_CURRENT_BINARY_DIR}/core/pyproject.toml)
GENERATE_SIP_PYTHON_MODULE_CODE(reos._core core/core.sip "${sip_files_core}" cpp_files)
BUILD_SIP_PYTHON_MODULE(reos._core core/core.sip "" reosCore)
set(SIP_CORE_CPP_FILES ${cpp_files})

if(UNIX OR MINGW)
  add_definitions(-Dprotected=public)
endif()

set (PY_MODULES core)

# Plugin utilities files to copy to staging or install
set(PY_FILES
  ${BINDING_FILES_ROOT_DIR}/__init__.py
)

add_custom_target(pyutils ALL DEPENDS ${PY_FILES})
install(FILES ${PY_FILES} DESTINATION "${REOS_PYTHON_DIR}")

# stage to output to make available when QGIS is run from build directory
foreach(pyfile ${PY_FILES})
  get_filename_component(pyfile_name ${pyfile} NAME)
  add_custom_command(TARGET pyutils
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${REOS_PYTHON_OUTPUT_DIRECTORY}"
    COMMAND ${CMAKE_COMMAND} -E copy ${pyfile} "${REOS_PYTHON_OUTPUT_DIRECTORY}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  PY_COMPILE(pyutils "${REOS_PYTHON_OUTPUT_DIRECTORY}/${pyfile_name}")
endforeach(pyfile)

foreach(module ${PY_MODULES})
  set(module_out ${module})
  if(${module} EQUAL "3d")
    set(module_out "3d_p")
  endif()
  set(module_name ${module})
  if(${module} EQUAL "3d")
    set(module_name "_3d")
  endif()
  add_custom_target(py${module} ALL)
  add_dependencies(py${module} python_module_reos__${module_out})

  # to let ccache find auto_generated
  include_directories(${CMAKE_BINARY_DIR}/python/${module})

  # concat auto_additions/*.py in _module_/__init__.py
  file(GLOB PY_FILES_AUTO_ADDITIONS ${BINDING_FILES_ROOT_DIR}/${module}/auto_additions/*.py)
  set(MONKEYPATCH_INJECTIONS "")
  foreach(pyfile ${PY_FILES_AUTO_ADDITIONS})
    file(READ ${pyfile} CONTENTS)
    string(APPEND MONKEYPATCH_INJECTIONS "${CONTENTS}")
  endforeach(pyfile)
  configure_file(${BINDING_FILES_ROOT_DIR}/${module}/__init__.py.in ${REOS_PYTHON_OUTPUT_DIRECTORY}/${module_name}/__init__.py @ONLY)
  install(FILES ${REOS_PYTHON_OUTPUT_DIRECTORY}/${module_name}/__init__.py DESTINATION "${REOS_PYTHON_DIR}/${module_name}")

  file(GLOB PY_FILES ${BINDING_FILES_ROOT_DIR}/${module}/*.py)
  install(FILES ${PY_FILES} DESTINATION "${REOS_PYTHON_DIR}/${module_name}")
  file(GLOB PY_FILES_ADDITIONS ${BINDING_FILES_ROOT_DIR}/${module}/additions/*.py)

  install(FILES ${PY_FILES_ADDITIONS} DESTINATION "${REOS_PYTHON_DIR}/${module_name}/additions")
  set(PY_FILES ${PY_FILES} ${PY_FILES_ADDITIONS})
  foreach(pyfile ${PY_FILES})
    file(RELATIVE_PATH rel_pyfile ${BINDING_FILES_ROOT_DIR} ${pyfile})
    get_filename_component(subdir ${rel_pyfile} DIRECTORY)

    add_custom_command(TARGET py${module}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory "${REOS_PYTHON_OUTPUT_DIRECTORY}/${subdir}"
      COMMAND ${CMAKE_COMMAND} -E copy ${pyfile} "${REOS_PYTHON_OUTPUT_DIRECTORY}/${subdir}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      # DEPENDS ${PY_FILES}
    )
  endforeach(pyfile)
  PY_COMPILE(py${module} "${REOS_PYTHON_OUTPUT_DIRECTORY}/${module_name}")

  # install class map file
  install(FILES ${BINDING_FILES_ROOT_DIR}/${module}/class_map.yaml DESTINATION "${REOS_PYTHON_DIR}/${module_name}")

  # install source sip files
  if(SIP_GLOBAL_INSTALL)
    file(GLOB sip_files ${CMAKE_CURRENT_BINARY_DIR}/${module}/*.sip)
    install(FILES ${sip_files} DESTINATION ${SIP_DEFAULT_SIP_DIR}/reos/${module})
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${module}/auto_generated DESTINATION ${SIP_DEFAULT_SIP_DIR}/reos/${module})
  endif()

  if(SIP_BUILD_EXECUTABLE)
    add_custom_command(TARGET py${module}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/${module}/build/_${module_out}/_${module_out}.pyi" "${REOS_PYTHON_OUTPUT_DIRECTORY}"
    )
  endif()
endforeach(module)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/METADATA.in ${CMAKE_CURRENT_BINARY_DIR}/METADATA @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/METADATA DESTINATION "${REOS_PYTHON_INSTALL_DIR}/reos-${COMPLETE_API_VERSION}.dist-info/")
