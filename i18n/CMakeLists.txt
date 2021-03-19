find_package(Qt5 COMPONENTS LinguistTools REQUIRED)
set(QT_LRELEASE_EXECUTABLE Qt5::lrelease)

#macro(ADD_TRANSLATION_FILES _sources )
#    foreach (_current_FILE ${ARGN})
#      get_filename_component(_in ${_current_FILE} ABSOLUTE)
#      get_filename_component(_basename ${_current_FILE} NAME_WE)

#      set(_out ${REOS_OUTPUT_DIRECTORY}/i18n/${_basename}.qm)

#      add_custom_command(
#         OUTPUT ${_out}
#         COMMAND ${QT_LRELEASE_EXECUTABLE}
#         ARGS -silent ${_in} -qm ${_out}
#         DEPENDS ${_in}
#      )

#      set(${_sources} ${${_sources}} ${_out} )
#   endforeach (_current_FILE)
#endmacro(ADD_TRANSLATION_FILES)

## make sure the output directory exists
#file(MAKE_DIRECTORY ${QGIS_OUTPUT_DIRECTORY}/i18n)

#set(TS_FILES reos_fr.ts reos_es.ts)

#ADD_TRANSLATION_FILES (QM_FILES ${TS_FILES})

#if (UNIX AND NOT APPLE AND PYQT5_FOUND)
#  add_custom_command(
#    OUTPUT ${CMAKE_BINARY_DIR}/org.qgis.qgis.desktop ${CMAKE_BINARY_DIR}/org.qgis.qgis.appdata.xml
#    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
#    COMMAND ${PYTHON_EXECUTABLE}
#    ARGS ${CMAKE_SOURCE_DIR}/scripts/ts2appinfo.py "${CMAKE_BINARY_DIR}"
#    COMMENT "Updating appinfo files..."
#    DEPENDS ${QM_FILES} ${CMAKE_SOURCE_DIR}/linux/org.qgis.qgis.desktop.in ${CMAKE_SOURCE_DIR}/linux/org.qgis.qgis.appdata.xml.in
#  )

#  set(MD_FILES ${CMAKE_BINARY_DIR}/org.qgis.qgis.desktop ${CMAKE_BINARY_DIR}/org.qgis.qgis.appdata.xml)

#  install(FILES ${CMAKE_BINARY_DIR}/org.qgis.qgis.desktop DESTINATION share/applications)
#  install(FILES ${CMAKE_BINARY_DIR}/org.qgis.qgis.appdata.xml DESTINATION share/metainfo)
#endif()

## creating a custom target is needed to make the files build
## "ALL" means that it will be run by default
#add_custom_target (translations ALL DEPENDS ${QM_FILES})

## first compile sources, then compile translations
#if (WITH_DESKTOP)
#  add_dependencies (translations ${QGIS_APP_NAME})
#endif()

#install (FILES ${QM_FILES}
#         DESTINATION ${QGIS_DATA_DIR}/i18n)
