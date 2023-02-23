find_package(Qt5 COMPONENTS LinguistTools REQUIRED)
set(QT_LRELEASE_EXECUTABLE Qt5::lrelease)

macro(ADD_TRANSLATION_FILES _sources )
    foreach (_current_FILE ${ARGN})
      get_filename_component(_in ${_current_FILE} ABSOLUTE)
      get_filename_component(_basename ${_current_FILE} NAME_WE)

      set(_out ${REOS_OUTPUT_DIRECTORY}/i18n/${_basename}.qm)

      add_custom_command(
         OUTPUT ${_out}
         COMMAND ${QT_LRELEASE_EXECUTABLE}
         ARGS -silent ${_in} -qm ${_out}
         DEPENDS ${_in}
      )

      set(${_sources} ${${_sources}} ${_out} )
   endforeach (_current_FILE)
endmacro(ADD_TRANSLATION_FILES)

## make sure the output directory exists
file(MAKE_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/i18n)

set(TS_FILES reos_en.ts reos_fr.ts reos_es.ts reos_it.ts reos_ne.ts reos_pt.ts reos_zh_TW.ts)

ADD_TRANSLATION_FILES (QM_FILES ${TS_FILES})

# creating a custom target is needed to make the files build
# "ALL" means that it will be run by default
add_custom_target (translations ALL DEPENDS ${QM_FILES})

# first compile sources, then compile translations
add_dependencies (translations ${LEKAN_APP_NAME})

install (FILES ${QM_FILES} DESTINATION i18n)
