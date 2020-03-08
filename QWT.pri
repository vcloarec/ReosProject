
DEFINES += QWT_DLL
INCLUDEPATH +=$${QWT_INCLUDE_PATH}


CONFIG(debug, debug|release){
    LIBS += $${QWT_LIB_FILE}
} else {
    LIBS += $${QWT_LIB_FILE}
}




