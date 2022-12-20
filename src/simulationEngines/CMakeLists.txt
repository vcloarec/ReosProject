# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

add_subdirectory(telemac)

IF (ENABLE_HECRAS AND ENABLE_HEC_DSS)
	add_subdirectory(hecras)
ENDIF(ENABLE_HECRAS AND ENABLE_HEC_DSS)


