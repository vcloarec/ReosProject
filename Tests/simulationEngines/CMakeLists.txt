# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

if (ENABLE_HECRAS AND ENABLE_HEC_DSS)
        add_subdirectory(hecras)
endif(ENABLE_HECRAS AND ENABLE_HEC_DSS)

add_subdirectory(telemac)
