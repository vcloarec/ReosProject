# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

ADD_SUBDIRECTORY(grib)
IF (COMEPHORE_TESTS_ENABLE)
    ADD_SUBDIRECTORY(comephore)
ENDIF(COMEPHORE_TESTS_ENABLE)


