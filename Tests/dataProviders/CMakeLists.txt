# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

add_subdirectory(grib)
if (COMEPHORE_TESTS_ENABLE)
    add_subdirectory(comephore)
endif(COMEPHORE_TESTS_ENABLE)


add_subdirectory(mf-api)
