# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

add_subdirectory(hydroportail)

if (HDF5_METEO_TESTS_ENABLE)
    add_subdirectory(meteo-hdf5)
endif(HDF5_METEO_TESTS_ENABLE)

if (ERA5_TESTS_ENABLE)
    add_subdirectory(era5)
endif(ERA5_TESTS_ENABLE)

if (GRIB_TESTS_ENABLE)
    add_subdirectory(grib)
endif(GRIB_TESTS_ENABLE)

if (COMEPHORE_TESTS_ENABLE)
    add_subdirectory(comephore)
endif(COMEPHORE_TESTS_ENABLE)

if (METEOFRANCEAPI_TESTS_ENABLE)
    add_subdirectory(mf-api)
endif(METEOFRANCEAPI_TESTS_ENABLE)

#add_subdirectory(vortex-io)


