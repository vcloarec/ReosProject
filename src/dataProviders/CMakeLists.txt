# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

add_subdirectory(hub-eau)
add_subdirectory(delft-FEWS)
if (HAS_ECCODES)
	add_subdirectory(grib)
endif (HAS_ECCODES)
add_subdirectory(comephore)
add_subdirectory(era5)
if (ENABLE_HEC_DSS)
	add_subdirectory(hec-dss)
endif (ENABLE_HEC_DSS)

add_subdirectory(meteofrance-api)
add_subdirectory(hydroportail)
add_subdirectory(vortex-io)

add_subdirectory(meteo-hdf5)
