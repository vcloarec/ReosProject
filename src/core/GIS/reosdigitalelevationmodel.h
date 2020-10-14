/***************************************************************************
                      reosdigitalelevationmodel.h
                     --------------------------------------
Date                 : 27-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSDIGITALELEVATIONMODEL_H
#define REOSDIGITALELEVATIONMODEL_H

#include <QString>

#include "reosmemoryraster.h"

class ReosDigitalElevationModel
{
  public:
    ReosDigitalElevationModel();
    ~ReosDigitalElevationModel();

    //! Returns elevation value at \a point in DEM coordinate
    virtual double elevationAt( const QPointF &point, const QString &destinationCrs = QString() ) = 0;

    /**
     * Extract a memory raster with simple precision from the DEM in \a extent.
     * The resolution of the raster will depend on the DEM specification.
     * Resolution and adjusted extent is stored in \a rasterExtent.
     * Destination coordinate reference system \a destinationCrs can be provided to override the extent crs
     * If destinantion crs and extent crs are invalid, the output will be in the same coordinate system as the souce of the DEM
     *
     */
    virtual ReosRasterMemory<float> extractMemoryRasterSimplePrecision(
      const ReosMapExtent &extent,
      ReosRasterExtent &rasterExtent,
      const QString &destinationCrs = QString() ) = 0;

  private:

};


#endif // REOSDIGITALELEVATIONMODEL_H
