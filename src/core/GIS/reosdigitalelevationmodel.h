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
#include "reosprocess.h"

/**
 * The ReosDigitalElevationModel abstract class is an interface for Digital elevation model of all type (TIN, raster)
*/
class ReosDigitalElevationModel
{
  public:
    ReosDigitalElevationModel() = default;
    virtual ~ReosDigitalElevationModel() = default;

    //! Returns elevation value at \a point in DEM coordinate
    virtual double elevationAt( const QPointF &point, const QString &destinationCrs = QString() ) const = 0;

    //! Returns a profile corresponding on the elevation on the DEM, resolution depends on the DEM type
    virtual QPolygonF elevationOnPolyline( const QPolygonF &polyline, const QString &destinationCrs = QString(), ReosProcess *process = nullptr ) const = 0;

    //! Returns the source of the DEM, if it is a map layer, returns the layer Id
    virtual QString source() const = 0;

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
      const QString &destinationCrs = QString(), ReosProcess *process = nullptr ) const = 0;
};

//! Process class that extract elevation on a polyline from a digital elevation model
class ReosElevationOnPolylineProcess: public ReosProcess
{
  public:
    //! Constructor
    ReosElevationOnPolylineProcess( ReosDigitalElevationModel *dem );

    //! Sets the entry \a polyline and eventually the destination CRS
    void setEntryPolyline( const QPolygonF &polyline, const QString destinationCRS = QString() );

    //! Returns the result profile after processing \see start()
    QPolygonF resultProfile() const;

    //! Start the processing
    void start() override;

  private:
    ReosDigitalElevationModel *mDem = nullptr;
    QPolygonF mPolyline;
    QString mDestinationCRS;
    QPolygonF mResult;

};



#endif // REOSDIGITALELEVATIONMODEL_H
