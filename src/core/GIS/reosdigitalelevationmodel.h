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

#include "reoscore.h"
#include "reosmemoryraster.h"
#include "reosprocess.h"

/**
 * The ReosDigitalElevationModel abstract class is an interface for Digital elevation model of all type (TIN, raster)
*/
class REOSCORE_EXPORT ReosDigitalElevationModel
{
  public:
    ReosDigitalElevationModel() = default;
    virtual ~ReosDigitalElevationModel() = default;

    //! Returns elevation value at \a point in DEM coordinate
    virtual double elevationAt( const QPointF &point, const QString &pointCrs = QString() ) const = 0;

    /**
     *  Returns a profile corresponding on the elevation on the DEM, resolution depends on the DEM type
     *
     * \param polyline the polyline that support the projection of elevation
     * \param polylineCrs is the CRS of \a polyline
     * \return a profile with distance in meters
     */
    virtual QPolygonF elevationOnPolyline( const QPolygonF &polyline, const QString &polylineCrs = QString(), ReosProcess *process = nullptr ) const = 0;

    /**
     *  Calculates and returns the average elevation
     *
     * \param polylgon the polygon in which the calculation is executed
     * \param polygonCrs is the CRS of \a polygon
     * \return a doublevalue corresponding to the average elevation
     */
    virtual double averageElevationInPolygon( const QPolygonF &polylgon, const QString &polygonCrs = QString(), ReosProcess *process = nullptr ) const = 0;

    /**
     *  Calculates and returns the average elevation based on a grid
     *
     * \param grid the grid represented by a raster memory of byte, 0 : do not take account the elevation, 1 : take account
     * \param rasterExtent the raster extent of the grid used to retrieve the world world coordinates
     * \return a doublevalue corresponding to the average elevation
     */
    virtual double averageElevationOnGrid( const ReosRasterMemory<unsigned char> &grid, const ReosRasterExtent &gridExtent, ReosProcess *process = nullptr ) const = 0;

    //! Returns the source of the DEM, if it is a map layer, returns the layer Id
    virtual QString source() const = 0;

    /**
     * Extract a memory raster with simple precision from the DEM in \a extent.
     * The resolution of the raster will depend on the DEM specification.
     * Resulting resolution and adjusted extent are stored in \a rasterExtent.
     * Destination coordinate reference system \a destinationCrs can be provided to override the extent crs
     * If destinantion crs and extent crs are invalid, the output will be in the same coordinate system as the souce of the DEM
     *
     */
    virtual ReosRasterMemory<float> extractMemoryRasterSimplePrecision(
      const ReosMapExtent &extent,
      ReosRasterExtent &rasterExtent,
      float &maxValue,
      const QString &destinationCrs = QString(), ReosProcess *process = nullptr ) const = 0;

    /**
     * Extract a memory raster with simple precision from the DEM in \a a rasterExtent.
     * Destination CRS, resolution and extent are stored in \a rasterExtent.
     * If destinantion crs the output will be in the same coordinate system as the souce of the DEM
     */
    virtual ReosRasterMemory<float> extractMemoryRasterSimplePrecision(
      const ReosRasterExtent &destinationRasterExtent,
      ReosProcess *process = nullptr ) const = 0;
};

//! Process class that extract elevation on a polyline from a digital elevation model
class REOSCORE_EXPORT ReosElevationOnPolylineProcess: public ReosProcess
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


//! Process that calculate the average elevation of the DEM in a watershed defined



#endif // REOSDIGITALELEVATIONMODEL_H
