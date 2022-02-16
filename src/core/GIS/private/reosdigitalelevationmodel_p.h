/***************************************************************************
                      reosdigitalelevationmodel.h
                     --------------------------------------
Date                 : 01-10-2020
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

#ifndef REOSDIGITALELEVATIONMODEL_P_H
#define REOSDIGITALELEVATIONMODEL_P_H

#include <math.h>
#include <qgsrasterlayer.h>
#include <qgscoordinatetransformcontext.h>

#include "reosdigitalelevationmodel.h"

class ReosDigitalElevationModelRaster: public ReosDigitalElevationModel
{
  public:
    ReosDigitalElevationModelRaster( QgsRasterLayer *rasterLayer, const QgsCoordinateTransformContext &transformContext );

    double elevationAt( const QPointF &point, const QString &pointCrs = QString() ) const override;

    QPolygonF elevationOnPolyline( const QPolygonF &polyline, const QString &polylineCrs = QString(), ReosProcess *process = nullptr ) const override;


    double averageElevationInPolygon( const QPolygonF &polygon, const QString &polygonCrs, ReosProcess *process ) const override;
    double averageElevationOnGrid( const ReosRasterMemory<unsigned char> &grid, const ReosRasterExtent &gridExtent, ReosProcess *process = nullptr ) const override;

    ReosRasterMemory<float> extractMemoryRasterSimplePrecision( const ReosMapExtent &destinationExtent,
        ReosRasterExtent &outputRasterExtent,
        float &maxValue,
        const QString &destinationCrs = QString(), ReosProcess *process = nullptr ) const override;

    ReosRasterMemory<float> extractMemoryRasterSimplePrecision(
      const ReosRasterExtent &destinationRasterExtent,
      ReosProcess *process = nullptr ) const override;

    QString source() const override;

  private:
    std::unique_ptr<QgsRasterDataProvider> mDataProvider;
    QgsCoordinateReferenceSystem mCrs;
    QgsCoordinateTransformContext mTransformContext;
    QString mSourceId;
    ReosRasterExtent mExtent;

    //! Adjust the extent to the border of pixel of the raster (extent increase)
    ReosRasterExtent rasterExtent( const QgsRectangle &originalExtent ) const;

};


class ReosDigitalElevationModelFactory
{
  public:
    static ReosDigitalElevationModel *createDEM( QgsRasterLayer *rasterLayer, const QgsCoordinateTransformContext &transformContext )
    {
      return new ReosDigitalElevationModelRaster( rasterLayer, transformContext );
    }
};

#endif // REOSDIGITALELEVATIONMODEL_P_H
