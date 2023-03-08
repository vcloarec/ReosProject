/***************************************************************************
  reostopographycollection_p.h - ReosTopographyCollection_p

 ---------------------
 begin                : 28.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSTOPOGRAPHYCOLLECTION_P_H
#define REOSTOPOGRAPHYCOLLECTION_P_H

#define SIP_NO_FILE

#include "reostopographycollection.h"
#include "reosdigitalelevationmodel_p.h"
#include "qgscoordinatetransform.h"

class ReosDigitalElevationModelRaster;

class ReosTopographyCollection_p : public ReosTopographyCollection
{
    Q_OBJECT
  public:
    ReosTopographyCollection_p( ReosGisEngine *gisEngine, QObject *parent );
    ReosTopographyCollection_p( const ReosEncodedElement &element, ReosGisEngine *gisEngine, QObject *parent );

    void prepare_p( const QgsCoordinateReferenceSystem &sourceCrs ) const;
    double elevationAt_p( const QgsPointXY &point ) const;
    void clean_p() const;

  private:


    mutable std::vector<std::unique_ptr<ReosDigitalElevationModelRaster>> mDems;
    mutable QVector<QgsCoordinateTransform> mTransforms;

};

#endif // REOSTOPOGRAPHYCOLLECTION_P_H
