/***************************************************************************
  reospolylinesstructure_p.h - ReosPolylinesStructure_p

 ---------------------
 begin                : 10.1.2022
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
#ifndef REOSPOLYLINESSTRUCTURE_P_H
#define REOSPOLYLINESSTRUCTURE_P_H

#include <qgsvectorlayer.h>
#include "reospolylinesstructure.h"

class ReosPolylineStructureVectorLayer: public ReosPolylinesStructure
{
  public:
    ReosPolylineStructureVectorLayer( const QString &wktCrs );

    ReosPolylineStructureVectorLayer *clone() override;

    void addPolylines( const QPolygonF &polyline, const QString &sourceCrs = QString( ), const QString &id = QString() ) override;
    QPolygonF polyline( const QString &destinationCrs = QString(), const QString &id = QString() ) const override;

    QgsVectorLayer *data() override {return mVectorLayer.get();}

    virtual void removeAll() override;
    virtual void translate( const QPointF &translation, const QString &crs, const QString &id = QString() ) override {};
    virtual void moveVertex( int index, const ReosSpatialPosition &newPosition, const QString &id = QString() )override {};
    virtual void insertVertex( int index, const ReosSpatialPosition &point, const QString &id = QString() )override {};
    virtual void removeVertex( int index, const QString &id = QString() )override {}

  private:
    ReosPolylineStructureVectorLayer() = default;
    std::unique_ptr<QgsVectorLayer> mVectorLayer;
    QHash<QString, QgsFeatureId> mReosToQgisId;

};


#endif // REOSPOLYLINESSTRUCTURE_P_H
