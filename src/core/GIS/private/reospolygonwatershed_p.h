/***************************************************************************
  reospolygonwatershed_p.h - ReosPolygonWatershed_p

---------------------
begin                : 26.8.2026
copyright            : (C) 2026 by Vincent Cloarec
email                : vcloarec at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************/

#ifndef REOSPOLYGONWATERSHED_P_H
#define REOSPOLYGONWATERSHED_P_H

#include <QPair>

#include <qgsgeometry.h>

#include "reospolygonwatershed.h"
#include "reosgeometrycomplex_p.h"
#include "reoswatershed.h"

class QgsSingleSymbolRenderer;

class ReosPolygonWatershed_p : public ReosPolygonWatershed, private ReosGeometryComplex_p
{
  public:
    ReosPolygonWatershed_p() = default;
    ReosPolygonWatershed_p( const QString &wktCrs );

    ReosPolygonWatershed *clone() const override;
    QObject *data() override;
    void addWatershed(const QPolygonF &watershed,
                       const QString &crs,
                      const QString &id,
                      ReosWatershed::Type type) override;
    void addWatershed( ReosWatershed *watershed ) override;
    void removeWatershed( const QString &id ) override;
    QPointF closestVertex( const QString &watershedId, const QPointF &position, const QString &destinationCrs, double tolerance, int &prevIndex, int &index, int &nextIndex ) const override;
    QPointF vertexPosition(const QString &watershedId, int index,const QString &destinationCrs) const override;
    ReosMapExtent watershedExtent( const QString &watershedId, const QString &destinationCrs ) const override;
    QPolygonF watershedDelineating( const QString &watershedId, const QString &destinationCrs ) const override;
    QString watershedUnderPosition( const QPointF &position, const QString &destinationCrs ) const override;
    bool canVertexBeMoved( const QString &watershedId, int index, const QPointF &destinationPosition, const QString &destinationCrs ) const override;
    void moveVertex(const QString &watershedId, int index, const QPointF &destinationPosition, const QString &destinationCrs) override;
    void clear() override;
    ReosMapExtent extent( const QString &crs ) const override;
    void setCrs( const QString &crs ) override;
    QString crs() const override;

    void render(void *mapSettings, QPainter *painter, bool highlight, const QPointF &highlightPosition) const override;

  private:
    QgsSingleSymbolRenderer *mRenderer=nullptr;

    //! <<watershedId,CRS>, geometry>
    mutable QPair<QPair<QString,QString>, QgsGeometry> mCacheGeometry;

   //! <CRS, geometry>
    mutable QPair<QString, QgsGeometry> mCacheHighlightedGeometry;

    QgsGeometry getGeometry( const QString &watershedId, const QString &destinationCrs ) const;

    QgsFeature getFeatureUnderPosition( const QPointF &position, const QString &destinationCrs, const QString &expression=QString() ) const;
};

#endif // REOSPOLYGONWATERSHED_P_H
