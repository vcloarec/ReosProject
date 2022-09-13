/***************************************************************************
  reosmappolygonstructure_p.cpp - ReosMapPolygonStructure_p

 ---------------------
 begin                : 6.2.2022
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
#include "reosmappolygonstructure_p.h"

#include <qgsmapcanvas.h>

#include "reosmapextent.h"
#include "reospolygonstructure.h"
#include "qgsmaplayerrenderer.h"

ReosMapPolygonStructure_p::ReosMapPolygonStructure_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{}

ReosMapItem_p *ReosMapPolygonStructure_p::clone()
{
  return nullptr;
}

QPointF ReosMapPolygonStructure_p::mapPos() const
{
  if ( mStructure.isNull() )
    return QPointF();
  ReosMapExtent extent = mStructure->extent( crs() );
  return QPointF( extent.xMapMin(), extent.yMapMin() );
}

void ReosMapPolygonStructure_p::updatePosition()
{
  if ( mStructure.isNull() )
    return;

  prepareGeometryChange();
  ReosMapExtent extent = mStructure->extent( crs() );

  QPointF tl = toCanvasCoordinates( QgsPointXY( extent.xMapMin(), extent.yMapMax() ) );
  QPointF br = toCanvasCoordinates( QgsPointXY( extent.xMapMax(), extent.yMapMin() ) );

  mBBox = QRectF( tl, br );
  mBBox = mBBox.normalized();
  mBBox = mBBox.adjusted( - 5, -5, 5, 5 );
}

QRectF ReosMapPolygonStructure_p::boundingRect() const
{
  return mBBox;
}

void ReosMapPolygonStructure_p::setStructure( ReosPolygonStructure *structure )
{
  mStructure = structure;
  updatePosition();
}

void ReosMapPolygonStructure_p::paint( QPainter *painter )
{
  if ( mStructure.isNull() )
    return;

  QgsVectorLayer *vectorLayer = qobject_cast<QgsVectorLayer *>( mStructure->data() );

  if ( vectorLayer )
  {
    QgsRenderContext renderContext = QgsRenderContext::fromMapSettings( mMapCanvas->mapSettings() );
    renderContext.setPainter( painter );
    renderContext.setCoordinateTransform( mMapCanvas->mapSettings().layerTransform( vectorLayer ) );
    std::unique_ptr<QgsMapLayerRenderer> renderer;
    renderer.reset( vectorLayer->createMapRenderer( renderContext ) );
    renderer->render();
  }
}
