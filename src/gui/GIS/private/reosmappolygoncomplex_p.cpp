/***************************************************************************
  reosmappolygoncomplex_p.cpp - ReosMapPolygonComplex_p

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
#include "reosmappolygoncomplex_p.h"

#include <qgsmapcanvas.h>

#include "reosgeometrycomplex.h"
#include "reosmapextent.h"

ReosMapPolygonComplex_p::ReosMapPolygonComplex_p( QgsMapCanvas *canvas )
  : ReosMapItem_p( canvas )
{}

ReosMapItem_p *ReosMapPolygonComplex_p::clone()
{
  return nullptr;
}

QPointF ReosMapPolygonComplex_p::mapPos() const
{
  if ( mGeometry.isNull() )
    return QPointF();
  ReosMapExtent extent = mGeometry->extent( crs() );
  return QPointF( extent.xMapMin(), extent.yMapMin() );
}

void ReosMapPolygonComplex_p::updatePosition()
{
  if ( mGeometry.isNull() )
    return;

  prepareGeometryChange();
  ReosMapExtent extent = mGeometry->extent( crs() );

  QPointF tl = toCanvasCoordinates( QgsPointXY( extent.xMapMin(), extent.yMapMax() ) );
  QPointF br = toCanvasCoordinates( QgsPointXY( extent.xMapMax(), extent.yMapMin() ) );

  mBBox = QRectF( tl, br );
  mBBox = mBBox.normalized();
  mBBox = mBBox.adjusted( -5, -5, 5, 5 );
}

QRectF ReosMapPolygonComplex_p::boundingRect() const
{
  return mBBox;
}

void ReosMapPolygonComplex_p::setStructure( ReosGeometryComplex *structure )
{
  mGeometry = structure;
  updatePosition();
}

void ReosMapPolygonComplex_p::setHovered( const QgsPointXY &position )
{
  ReosMapItem_p::setHovered( position );
  mHoveredPosition = position.toQPointF();
}

void ReosMapPolygonComplex_p::clearHover()
{
  ReosMapItem_p::clearHover();
  mHoveredPosition = QPointF();
}

void ReosMapPolygonComplex_p::paint( QPainter *painter )
{
  if ( mGeometry.isNull() )
    return;
  QgsMapSettings &mapSettings = mMapCanvas->mapSettings();
  mGeometry->render( static_cast<void *>( &mapSettings ), painter, mIsHovered, mHoveredPosition );
}
