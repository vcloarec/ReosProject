/***************************************************************************
                      reosmappolygon.cpp
                     --------------------------------------
Date                 : 17-09-2020
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

#include "reosmappolygon_p.h"

#include <QPainter>
#include <QVector2D>
#include <qgspoint.h>
#include <qgsmapcanvas.h>
#include <qgslinestring.h>
#include <reosmapextent.h>

#include "reospolylinesstructure.h"

ReosMapPolygon_p::ReosMapPolygon_p( QgsMapCanvas *canvas ):
  ReosMapPolygonBase_p( canvas )
{
}

ReosMapPolygon_p *ReosMapPolygon_p::clone()
{
  std::unique_ptr<ReosMapPolygon_p> other( new ReosMapPolygon_p( this ) );
  other->updatePosition();
  return other.release();
}

ReosMapPolygonBase_p::ReosMapPolygonBase_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{}

QRectF ReosMapPolygonBase_p::boundingRect() const
{
  QRectF bb = mViewPolygon.boundingRect();
  if ( externalWidth > width )
    return bb.adjusted( -externalWidth, -externalWidth, externalWidth, externalWidth );
  else
    return bb.adjusted( -width, -width, width, width );
}

void ReosMapPolygonBase_p::updatePosition()
{
  prepareGeometryChange();
  mViewPolygon.clear();
  const QPolygonF mapPoly = geometry();
  if ( mapPoly.count() < 1 )
    return;
  const QPointF pview0 = toCanvasCoordinates( QgsPoint( mapPoly.at( 0 ) ) );

  for ( auto &p : mapPoly )
  {
    const QPointF pview = toCanvasCoordinates( QgsPoint( p ) );
    mViewPolygon.append( QPointF( pview.x() - pview0.x(), pview.y() - pview0.y() ) );
  }
  setPos( pview0 );

  if ( mSegmentMarker >= 0 )
    mMarkerPositionOnView = toCanvasCoordinates( QgsPoint( mMarkerposition ) );
}

QPainterPath ReosMapPolygonBase_p::shape() const
{
  QPainterPath path;
  path.addPolygon( mViewPolygon );
  path.closeSubpath();
  return path ;
}

void ReosMapPolygonBase_p::setEditing( bool b )
{
  mIsEditing = b;
  update();
}

int ReosMapPolygonBase_p::findVertexInView( const QRectF &zone ) const
{
  for ( int i = 0; i < mViewPolygon.size(); ++i )
    if ( zone.contains( mViewPolygon.at( i ) + pos() ) )
      return i;

  return -1;
}

void ReosMapPolygonBase_p::activeMarker( bool b )
{
  mIsMarkerActive = b;
}

void ReosMapPolygonBase_p::setMarkerDistance( double d )
{
  const QPolygonF mapPoly = geometry();

  if ( !mIsMarkerActive || d < 0 || mapPoly.empty() )
  {
    mSegmentMarker = -1;
    updatePosition();
    return;
  }

  double distFromBegin = 0;
  int i = 0;
  do
  {
    const QPointF &p1 = mapPoly.at( i );
    const QPointF &p2 = mapPoly.at( i + 1 );
    distFromBegin += sqrt( pow( p1.x() - p2.x(), 2 ) + pow( p1.y() - p2.y(), 2 ) );
  }
  while ( d >= distFromBegin && ++i < mapPoly.count() - 1 );

  if ( d > distFromBegin )
  {
    mSegmentMarker = -1;
    updatePosition();
    return;
  }

  if ( i >= mapPoly.count() - 1 )
  {
    mSegmentMarker = -1;
    updatePosition();
    return;
  }
  mSegmentMarker = i;
  const QPointF &p1 = mapPoly.at( i );
  const QPointF &p2 = mapPoly.at( i + 1 );
  double segDist = sqrt( pow( p1.x() - p2.x(), 2 ) + pow( p1.y() - p2.y(), 2 ) );
  double distFromP2 = distFromBegin - d;
  double ratio = distFromP2 / segDist;
  mMarkerposition = p2 - ratio * ( p2 - p1 );
  updatePosition();
}

void ReosMapPolygonBase_p::setMarkerArrow( bool b )
{
  mMarkerArrow = b;
}

void ReosMapPolygonBase_p::paint( QPainter *painter )
{
  painter->save();
  QPen pen;
  pen.setJoinStyle( Qt::RoundJoin );

  QVector2D dir;
  QVector2D normDir;
  double arrowSize = 2 * width;
  if ( mSegmentMarker > -1 )
  {
    dir = QVector2D( mViewPolygon.at( mSegmentMarker + 1 ) - mViewPolygon.at( mSegmentMarker ) );
    dir.normalize();
    normDir = QVector2D( dir.y(), -dir.x() );
  }

  QColor colorToApply = isHovered ? color.lighter( 150 ) : color;
  QColor externalColorToApply = isHovered ? externalColor.lighter( 150 ) : externalColor;


  if ( externalWidth > width )
  {
    pen.setWidthF( externalWidth );
    pen.setColor( externalColorToApply );
    QBrush brush( Qt::NoBrush );
    painter->setBrush( brush );
    painter->setPen( pen );
    draw( painter );

    if ( mSegmentMarker >= 0 )
    {
      QBrush brush( Qt::SolidPattern );
      brush.setColor( externalColorToApply );
      pen.setWidthF( externalWidth );
      painter->setBrush( brush );

      if ( mMarkerArrow )
      {
        QVector2D dir;
        QVector2D normDir;
        dir = QVector2D( mViewPolygon.at( mSegmentMarker + 1 ) - mViewPolygon.at( mSegmentMarker ) );
        dir.normalize();
        normDir = QVector2D( dir.y(), -dir.x() );
        QPolygonF arrow( 3 );
        QPointF arrowPos = mMarkerPositionOnView - dir.toPointF() * arrowSize - pos();
        arrow[0] =  arrowPos + dir.toPointF() * arrowSize * 2;
        arrow[1] = arrowPos + normDir.toPointF() * arrowSize;
        arrow[2] = arrowPos - normDir.toPointF() * arrowSize;
        painter->setPen( pen );
        painter->drawPolygon( arrow );
      }

      pen.setStyle( Qt::SolidLine );
    }

  }

  pen.setWidthF( width );
  pen.setColor( colorToApply );
  pen.setStyle( style );
  QBrush brush( brushStyle );
  brush.setColor( fillColor );
  painter->setBrush( brush );
  painter->setPen( pen );
  draw( painter );

  if ( mIsEditing )
  {
    pen.setWidthF( 1 );
    QBrush brush( Qt::SolidPattern );
    brush.setColor( externalColor );
    painter->setBrush( brush );
    painter->setPen( pen );

    for ( const QPointF &pt : std::as_const( mViewPolygon ) )
    {
      QRectF rect( pt - QPointF( externalWidth, externalWidth ), QSizeF( externalWidth * 2, externalWidth * 2 ) );
      painter->drawRect( rect );
    }

    pen.setColor( color );
    brush.setColor( color );
    painter->setBrush( brush );
    painter->setPen( pen );
    for ( const QPointF &pt : std::as_const( mViewPolygon ) )
    {
      QRectF rect( pt - QPointF( width, width ), QSizeF( width * 2, width * 2 ) );
      painter->drawRect( rect );
    }

  }

  if ( mSegmentMarker >= 0 )
  {
    QBrush brush( Qt::SolidPattern );
    brush.setColor( colorToApply );
    painter->setPen( pen );
    painter->setBrush( brush );
    brush.setColor( colorToApply );

    if ( mMarkerArrow )
    {
      QPolygonF arrow( 3 );
      QPointF arrowPos = mMarkerPositionOnView - dir.toPointF() * arrowSize - pos();
      arrow[0] =  arrowPos + dir.toPointF() * arrowSize * 2;
      arrow[1] = arrowPos + normDir.toPointF() * arrowSize;
      arrow[2] = arrowPos - normDir.toPointF() * arrowSize;
      painter->setPen( pen );
      painter->drawPolygon( arrow );
    }
    else
    {
      pen.setWidthF( width / 2 );
      pen.setColor( externalColorToApply );
      painter->setPen( pen );
      painter->drawEllipse( mMarkerPositionOnView - pos(), externalWidth, externalWidth );
    }

    pen.setStyle( Qt::SolidLine );
  }


  painter->restore();
}

ReosMapPolygonBase_p::ReosMapPolygonBase_p( ReosMapPolygonBase_p *other ): ReosMapItem_p( mMapCanvas )
{
  color = other->color;
  externalColor  = other->externalColor;
  width = other->width;
  externalWidth = other->externalWidth;
  style = other->style;
  brushStyle = other->brushStyle;
  fillColor = other->fillColor;
}

void ReosMapPolygonBase_p::draw( QPainter *painter )
{
  painter->drawPolygon( mViewPolygon );
}

void ReosMapPolygon_p::translate( const QPointF &translation )
{
  QPolygonF newPoints( mMapPolygon.size() );

  for ( int i = 0; i < mMapPolygon.size(); ++i )
    newPoints[i] = mMapPolygon.at( i ) + translation;

  mMapPolygon = newPoints;
  updatePosition();
}

QPointF ReosMapPolygon_p::mapPos() const
{
  if ( mMapPolygon.isEmpty() )
    return QPointF();
  else
    return mMapPolygon.at( 0 );
}


void ReosMapPolygon_p::setGeometry( const QPolygonF &geom )
{
  mMapPolygon = geom;
  if ( mMapPolygon.isEmpty() )
    mSegmentMarker = -1;

  updatePosition();
}

QPolygonF ReosMapPolygon_p::geometry() const
{
  return mMapPolygon;
}

void ReosMapPolygon_p::moveVertex( int index, const QPointF &newPosition )
{
  if ( index < 0 || index >= mMapPolygon.count() )
    return;

  mMapPolygon.replace( index, newPosition );
  updatePosition();
}

void ReosMapPolygon_p::insertVertex( int index, const QPointF &point )
{
  mMapPolygon.insert( index, point );
  updatePosition();
}

void ReosMapPolygon_p::removeVertex( int index )
{
  mMapPolygon.removeAt( index );
  updatePosition();
}

ReosMapPolygon_p::ReosMapPolygon_p( ReosMapPolygon_p *other )
  : ReosMapPolygonBase_p( other )
{
  mMapPolygon = other->mMapPolygon;
}

ReosMapPolyline_p::ReosMapPolyline_p( QgsMapCanvas *canvas ):
  ReosMapPolygon_p( canvas )
{}

ReosMapPolyline_p *ReosMapPolyline_p::clone()
{
  ReosMapPolyline_p *other = new ReosMapPolyline_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->mMapPolygon = mMapPolygon;
  other->updatePosition();
  return other;
}

void ReosMapPolyline_p::setExtremityDistance( double d )
{
  mExtremityDistance = d;
}

void ReosMapPolyline_p::draw( QPainter *painter )
{
  if ( mExtremityDistance > 0 && mViewPolygon.count() >= 2 )
  {
    QPolygonF polylineToDraw = mViewPolygon;

    QPointF firstPoint = mViewPolygon.first();
    QPointF secondPoint = mViewPolygon.at( 1 );
    QPointF lastPoint = mViewPolygon.last();
    QPointF beforeLastPoint = mViewPolygon.at( mViewPolygon.count() - 2 );

    QPointF firstDir = secondPoint - firstPoint;
    QPointF lastDir = beforeLastPoint - lastPoint;

    double d1 = std::sqrt( firstDir.x() * firstDir.x() + firstDir.y() * firstDir.y() );
    double d2 = std::sqrt( lastDir.x() * lastDir.x() + lastDir.y() * lastDir.y() );

    firstPoint = firstPoint + firstDir / d1 * mExtremityDistance;
    lastPoint = lastPoint + lastDir / d2 * mExtremityDistance;

    polylineToDraw[0] = firstPoint;
    polylineToDraw[polylineToDraw.count() - 1] = lastPoint;

    painter->drawPolyline( polylineToDraw );
  }
  else
    painter->drawPolyline( mViewPolygon );
}

ReosMapMarkerFilledCircle_p::ReosMapMarkerFilledCircle_p( QgsMapCanvas *canvas ): ReosMapMarker_p( canvas )
{}

ReosMapMarkerFilledCircle_p *ReosMapMarkerFilledCircle_p::clone()
{
  ReosMapMarkerFilledCircle_p *other = new ReosMapMarkerFilledCircle_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->mapPoint = mapPoint;
  other->isEmpty = isEmpty;
  other->updatePosition();
  return other;
}

QRectF ReosMapMarker_p::boundingRect() const
{
  if ( isEmpty )
    return QRectF();

  double w = std::max( externalWidth, width ) + 2;
  return QRectF( mViewPoint - QPointF( w / 2, w / 2 ), QSizeF( w, w ) );
}

void ReosMapMarker_p::setMapPosition( const QgsPointXY &pos )
{
  mapPoint = pos.toQPointF();
  updatePosition();
}

ReosMapMarker_p::ReosMapMarker_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{}

void ReosMapMarker_p::updatePosition()
{
  if ( isEmpty )
    return;
  prepareGeometryChange();
  mViewPoint = toCanvasCoordinates( mapPoint );
}

QPainterPath ReosMapMarkerFilledCircle_p::shape() const
{
  if ( isEmpty )
    return QPainterPath();
  QPainterPath path;
  path.addEllipse( boundingRect() );
  return path;
}

void ReosMapMarker_p::translate( const QPointF &translation )
{
  mapPoint += translation;
  updatePosition();
}

QPointF ReosMapMarker_p::mapPos() const {return mapPoint;}

void ReosMapMarkerFilledCircle_p::paint( QPainter *painter )
{
  if ( isEmpty )
    return;

  painter->save();
  QPen pen;
  pen.setStyle( Qt::NoPen );
  if ( externalWidth > width )
  {
    pen.setColor( externalColor );
    QBrush brush( Qt::SolidPattern );
    brush.setColor( externalColor );
    painter->setBrush( brush );
    painter->setPen( pen );
    painter->drawEllipse( mViewPoint, externalWidth / 2, externalWidth / 2 );
  }
  pen.setColor( color );
  QBrush brush( Qt::SolidPattern );
  brush.setColor( color );
  painter->setPen( pen );
  painter->setBrush( brush );
  painter->drawEllipse( mViewPoint, width / 2, width / 2 );
  painter->restore();
}

ReosMapMarkerEmptySquare_p::ReosMapMarkerEmptySquare_p( QgsMapCanvas *canvas ):  ReosMapMarker_p( canvas )
{}

ReosMapMarkerEmptySquare_p *ReosMapMarkerEmptySquare_p::clone()
{
  ReosMapMarkerEmptySquare_p *other = new ReosMapMarkerEmptySquare_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->mapPoint = mapPoint;
  other->isEmpty = isEmpty;
  other->updatePosition();
  return other;
}

QPainterPath ReosMapMarkerEmptySquare_p::shape() const
{
  if ( isEmpty )
    return QPainterPath();
  QPen pen;
  QPainterPath path;
  double squareWidth = ( externalWidth + width ) / 2;
  QRectF square( mViewPoint - QPointF( squareWidth / 2, squareWidth / 2 ), QSize( squareWidth, squareWidth ) );
  pen.setWidth( std::max( 0.0, externalWidth - width ) );
  QPainterPathStroker pps( pen );
  path.addRect( square );
  return pps.createStroke( path );
}

void ReosMapMarkerEmptySquare_p::paint( QPainter *painter )
{
  if ( isEmpty )
    return;

  painter->save();
  QPen pen;

  double squareWidth = ( externalWidth + width ) / 2;
  QRectF square( mViewPoint - QPointF( squareWidth / 2, squareWidth / 2 ), QSize( squareWidth, squareWidth ) );

  pen.setWidth( std::max( 0.0, externalWidth - width ) );
  pen.setColor( isHovered ? externalColor.lighter() : externalColor );
  QBrush brush( Qt::NoBrush );
  painter->setBrush( brush );
  painter->setPen( pen );

  painter->drawRect( square );

  pen.setColor( isHovered ? color.lighter() : color );
  pen.setWidth( std::max( 0.0, externalWidth - width ) / 2 );
  painter->setPen( pen );
  painter->drawRect( square );
  painter->restore();
}

ReosMapMarkerEmptyCircle_p::ReosMapMarkerEmptyCircle_p( QgsMapCanvas *canvas ):  ReosMapMarker_p( canvas )
{}

ReosMapMarkerEmptyCircle_p *ReosMapMarkerEmptyCircle_p::clone()
{
  ReosMapMarkerEmptyCircle_p *other = new ReosMapMarkerEmptyCircle_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->mapPoint = mapPoint;
  other->isEmpty = isEmpty;
  other->updatePosition();
  return other;
}

QPainterPath ReosMapMarkerEmptyCircle_p::shape() const
{
  if ( isEmpty )
    return QPainterPath();
  QPen pen;
  QPainterPath path;
  double squareWidth = ( externalWidth + width ) / 2;
  QRectF square( mViewPoint - QPointF( squareWidth / 2, squareWidth / 2 ), QSize( squareWidth, squareWidth ) );
  pen.setWidth( std::max( 0.0, externalWidth - width ) );
  QPainterPathStroker pps( pen );
  path.addEllipse( square );
  return pps.createStroke( path );
}

void ReosMapMarkerEmptyCircle_p::paint( QPainter *painter )
{
  if ( isEmpty )
    return;

  painter->save();
  QPen pen;

  double circleWidth = ( externalWidth + width ) / 2;
  QRectF square( mViewPoint - QPointF( circleWidth / 2, circleWidth / 2 ), QSize( circleWidth, circleWidth ) );

  pen.setWidth( std::max( 0.0, externalWidth - width ) );
  pen.setColor( isHovered ? externalColor.lighter() : externalColor );
  QBrush brush( Qt::NoBrush );
  painter->setBrush( brush );
  painter->setPen( pen );

  painter->drawEllipse( square );

  pen.setColor( isHovered ? color.lighter() : color );
  pen.setWidth( std::max( 0.0, externalWidth - width ) / 2 );
  painter->setPen( pen );
  painter->drawEllipse( square );
  painter->restore();
}


ReosMapMarkerSvg_p::ReosMapMarkerSvg_p( QgsMapCanvas *canvas, const QString &filePath )
  : ReosMapMarker_p( canvas )
  , mFilePath( filePath )
  , mSvgRenderer( new QSvgRenderer( filePath ) )
{}

ReosMapMarkerSvg_p *ReosMapMarkerSvg_p::clone()
{
  ReosMapMarkerSvg_p *other = new ReosMapMarkerSvg_p( mMapCanvas, mFilePath );
  other->mapPoint = mapPoint;
  other->isEmpty = isEmpty;
  other->updatePosition();
  return other;
}

QRectF ReosMapMarkerSvg_p::boundingRect() const
{
  QRectF bb = mSvgRenderer->viewBox();
  bb.translate( -bb.width() / 2, -bb.height() / 2 );
  bb.translate( mViewPoint );

  return bb;
}

void ReosMapMarkerSvg_p::paint( QPainter *painter )
{
  if ( !mSvgRenderer )
    return;
  painter->save();
  QRectF viewBox = mSvgRenderer->viewBoxF();
  painter->translate( mViewPoint - QPointF( viewBox.width() / 2, viewBox.height() / 2 ) );
  mSvgRenderer->render( painter, viewBox );
  painter->restore();
}

ReosMapPolygonStructured_p::ReosMapPolygonStructured_p( QgsMapCanvas *canvas )
  : ReosMapPolygonBase_p( canvas )
{}

ReosMapPolygonStructured_p *ReosMapPolygonStructured_p::clone()
{
  std::unique_ptr<ReosMapPolygonStructured_p> other( new ReosMapPolygonStructured_p( this ) );
  return other.release();
}

void ReosMapPolygonStructured_p::translate( const QPointF &translation )
{
  if ( mStructure )
    mStructure->translate( translation, mMapCanvas->mapSettings().destinationCrs().toWkt() );

  updatePosition();
}

QPointF ReosMapPolygonStructured_p::mapPos() const
{
  QPolygonF poly = geometry();
  if ( poly.isEmpty() )
    return QPointF();

  return poly.at( 0 );
}

void ReosMapPolygonStructured_p::setGeometry( const QPolygonF &geom )
{
  if ( !mStructure )
    return;

  mStructure->removeAll();
  mStructure->addPolylines( geom, crs() );
  updatePosition();
}

void ReosMapPolygonStructured_p::moveVertex( int index, const QPointF &newPosition )
{
  if ( !mStructure )
    return;

  mStructure->moveVertex( index, ReosSpatialPosition( newPosition, crs() ) );

  updatePosition();
}

void ReosMapPolygonStructured_p::insertVertex( int index, const QPointF &point )
{
  if ( !mStructure )
    return;

  mStructure->insertVertex( index, ReosSpatialPosition( point, crs() ) );

  updatePosition();
}

void ReosMapPolygonStructured_p::removeVertex( int index )
{
  if ( !mStructure )
    return;

  mStructure->removeVertex( index );

  updatePosition();
}

QPolygonF ReosMapPolygonStructured_p::geometry() const
{
  if ( mStructure )
    return mStructure->polyline( crs() );

  return QPolygonF();
}

void ReosMapPolygonStructured_p::setStructrure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  updatePosition();
}

ReosMapPolygonStructured_p::ReosMapPolygonStructured_p( ReosMapPolygonStructured_p *other )
  : ReosMapPolygonBase_p( other )
{
  mStructure = other->mStructure->clone();
}


QString ReosMapItem_p::crs() const
{
  return mMapCanvas->mapSettings().destinationCrs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
}
