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

ReosMapPolygon_p::ReosMapPolygon_p( QgsMapCanvas *canvas ):
  ReosMapItem_p( canvas )
{

}

ReosMapPolygon_p *ReosMapPolygon_p::clone()
{
  ReosMapPolygon_p *other = new ReosMapPolygon_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->brushStyle = brushStyle;
  other->fillColor = fillColor;
  other->mapPolygon = mapPolygon;
  other->updatePosition();
  return other;
}

QRectF ReosMapPolygon_p::boundingRect() const
{
  QRectF bb = mViewPolygon.boundingRect();
  if ( externalWidth > width )
    return bb.adjusted( -externalWidth, -externalWidth, externalWidth, externalWidth );
  else
    return bb.adjusted( -width, -width, width, width );
}

void ReosMapPolygon_p::updatePosition()
{
  prepareGeometryChange();
  mViewPolygon.clear();
  if ( mapPolygon.count() < 1 )
    return;
  const QPointF pview0 = toCanvasCoordinates( QgsPoint( mapPolygon.at( 0 ) ) );

  for ( auto &p : mapPolygon )
  {
    const QPointF pview = toCanvasCoordinates( QgsPoint( p ) );
    mViewPolygon.append( QPointF( pview.x() - pview0.x(), pview.y() - pview0.y() ) );
  }
  setPos( pview0 );

  if ( mSegmentMarker >= 0 )
    mMarkerPositionOnView = toCanvasCoordinates( QgsPoint( mMarkerposition ) );
}

QPainterPath ReosMapPolygon_p::shape() const
{
  QPainterPath path;
  path.addPolygon( mViewPolygon );
  path.closeSubpath();
  return path ;
}

void ReosMapPolygon_p::setEditing( bool b )
{
  mIsEditing = b;
  update();
}

void ReosMapPolygon_p::translate( const QPointF &translation )
{
  QPolygonF newPoints( mapPolygon.size() );

  for ( int i = 0; i < mapPolygon.size(); ++i )
    newPoints[i] = mapPolygon.at( i ) + translation;

  mapPolygon = newPoints;
  updatePosition();
}

QPointF ReosMapPolygon_p::mapPos() const
{
  if ( mapPolygon.isEmpty() )
    return QPointF();
  else
    return mapPolygon.at( 0 );
}

int ReosMapPolygon_p::findVertexInView( const QRectF &zone ) const
{
  for ( int i = 0; i < mViewPolygon.size(); ++i )
    if ( zone.contains( mViewPolygon.at( i ) + pos() ) )
      return i;

  return -1;
}

void ReosMapPolygon_p::activeMarker( bool b )
{
  mIsMarkerActive = b;
}

void ReosMapPolygon_p::setMarkerDistance( double d )
{
  if ( !mIsMarkerActive || d < 0 || mapPolygon.empty() )
  {
    mSegmentMarker = -1;
    updatePosition();
    return;
  }

  double distFromBegin = 0;
  int i = 0;
  do
  {
    const QPointF &p1 = mapPolygon.at( i );
    const QPointF &p2 = mapPolygon.at( i + 1 );
    distFromBegin += sqrt( pow( p1.x() - p2.x(), 2 ) + pow( p1.y() - p2.y(), 2 ) );
  }
  while ( d >= distFromBegin && ++i < mapPolygon.count() - 1 );

  if ( d > distFromBegin )
  {
    mSegmentMarker = -1;
    updatePosition();
    return;
  }

  if ( i >= mapPolygon.count() - 1 )
  {
    mSegmentMarker = -1;
    updatePosition();
    return;
  }
  mSegmentMarker = i;
  const QPointF &p1 = mapPolygon.at( i );
  const QPointF &p2 = mapPolygon.at( i + 1 );
  double segDist = sqrt( pow( p1.x() - p2.x(), 2 ) + pow( p1.y() - p2.y(), 2 ) );
  double distFromP2 = distFromBegin - d;
  double ratio = distFromP2 / segDist;
  mMarkerposition = p2 - ratio * ( p2 - p1 );
  updatePosition();
}

void ReosMapPolygon_p::setMarkerArrow( bool b )
{
  mMarkerArrow = b;
}

void ReosMapPolygon_p::paint( QPainter *painter )
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

void ReosMapPolygon_p::draw( QPainter *painter )
{
  painter->drawPolygon( mViewPolygon );
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
  other->mapPolygon = mapPolygon;
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
{

}

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

