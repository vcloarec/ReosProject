/***************************************************************************
                      reosmaptool_p.cpp
                     --------------------------------------
Date                 : October-2020
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

#include "reosmaptool_p.h"
#include "reosstyleregistery.h"

#include <QGraphicsScene>
#include <QMenu>

#include <qgsmapmouseevent.h>
#include <qgsmaptoolidentify.h>
#include <qgsidentifymenu.h>

#include "reosmappolygon_p.h"
#include "reosgeometryutils.h"


static QList<QgsMapToolIdentify::IdentifyResult> searchFeatureOnMap( QgsMapMouseEvent *e, QgsMapCanvas *canvas, const QList<QgsWkbTypes::GeometryType> &geomType )
{
  QList<QgsMapToolIdentify::IdentifyResult> results;
  const QMap< QString, QString > derivedAttributes;

  QgsPointXY mapPoint = e->mapPoint();
  double x = mapPoint.x(), y = mapPoint.y();
  const double sr = QgsMapTool::searchRadiusMU( canvas );

  const QList<QgsMapLayer *> layers = canvas->layers();
  for ( QgsMapLayer *layer : layers )
  {
    if ( layer->type() == QgsMapLayerType::VectorLayer )
    {
      QgsVectorLayer *vectorLayer = static_cast<QgsVectorLayer *>( layer );

      bool typeIsSelectable = false;
      for ( const QgsWkbTypes::GeometryType &type : geomType )
        if ( vectorLayer->geometryType() == type )
        {
          typeIsSelectable = true;
          break;
        }
      if ( typeIsSelectable )
      {
        QgsRectangle rect( x - sr, y - sr, x + sr, y + sr );
        QgsCoordinateTransform transform = canvas->mapSettings().layerTransform( vectorLayer );

        try
        {
          rect = transform.transformBoundingBox( rect, Qgis::TransformDirection::Reverse );
        }
        catch ( QgsCsException & )
        {
          QgsDebugMsg( QStringLiteral( "Could not transform geometry to layer CRS" ) );
        }

        QgsFeatureIterator fit = vectorLayer->getFeatures( QgsFeatureRequest()
                                 .setFilterRect( rect )
                                 .setFlags( QgsFeatureRequest::ExactIntersect ) );
        QgsFeature f;
        while ( fit.nextFeature( f ) )
        {
          results << QgsMapToolIdentify::IdentifyResult( vectorLayer, f, derivedAttributes );
        }
      }
    }
  }

  return results;
}

bool ReosMapTool_p::hasFeatureOnMap( const QPointF &mapPoint ) const
{
  double x = mapPoint.x(), y = mapPoint.y();
  const double sr = QgsMapTool::searchRadiusMU( mCanvas );

  const QList<QgsMapLayer *> layers = mCanvas->layers();
  for ( QgsMapLayer *layer : layers )
  {
    if ( layer->type() == QgsMapLayerType::VectorLayer )
    {
      QgsVectorLayer *vectorLayer = static_cast<QgsVectorLayer *>( layer );

      QgsRectangle rect( x - sr, y - sr, x + sr, y + sr );
      QgsCoordinateTransform transform = mCanvas->mapSettings().layerTransform( vectorLayer );

      try
      {
        rect = transform.transformBoundingBox( rect, Qgis::TransformDirection::Reverse );
      }
      catch ( QgsCsException & )
      {
        QgsDebugMsg( QStringLiteral( "Could not transform geometry to layer CRS" ) );
      }

      QgsFeatureIterator fit = vectorLayer->getFeatures( QgsFeatureRequest()
                               .setFilterRect( rect )
                               .setFlags( QgsFeatureRequest::ExactIntersect ) );
      QgsFeature feat;
      if ( fit.nextFeature( feat ) )
        return true;
    }
  }

  return false;
}


ReosMapTool_p::ReosMapTool_p( QgsMapCanvas *canvas ):
  QgsMapTool( canvas ), mContextMenuPopulator( new ReosMenuPopulator )
{

}

void ReosMapTool_p::activate()
{
  QgsMapTool::activate();
}

void ReosMapTool_p::deactivate()
{
  QgsMapTool::deactivate();
}


bool ReosMapTool_p::populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent *e )
{
  if ( mContextMenuPopulator )
    return mContextMenuPopulator->populate( menu, e );

  return false;
}

void ReosMapTool_p::setContextMenuPopulator( ReosMenuPopulator *populator )
{
  mContextMenuPopulator.reset( populator );
}

void ReosMapTool_p::setSearchUnderPoint( bool underPoint )
{
  mUnderPoint = underPoint;
}

void ReosMapTool_p::setSearchZoneSize( const QSizeF &size )
{
  mSearchZone = size;
}

void ReosMapTool_p::setSearchTargetDescription( const QString &description )
{
  mTargetDescritpion = description;
}

ReosMapToolDrawPolyline_p::ReosMapToolDrawPolyline_p( QgsMapCanvas *map, bool closed ):
  ReosMapTool_p( map )
{
  mClosed = closed;
  mRubberBand = new QgsRubberBand( map, closed ? QgsWkbTypes::PolygonGeometry : QgsWkbTypes::LineGeometry );
}

ReosMapToolDrawPolyline_p::~ReosMapToolDrawPolyline_p()
{
  if ( mRubberBand )
    delete mRubberBand.data();
}

void ReosMapToolDrawPolyline_p::deactivate()
{
  if ( mRubberBand )
    mRubberBand->reset( mClosed ? QgsWkbTypes::PolygonGeometry : QgsWkbTypes::LineGeometry );
  ReosMapTool_p::deactivate();
}



void ReosMapToolDrawPolyline_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  ReosMapTool_p::canvasMoveEvent( e );

  mRubberBand->movePoint( e->mapPoint() );

  if ( selfIntersect() )
  {
    mRubberBand->setColor( ReosStyleRegistery::instance()->invalidColor() );
    mRubberBand->setFillColor( ReosStyleRegistery::instance()->invalidColor( 100 ) );
  }
  else
  {
    mRubberBand->setColor( mColor );
    mRubberBand->setFillColor( mFillColor );
  }
}

void ReosMapToolDrawPolyline_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( e->button() == Qt::LeftButton )
  {
    if ( snappingEnabled() )
      e->snapPoint();
    mRubberBand->addPoint( e->mapPoint() );
  }

  if ( e->button() == Qt::RightButton )
  {
    if ( mRubberBand->numberOfVertices() < ( mClosed ? 2 : 1 ) )
    {
      const QgsGeometry geom = selectFeatureOnMap( e );
      if ( !geom.isNull() )
      {
        QPolygonF returnPoly = geom.asQPolygonF();
        if ( mClosed && !returnPoly.empty() )
          returnPoly.removeLast();

        emit polylineDrawn( returnPoly );
      }
    }
    else
    {

      QPolygonF polyline = mRubberBand->asGeometry().asQPolygonF();
      if ( !selfIntersect() )
      {
        if ( !polyline.isEmpty() )
        {
          polyline.removeLast();
          if ( mClosed && !polyline.isEmpty() )
            polyline.removeLast();
        }

        emit polylineDrawn( polyline );
        mRubberBand->reset( mClosed ? QgsWkbTypes::PolygonGeometry : QgsWkbTypes::LineGeometry );
        updateColor();
      }
    }


  }
}

void ReosMapToolDrawPolyline_p::setColor( const QColor &color )
{
  mRubberBand->setColor( color );
  mColor = color;
}

void ReosMapToolDrawPolyline_p::setFillColor( const QColor &color )
{
  mRubberBand->setFillColor( color );
  mFillColor = color;
}

void ReosMapToolDrawPolyline_p::setAllowSelfIntersect( bool allowSelfIntersect )
{
  mAllowSelfIntersect = allowSelfIntersect;
}

bool ReosMapToolDrawPolyline_p::selfIntersect() const
{
  QgsGeometry geom = mRubberBand->asGeometry();
  geom.removeDuplicateNodes();
  return !geom.isNull() && geom.constGet()->vertexCount() > 3 && !QgsGeometryUtils::selfIntersections( geom.constGet(), 0, 0, 0 ).isEmpty();
}

void ReosMapToolDrawPolyline_p::updateColor()
{
  if ( selfIntersect() )
  {
    mRubberBand->setColor( ReosStyleRegistery::instance()->invalidColor() );
    mRubberBand->setFillColor( ReosStyleRegistery::instance()->invalidColor( 100 ) );
  }
  else
  {
    mRubberBand->setColor( mColor );
    mRubberBand->setFillColor( mFillColor );
  }
}




ReosMapToolDrawExtent_p::ReosMapToolDrawExtent_p( QgsMapCanvas *map ): ReosMapTool_p( map )
{
  mRubberBand = new QgsRubberBand( map, QgsWkbTypes::PolygonGeometry );
}

ReosMapToolDrawExtent_p::~ReosMapToolDrawExtent_p()
{
  if ( mRubberBand )
    delete mRubberBand.data();
}

void ReosMapToolDrawExtent_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( !mIsDrawing )
    return;

  mEndPoint = e->mapPoint();
  drawExtent();
}

void ReosMapToolDrawExtent_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  mIsDrawing = true;
  mStartPoint = e->mapPoint();
}

void ReosMapToolDrawExtent_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  mIsDrawing = false;
  mEndPoint = e->mapPoint();
  mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  QRectF extent( mStartPoint.toQPointF(), mEndPoint.toQPointF() );
  emit extentDrawn( extent );
}

void ReosMapToolDrawExtent_p::deactivate()
{
  if ( mRubberBand )
    mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  mIsDrawing = false;
  ReosMapTool_p::deactivate();
}

void ReosMapToolDrawExtent_p::drawExtent()
{
  QgsRectangle rect( mStartPoint, mEndPoint );

  mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  mRubberBand->addPoint( QgsPointXY( rect.xMinimum(), rect.yMinimum() ), false );
  mRubberBand->addPoint( QgsPointXY( rect.xMaximum(), rect.yMinimum() ), false );
  mRubberBand->addPoint( QgsPointXY( rect.xMaximum(), rect.yMaximum() ), false );
  mRubberBand->addPoint( QgsPointXY( rect.xMinimum(), rect.yMaximum() ), true );
}


ReosMapToolSelectMapItem_p::ReosMapToolSelectMapItem_p( QgsMapCanvas *map, const QString &targetDescription ):
  ReosMapTool_p( map )
{
  setSearchTargetDescription( targetDescription );
  setSearchZoneSize( QSizeF( 5, 5 ) );
}

void ReosMapToolSelectMapItem_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  QPointF p = e->localPos();

  ReosMapItem_p *mapItem = searchItem( p );

  if ( mapItem )
    emit found( mapItem->base, e->mapPoint().toQPointF() );
  else
    emit found( nullptr, e->mapPoint().toQPointF() );

}

bool ReosMapToolSelectMapItem_p::populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event )
{
  canvasReleaseEvent( event );
  return ReosMapTool_p::populateContextMenuWithEvent( menu, event );
}

ReosMapToolDrawPoint_p::ReosMapToolDrawPoint_p( QgsMapCanvas *map )
  : ReosMapTool_p( map )
{
}

ReosMapToolDrawPoint_p::~ReosMapToolDrawPoint_p() {}

void ReosMapToolDrawPoint_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( e->button() == Qt::LeftButton )
    emit pointDrawn( e->mapPoint().toQPointF() );
}

ReosMapToolEditPolygon_p::ReosMapToolEditPolygon_p( QgsMapCanvas *map ):
  ReosMapTool_p( map )
{
}

void ReosMapToolEditPolygon_p::setMapPolygon( ReosMapPolygon_p *polygon )
{
  if ( mPolygon )
    mPolygon->setEditing( false );

  mPolygon = polygon;

  if ( mPolygon )
    mPolygon->setEditing( isActive() );
}

void ReosMapToolEditPolygon_p::activate()
{
  if ( mPolygon )
    mPolygon->setEditing( true );

  ReosMapTool_p::activate();
}

void ReosMapToolEditPolygon_p::deactivate()
{
  if ( mPolygon )
    mPolygon->setEditing( false );

  ReosMapTool_p::deactivate();
}

bool ReosMapToolEditPolygon_p::populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event )
{
  if ( !menu )
    return false;

  // remove QQIS default menu
  menu->clear();

  const QPointF mapPoint = event->mapPoint().toQPointF();
  menu->addAction( tr( "Insert vertex" ), this, [mapPoint, this]
  {
    int index = ReosGeometryUtils::closestSegment( mapPoint, mPolygon->geometry() );
    if ( index != -1 )
    {
      mPolygon->insertVertex( index, mapPoint );
      emit this->polygonEdited();
    }

  } );

  int existingVertex = mPolygon->findVertexInView( viewSearchZone( event->pos() ) );
  if ( existingVertex >= 0 )
  {
    menu->addAction( tr( "Remove vertex" ), this, [existingVertex, this]
    {
      mPolygon->removeVertex( existingVertex );
      mPolygon->updatePosition();
      emit this->polygonEdited();
    } );
  }

  return true;
}

void ReosMapToolEditPolygon_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  if ( !mPolygon )
    return;

  mMovingVertex = mPolygon->findVertexInView( viewSearchZone( e->pos() ) );
}

void ReosMapToolEditPolygon_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( mMovingVertex < 0 || !mPolygon )
    return;
  mIsEdited = true;
  mPolygon->moveVertex( mMovingVertex, e->mapPoint().toQPointF() );

}

void ReosMapToolEditPolygon_p::canvasReleaseEvent( QgsMapMouseEvent * )
{
  if ( mIsEdited )
    emit polygonEdited();
  mIsEdited = false;
  mMovingVertex = -1;
}


QRectF ReosMapTool_p::viewSearchZone( const QPoint &pt )
{
  QPoint zone( mSearchZone.width() / 2, mSearchZone.height() / 2 );
  return QRectF( QPointF( pt - zone ),  QPointF( pt + zone ) );
}

ReosMapItem_p *ReosMapTool_p::searchItem( const QPointF &p ) const
{
  QList<QGraphicsItem *> listItems;
  if ( mUnderPoint )
    listItems  = canvas()->scene()->items( p );
  else
  {
    QRectF rectf( p - QPointF( mSearchZone.width() / 2, mSearchZone.height() / 2 ), mSearchZone );
    listItems  = canvas()->scene()->items( rectf );
  }

  QGraphicsItem *item = nullptr;
  ReosMapItem_p *mapItem = nullptr;
  int i = 0;
  while ( !( mapItem ) && i < listItems.count() )
  {
    item = listItems.at( i );
    mapItem = dynamic_cast<ReosMapItem_p *>( item );
    if ( mapItem && !mTargetDescritpion.isEmpty() && !mapItem->base->description().contains( mTargetDescritpion ) )
      mapItem = nullptr;
    ++i;
  }

  return mapItem;
}

QgsGeometry ReosMapTool_p::selectFeatureOnMap( QgsMapMouseEvent *e )
{
  const QList<QgsMapToolIdentify::IdentifyResult> &results =
    searchFeatureOnMap( e, mCanvas, QList<QgsWkbTypes::GeometryType>() << QgsWkbTypes::PolygonGeometry << QgsWkbTypes::LineGeometry );

  QgsIdentifyMenu *menu = new QgsIdentifyMenu( mCanvas );
  menu->setExecWithSingleResult( true );
  menu->setAllowMultipleReturn( false );
  const QPoint globalPos = mCanvas->mapToGlobal( QPoint( e->pos().x() + 5, e->pos().y() + 5 ) );
  const QList<QgsMapToolIdentify::IdentifyResult> selectedFeatures = menu->exec( results, globalPos );
  menu->deleteLater();

  if ( !selectedFeatures.empty() && selectedFeatures[0].mFeature.hasGeometry() )
  {
    QgsCoordinateTransform transform = mCanvas->mapSettings().layerTransform( selectedFeatures.at( 0 ).mLayer );
    QgsGeometry geom = selectedFeatures[0].mFeature.geometry();
    try
    {
      geom.transform( transform );
    }
    catch ( QgsCsException & )
    {}

    return geom;;
  }

  return QgsGeometry();
}

bool ReosMapTool_p::snappingEnabled() const
{
  return mSnappingEnabled;
}

void ReosMapTool_p::setSeachWhenMoving( bool seachWhenMoving )
{
  mSeachWhenMoving = seachWhenMoving;
}

void ReosMapTool_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( mSnappingEnabled )
  {
    e->snapPoint();
    mSnappingIndicator->setMatch( e->mapPointMatch() );
  }

  if ( !mSeachWhenMoving )
    return;

  ReosMapItem_p *foundItem = searchItem( e->localPos() );

  if ( mFoundItem && mFoundItem != foundItem )
  {
    mFoundItem->isHovered = false;
    mFoundItem->update();
    mFoundItem = nullptr;
  }

  if ( foundItem )
  {
    mFoundItem = foundItem;
    mFoundItem->isHovered = true;
    mFoundItem->update();
  }

  if ( foundItem )
    emit foundItemWhenMoving( foundItem );
}


void ReosMapTool_p::clearHoveredItem()
{
  if ( mFoundItem )
  {
    mFoundItem->isHovered = false;
    mFoundItem->update();
    mFoundItem = nullptr;
  }
}

void ReosMapTool_p::enableSnapping( bool enable )
{
  mSnappingEnabled = enable;
  if ( mSnappingEnabled )
    mSnappingIndicator.reset( new QgsSnapIndicator( canvas() ) );
  else
    mSnappingIndicator.reset();
}


void ReosMapTool_p::keyPressEvent( QKeyEvent *e )
{
  emit keyPressed( e->key() );
}

QString ReosMapTool_p::mapCrs() const
{
  return canvas()->mapSettings().destinationCrs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}


ReosMapToolMoveItem_p::ReosMapToolMoveItem_p( QgsMapCanvas *map ): ReosMapTool_p( map )
{}

ReosMapToolMoveItem_p::~ReosMapToolMoveItem_p()
{}

void ReosMapToolMoveItem_p::setCurrentItem( ReosMapItem_p *item )
{
  if ( item )
    item->setEditing( false );

  mCurrentItem = item;

  if ( item )
    item->setEditing( isActive() );
}

void ReosMapToolMoveItem_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  if ( !mCurrentItem || !isItemUnderPoint( e->pos() ) )
    return;

  mMovingItem.reset( mCurrentItem->clone() );
  mMovingItem->color = mMovingColor;
  mMovingItem->externalColor = mMovingColor;
  mStartPoint = e->mapPoint().toQPointF();
  mIsMoving = true;

}

void ReosMapToolMoveItem_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( !mCurrentItem || !mIsMoving || !mMovingItem )
    return;
  QPointF translation = e->mapPoint().toQPointF() - mStartPoint;
  QPointF oldItemTranslation = mMovingItem->mapPos() - mCurrentItem->mapPos();
  QPointF diff = translation - oldItemTranslation;

  mMovingItem->translate( diff );
}

void ReosMapToolMoveItem_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( !mMovingItem )
    return;

  mMovingItem.reset();

  if ( mCurrentItem )
  {
    mCurrentItem->translate( e->mapPoint().toQPointF() - mStartPoint );
    emit itemMoved( mCurrentItem->base );
  }

  mIsMoving = false;
}

bool ReosMapToolMoveItem_p::isItemUnderPoint( const QPoint &p )
{
  QList<QGraphicsItem *> listItems;

  listItems  = canvas()->scene()->items( viewSearchZone( p ) );

  for ( QGraphicsItem *item : std::as_const( listItems ) )
    if ( item == mCurrentItem )
      return true;

  return false;

}

void ReosMapToolMoveItem_p::setMovingColor( const QColor &movingColor )
{
  mMovingColor = movingColor;
}

ReosMapToolDrawHydraulicNetworkLink_p::ReosMapToolDrawHydraulicNetworkLink_p( QgsMapCanvas *mapCanvas ): ReosMapTool_p( mapCanvas )
{
  mRubberBand = new QgsRubberBand( mCanvas );
}

void ReosMapToolDrawHydraulicNetworkLink_p::appendItem( ReosMapItem_p *item )
{
  mLinkedItems.append( item );
  if ( mRubberBand->numberOfVertices() == 0 )
    mRubberBand->addPoint( item->mapPos() );
  mRubberBand->addPoint( item->mapPos() );
}

void ReosMapToolDrawHydraulicNetworkLink_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  ReosMapTool_p::canvasMoveEvent( e );

  if ( mFoundItem )
    mRubberBand->movePoint( mFoundItem->mapPos() );
  else
    mRubberBand->movePoint( e->mapPoint() );
}

void ReosMapToolDrawHydraulicNetworkLink_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  switch ( e->button() )
  {
    case Qt::LeftButton:
    {
      ReosMapItem_p *item = searchItem( e->localPos() );
      if ( item )
        emit itemSelected( item );
    }
    break;
    case Qt::RightButton:
      mRubberBand->reset();
      mLinkedItems.clear();
      break;
    default:
      break;
  }
}
