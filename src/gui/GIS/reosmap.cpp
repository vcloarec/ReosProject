/***************************************************************************
                      reosmap.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QLayout>
#include <qgsmapcanvas.h>
#include <qgslayertreemapcanvasbridge.h>
#include <qgslayertreemodel.h>

#include "reosmap.h"
#include "reosgisengine.h"


ReosMap::ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget ): ReosModule( gisEngine ),
  mCanvas( new QgsMapCanvas( parentWidget ) )
  /*,
  mapToolNeutral( new HdMapToolNeutral( this ) )*/
{
  mCanvas->setExtent( QgsRectangle( 0, 0, 200, 200 ) );
  mCanvas->setObjectName( "map canvas" );

  QgsLayerTreeModel *layerTreeModel = qobject_cast<QgsLayerTreeModel *>( gisEngine->layerTreeModel() );
  if ( layerTreeModel )
  {
    auto bridge = new QgsLayerTreeMapCanvasBridge( layerTreeModel->rootGroup(), mCanvas, this );
    bridge->setAutoSetupOnFirstLayer( true );

    //Thses two following connections seem weird, but that is for avoiding that the QgsProjectInstance call the lambda while this is destroyed
    //connect( QgsProject::instance(), &QgsProject::readProject, this, &ReosMap::readProject );

    //connect( this, &ReosMap::readProject, [this, bridge]( const QDomDocument & doc )
    connect( QgsProject::instance(), &QgsProject::readProject, [this, bridge]( const QDomDocument & doc )
    {
      bool autoSetupOnFirstLayer = bridge->autoSetupOnFirstLayer();
      bridge->setAutoSetupOnFirstLayer( false );
      bridge->setCanvasLayers();
      if ( autoSetupOnFirstLayer )
        bridge->setAutoSetupOnFirstLayer( true );
      this->mCanvas->readProject( doc );
    } );
  }

  connect( mCanvas, &QgsMapCanvas::xyCoordinates, this, [this]( const QgsPointXY & p )
  {
    emit cursorMoved( p.toQPointF() );
  } );
}

QWidget *ReosMap::mapCanvas() const {return mCanvas;}

ReosMapItem *ReosMap::createMapItem( ReosMapItemFactory *factory )
{
  return factory->create( mCanvas );
}

void ReosMap::refreshCanvas()
{
  mCanvas->refresh();
}

//void ReosMap::setMapTool( ReosMapTool *tool )
//{
//  if ( currentMapTool )
//  {
//    disconnect( currentMapTool, &ReosMapTool::stop, this, &ReosMap::askUnsetMapTool );
//  }
//  currentMapTool = tool;
//  canvas_->setMapTool( tool );
//  connect( currentMapTool, &ReosMapTool::stop, this, &ReosMap::askUnsetMapTool );

//}

//ReosMapTool *ReosMap::getMaptool() const
//{
//  return currentMapTool;
//}

//QgsCoordinateReferenceSystem ReosMap::getCoordinateReferenceSystem()
//{
//  if ( canvas_ )
//    return canvas_->mapSettings().destinationCrs();
//  else
//    return QgsCoordinateReferenceSystem();
//}


//void ReosMap::setMapExtent( QRectF extent )
//{
//  canvas_->setExtent( QgsRectangle( extent ) );
//}

//void ReosMap::setMapSavedExtent( QRectF extent )
//{
//  savedExtent = extent;
//}

//QByteArray ReosMap::encode() const
//{
//  ReosEncodedElement encodedMap( QStringLiteral( "Map" ) );
//  encodedMap.addData( QStringLiteral( "Current extent" ), canvas_->extent().toRectF() );
//  return encodedMap.encode();
//}

//void ReosMap::decode( QByteArray &byteArray )
//{
//  ReosEncodedElement encodedMap( byteArray );
//  QRectF extent;
//  if ( encodedMap.getData( QStringLiteral( "Current extent" ), extent ) )
//  {
//    if ( extent != QRectF() )
//    {
//      savedExtent = QgsRectangle( extent );
//      canvas_->setExtent( extent );
//    }
//  }

//}

//void ReosMap::setToSaveExtent()
//{
//  canvas_->setExtent( savedExtent );
//}

//void ReosMap::saveMapExtent()
//{
//  savedExtent = canvas_->extent();
//}


//void ReosMap::unsetMapTool( ReosMapTool *tool )
//{
//  if ( currentMapTool == tool )
//  {
//    disconnect( currentMapTool, &ReosMapTool::stop, this, &ReosMap::askUnsetMapTool );
//    currentMapTool = nullptr;
//  }
//}

//void ReosMap::unsetMapTool()
//{
//  if ( currentMapTool )
//    disconnect( currentMapTool, &ReosMapTool::stop, this, &ReosMap::askUnsetMapTool );
//  currentMapTool = mapToolNeutral;
//  canvas_->setMapTool( mapToolNeutral );
//}

//void ReosMap::askUnsetMapTool()
//{
//  unsetMapTool();
//}

//void ReosMap::refreshMap() {canvas_->refresh();}

//void ReosMap::crsChanged()
//{
//  canvas_->setDestinationCrs( QgsProject::instance()->crs() );
//}

//QWidget *ReosMap::getWidget() const
//{
//  return canvas_;
//}

ReosMapCursorPosition::ReosMapCursorPosition( ReosMap *map, QWidget *parent ): QLabel( parent )
{
  QRect rect( 0, 0, 150, 15 );
  setGeometry( rect );

  connect( map, &ReosMap::cursorMoved, this, &ReosMapCursorPosition::setPosition );
}

void ReosMapCursorPosition::setPosition( const  QPointF &p )
{
  QString position = tr( "Map Coordinate : " );
  position.append( QString::number( p.x(), 'f', 2 ) );
  position.append( " : " );
  position.append( QString::number( p.y(), 'f', 2 ) );
  position.append( "  " );
  setText( position );
}
