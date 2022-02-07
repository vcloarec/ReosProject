/***************************************************************************
  reospolygonstructure_p.cpp - ReosPolygonStructure_p

 ---------------------
 begin                : 5.2.2022
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
#include "reospolygonstructure_p.h"

#include <QDebug>

#include <qgsvectorlayer.h>
#include <qgscategorizedsymbolrenderer.h>
#include <qgslinestring.h>
#include <qgspolygon.h>
#include <qgsfillsymbol.h>
#include <qgsfillsymbollayer.h>

#include "reosstyleregistery.h"

ReosPolygonStructure_p::ReosPolygonStructure_p( const QString &wktCrs ): ReosGeometryStructure_p( QStringLiteral( "Polygon" ), wktCrs )
{
  mVectorLayer->startEditing();
  mVectorLayer->extent();
  QgsField field;
  field.setType( QVariant::String );
  field.setName( QStringLiteral( "classId" ) );
  mVectorLayer->addAttribute( field );

  mRenderer = new QgsCategorizedSymbolRenderer( QStringLiteral( "classId" ) );
  mVectorLayer->setRenderer( mRenderer );

  mVectorLayer->undoStack()->clear();

  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
}

ReosPolygonStructure_p::~ReosPolygonStructure_p()
{}

QObject *ReosPolygonStructure_p::data()
{
  return mVectorLayer.get();
}

void ReosPolygonStructure_p::addPolygon( const QPolygonF &polygon, const QString &classId, const QString &sourceCrs )
{
  const QgsCoordinateTransform transform = toLayerTransform( sourceCrs );

  mVectorLayer->beginEditCommand( tr( "Add resolution polygon" ) );

  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( polygon ) );
  std::unique_ptr<QgsPolygon> qgsPolygon = std::make_unique<QgsPolygon>( linestring.release() );
  QgsGeometry geom( qgsPolygon.release() );

  QgsGeometry layerGeom = geom;

  if ( transform.isValid() )
  {
    try
    {
      layerGeom.transform( transform );
    }
    catch ( QgsCsException &e )
    {
      layerGeom = geom;
    }
  }

  // check for intersecting feature
  QgsFeatureRequest request( layerGeom.boundingBox() );
  QgsFeatureIterator fit = mVectorLayer->getFeatures( request );
  QgsFeature intersectingFeature;

  QSet<QgsFeatureId> sameClassIntersecting;
  QSet<QgsFeatureId> featureToRemove;
  while ( fit.nextFeature( intersectingFeature ) )
  {
    if ( intersectingFeature.geometry().intersects( layerGeom ) )
    {
      if ( intersectingFeature.attribute( 0 ) == classId )
        sameClassIntersecting.insert( intersectingFeature.id() );
      else
      {
        QgsGeometry newGeometry = intersectingFeature.geometry().difference( layerGeom );
        if ( newGeometry.isEmpty() )
          featureToRemove.insert( intersectingFeature.id() );
        mVectorLayer->changeGeometry( intersectingFeature.id(), newGeometry );
      }
    }
  }

  for ( QgsFeatureId fid : featureToRemove )
  {
    mVectorLayer->deleteFeature( fid );
    qDebug() << "remove feature " << fid;
  }

  const QgsFields fields = mVectorLayer->fields();
  QgsFeature feat;
  feat.setGeometry( layerGeom );
  feat.setFields( fields, true );
  feat.setAttribute( 0, classId );

  if ( mVectorLayer->addFeature( feat ) )
  {
    addNewClass( classId );
  }

  mVectorLayer->endEditCommand();
}

int ReosPolygonStructure_p::classIndex( const ReosSpatialPosition &position ) const
{
  QgsRectangle rect( QgsPointXY( position.position() ), QgsPointXY( position.position() ) );
  QgsFeatureIterator it = mVectorLayer->getFeatures( rect );

  QgsFeature feat;
  if ( it.nextFeature( feat ) )
  {
    const QString classId = feat.attribute( 0 ).toString();
    return mClasses.indexOf( classId );
  }

  return -1;
}

QStringList ReosPolygonStructure_p::classes() const
{
  return mClasses;
}

ReosMapExtent ReosPolygonStructure_p::extent( const QString &crs ) const
{
  return ReosGeometryStructure_p::extent( crs );
}

QUndoStack *ReosPolygonStructure_p::undoStack() const
{
  return mVectorLayer->undoStack();
}

void ReosPolygonStructure_p::addNewClass( const QString &classId )
{
  if ( mClasses.contains( classId ) )
    return;

  mClasses.append( classId );

  std::unique_ptr<QgsFillSymbol> fillSymbol = std::make_unique<QgsFillSymbol>();

  QgsSymbolLayer *symbLayer = fillSymbol->symbolLayers().at( 0 );
  if ( symbLayer->layerType() == QStringLiteral( "SimpleFill" ) )
  {
    QgsSimpleFillSymbolLayer *fillLayer = static_cast<QgsSimpleFillSymbolLayer *>( symbLayer );
    fillLayer->setFillColor( ReosStyleRegistery::instance()->fillColor( 100 ) );
    fillLayer->setStrokeStyle( Qt::DashLine );
    fillLayer->setStrokeColor( Qt::lightGray );
  }

  QgsRendererCategory category( classId, fillSymbol.release(), QString() );

  mRenderer->addCategory( category );
}
