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

  mVectorLayer->undoStack()->blockSignals( true );

  QgsField field;
  field.setType( QVariant::String );
  field.setName( QStringLiteral( "classId" ) );
  mVectorLayer->addAttribute( field );
  mRenderer = new QgsCategorizedSymbolRenderer( QStringLiteral( "classId" ) );
  mVectorLayer->commitChanges( false );
  mVectorLayer->setRenderer( mRenderer );
  mVectorLayer->undoStack()->clear();

  mVectorLayer->undoStack()->blockSignals( false );

  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
}

ReosPolygonStructure_p::~ReosPolygonStructure_p()
{}

ReosPolygonStructure *ReosPolygonStructure_p::clone() const
{
  std::unique_ptr<ReosPolygonStructure_p> other = std::make_unique<ReosPolygonStructure_p>();
  other->mVectorLayer.reset( mVectorLayer->clone() );
  other->mRenderer = static_cast<QgsCategorizedSymbolRenderer *>( other->mVectorLayer->renderer() );
  other->mClasses = mClasses;
  other->mTolerance = mTolerance;

  // in editing mode, if the changes are not commited, the feature will bot be clones
  // If we commit, that will clear the undostack and we don't want, so preferable to copy all features
  QgsFeatureIterator it = mVectorLayer->getFeatures();
  other->mVectorLayer->startEditing();
  QgsFeature feat;
  while ( it.nextFeature( feat ) )
    other->mVectorLayer->addFeature( feat );
  other->mVectorLayer->commitChanges( false );

  return other.release();
}

QObject *ReosPolygonStructure_p::data()
{
  return mVectorLayer.get();
}

double ReosPolygonStructure_p::value( const ReosSpatialPosition &position, bool acceptClose ) const
{
  QgsPointXY pt = toLayerCoordinates( position );

  QgsRectangle rect( pt - QgsVector( mTolerance, mTolerance ), pt + QgsVector( mTolerance, mTolerance ) );
  QgsFeatureIterator it = mVectorLayer->getFeatures( QgsFeatureRequest()
                          .setFilterRect( rect )
                          .setFlags( QgsFeatureRequest::ExactIntersect ) );

  QgsFeature feat;
  double value = 0;
  int foundValues = 0;
  while ( it.nextFeature( feat ) )
  {
    const QgsGeometry &geom = feat.geometry();
    const QString classId = feat.attribute( 0 ).toString();
    if ( acceptClose || ( geom.contains( &pt ) ) )
    {
      bool ok = false;
      double v = mClasses.value( classId ).toDouble( &ok );
      if ( ok )
      {
        foundValues++;
        value += v;
        if ( !acceptClose )
          break;
      }
    }
  }

  if ( foundValues > 0 )
    return value / foundValues;
  else
    return std::numeric_limits<double>::quiet_NaN();

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

  QSet<QgsFeatureId> featureToRemove;
  while ( fit.nextFeature( intersectingFeature ) )
  {
    if ( intersectingFeature.geometry().intersects( layerGeom ) )
    {
      if ( intersectingFeature.attribute( 0 ) == classId )
      {
        layerGeom = layerGeom.combine( intersectingFeature.geometry() );
        featureToRemove.insert( intersectingFeature.id() );
      }
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
  }

  if ( !classId.isEmpty() )
  {
    const QgsFields fields = mVectorLayer->fields();
    QgsFeature feat;
    feat.setGeometry( layerGeom );
    feat.setFields( fields, true );
    feat.setAttribute( 0, classId );
    mVectorLayer->addFeature( feat );
  }

  mVectorLayer->endEditCommand();
}

QStringList ReosPolygonStructure_p::classes() const
{
  return mClasses.keys();
}

ReosMapExtent ReosPolygonStructure_p::extent( const QString &crs ) const
{
  return ReosGeometryStructure_p::extent( crs );
}

QColor ReosPolygonStructure_p::color( const QString &classId ) const
{
  int ind = mRenderer->categoryIndexForValue( classId );
  const QgsCategoryList &list = mRenderer->categories();
  if ( ind < 0 || ind >= list.count() )
    return QColor();

  return symbolColor( list.at( ind ).symbol() );
}

double ReosPolygonStructure_p::value( const QString &classId ) const
{
  QVariant var = mClasses.value( classId );
  if ( var.isValid() )
  {
    bool ok = false;
    double v = var.toDouble( &ok );
    if ( ok )
      return v;
  }

  return std::numeric_limits<double>::quiet_NaN();
}

int ReosPolygonStructure_p::polygonsCount() const
{
  long long count = mVectorLayer->featureCount();

  if ( count > __INT32_MAX__ )
    return __INT32_MAX__;

  return int( count );
}

QUndoStack *ReosPolygonStructure_p::undoStack() const
{
  return mVectorLayer->undoStack();
}

QColor ReosPolygonStructure_p::symbolColor( QgsSymbol *sym ) const
{
  const QgsSymbolLayer *lay = sym->symbolLayer( 0 );
  if ( lay->layerType() != QStringLiteral( "SimpleFill" ) )
    return sym->color();

  return static_cast<const QgsFillSymbolLayer *>( lay )->fillColor();
}

void ReosPolygonStructure_p::addClass( const QString &classId, double value )
{
  if ( mClasses.contains( classId ) )
    return;

  mClasses.insert( classId, value );

  std::unique_ptr<QgsFillSymbol> fillSymbol = std::make_unique<QgsFillSymbol>();

  QgsSymbolLayer *symbLayer = fillSymbol->symbolLayers().at( 0 );

  if ( symbLayer->layerType() == QStringLiteral( "SimpleFill" ) )
  {
    QgsSimpleFillSymbolLayer *fillLayer = static_cast<QgsSimpleFillSymbolLayer *>( symbLayer );
    fillLayer->setFillColor( ReosStyleRegistery::instance()->fillColor() );
    fillLayer->setStrokeStyle( Qt::DashLine );
    fillLayer->setStrokeColor( Qt::lightGray );
  }

  QgsRendererCategory category( classId, fillSymbol.release(), QString() );

  mRenderer->addCategory( category );

  emit classesChanged();
}
