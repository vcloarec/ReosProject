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

#include <qgsvectorlayer.h>
#include <qgscategorizedsymbolrenderer.h>
#include <qgslinestring.h>
#include <qgspolygon.h>
#include <qgsfillsymbol.h>
#include <qgsfillsymbollayer.h>
#include <qgsgeometryengine.h>
#include <qgsspatialindex.h>

#include "reosstyleregistery.h"

ReosPolygonStructure_p::ReosPolygonStructure_p( const QString &wktCrs ): ReosGeometryStructure_p( QStringLiteral( "Polygon" ), wktCrs )
{
  init();
  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
}

ReosPolygonStructure_p::ReosPolygonStructure_p( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "polygon-structure" ) )
    return;
  QString wktCrs;
  element.getData( QStringLiteral( "crs" ), wktCrs );
  mVectorLayer.reset( new QgsVectorLayer( QStringLiteral( "Polygon?crs=" )
                                          + wktCrs
                                          + QStringLiteral( "&index=yes" )
                                          , QStringLiteral( "internalLayer" ),
                                          QStringLiteral( "memory" ) ) );

  init();

  element.getData( QStringLiteral( "tolerance" ), mTolerance );
  element.getData( QStringLiteral( "classes" ), mClasses );
  element.getData( QStringLiteral( "last-color-index" ), mLastColorIndex );

  QVariantMap colors;
  element.getData( QStringLiteral( "colors-class" ), colors );
  for ( const QString &key : colors.keys() )
    addClassColor( key, colors.value( key ).value<QColor>() );

  QStringList featClass;
  QVariantList polygons;

  element.getData( QStringLiteral( "polygons-class" ), featClass );
  element.getData( QStringLiteral( "polygons-geometry" ), polygons );

  mVectorLayer->undoStack()->blockSignals( true );
  QVariantList varFeatures;
  element.getData( QStringLiteral( "polygons" ), varFeatures );
  for ( const QVariant &featVar : std::as_const( varFeatures ) )
  {
    QgsFeature feat = featVar.value<QgsFeature>();
    mVectorLayer->addFeature( feat );
  }
  mVectorLayer->undoStack()->clear();
  mVectorLayer->undoStack()->blockSignals( false );
}


void ReosPolygonStructure_p::init()
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

ReosEncodedElement ReosPolygonStructure_p::encode() const
{
  ReosEncodedElement element( QStringLiteral( "polygon-structure" ) );

  element.addData( QStringLiteral( "crs" ), crs() );
  element.addData( QStringLiteral( "tolerance" ), mTolerance );
  element.addData( QStringLiteral( "classes" ), mClasses );
  element.addData( QStringLiteral( "last-color-index" ), mLastColorIndex );

  QVariantMap colors;
  for ( const QString &key : mClasses.keys() )
    colors.insert( key, color( key ) );

  element.addData( QStringLiteral( "colors-class" ), colors );

  QgsFeatureIterator it = mVectorLayer->getFeatures();

  QStringList featClass;
  QVariantList varFeatures;

  QgsFeature feat;
  while ( it.nextFeature( feat ) )
  {
    QVariant var( feat );
    varFeatures.append( var );
  }

  element.addData( QStringLiteral( "polygons" ), varFeatures );

  mDirty = false;
  return element;
}

QString ReosPolygonStructure_p::crs() const
{
  return mVectorLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
}

ReosPolygonStructure_p::~ReosPolygonStructure_p()
{
  disconnect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
}

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
    catch ( QgsCsException & )
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
  mDirty = true;
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

  if ( count > INT32_MAX )
    return INT32_MAX;

  return int( count );
}

ReosPolygonStructureValues *ReosPolygonStructure_p::values( const QString &destinationCrs ) const
{
  std::unique_ptr<ReosPolygonStructureValues_p> ret( new ReosPolygonStructureValues_p );

  ret->mCacheGeom = nullptr;
  ret->mCacheValue = std::numeric_limits<double>::quiet_NaN();
  ret->mTransform = toDestinationTransform( destinationCrs );
  ret->mTolerance = mTolerance;
  ret->mSpatialIndex.reset( new QgsSpatialIndex( *mVectorLayer.get() ) );

  QgsFeatureIterator it = mVectorLayer->getFeatures();

  QgsFeature feat;
  QgsGeometry zoneWithoutPolygon = QgsGeometry::fromRect( mVectorLayer->extent() );

  while ( it.nextFeature( feat ) )
  {
    const QgsGeometry geom = feat.geometry();
    ret->mGeomEngines.emplace( feat.id(), QgsGeometry::createGeometryEngine( geom.constGet() ) );
    ret->mGeomEngines.at( feat.id() )->prepareGeometry();
    const QString classId = feat.attribute( 0 ).toString();
    ret->mValues.insert( feat.id(), mClasses.value( classId ).toDouble() );
    zoneWithoutPolygon = zoneWithoutPolygon.difference( geom );
  }

  ret->mZoneWithoutPolygon.reset( QgsGeometry::createGeometryEngine( zoneWithoutPolygon.constGet() ) );
  ret->mZoneWithoutPolygon->prepareGeometry();

  return ret.release();
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

void ReosPolygonStructure_p::addClassColor( const QString &classId, const QColor &color )
{
  std::unique_ptr<QgsFillSymbol> fillSymbol = std::make_unique<QgsFillSymbol>();
  QgsSymbolLayer *symbLayer = fillSymbol->symbolLayers().at( 0 );
  if ( symbLayer->layerType() == QStringLiteral( "SimpleFill" ) )
  {
    QgsSimpleFillSymbolLayer *fillLayer = static_cast<QgsSimpleFillSymbolLayer *>( symbLayer );
    fillLayer->setFillColor( color );
    fillLayer->setStrokeStyle( Qt::DashLine );
    fillLayer->setStrokeColor( Qt::lightGray );
  }

  QgsRendererCategory category( classId, fillSymbol.release(), QString() );
  mRenderer->addCategory( category );
}

void ReosPolygonStructure_p::removeClassColor( const QString &classId )
{
  int ind = mRenderer->categoryIndexForValue( classId );
  mRenderer->deleteCategory( ind );
}

void ReosPolygonStructure_p::addClass( const QString &classId, double value )
{
  if ( mClasses.contains( classId ) )
    return;

  mVectorLayer->beginEditCommand( tr( "Add class" ) );

  const QColor color = ReosStyleRegistery::instance()->fillColor( mLastColorIndex, 100 );
  mVectorLayer->undoStack()->push( new ReosPolygonStructureUndoCommandAddClass( this, classId, value, color ) );

  mVectorLayer->endEditCommand();
  mDirty = true;
}

void ReosPolygonStructure_p::removeClass( const QString &classId )
{
  if ( !mClasses.contains( classId ) )
    return;

  mVectorLayer->beginEditCommand( tr( "Remove class" ) );

  QgsFeatureIterator it = mVectorLayer->getFeatures( QStringLiteral( "classId='%1'" ).arg( classId ) );
  QgsFeatureIds fids;

  QgsFeature feat;
  while ( it.nextFeature( feat ) )
    fids.insert( feat.id() );

  mVectorLayer->deleteFeatures( fids );
  mVectorLayer->undoStack()->push( new ReosPolygonStructureUndoCommandRemoveClass( this, classId ) );

  mVectorLayer->endEditCommand();
  mDirty = true;
}

QString ReosPolygonStructure_p::valueToClass( double value ) const
{
  for ( auto it = mClasses.begin(); it != mClasses.end(); ++it )
  {
    if ( it.value().toDouble() == value )
      return it.key();
  }

  return QString();
}

ReosPolygonStructureUndoCommandAddClass::ReosPolygonStructureUndoCommandAddClass( ReosPolygonStructure_p *structure, const QString &classId, double value, const QColor &color )
  : mStructure( structure )
  , mClassId( classId )
  , mValue( value )
  , mColor( color )
{}

void ReosPolygonStructureUndoCommandAddClass::redo()
{
  mStructure->mClasses.insert( mClassId, mValue );
  mStructure->addClassColor( mClassId, mColor );
  emit mStructure->classesChanged();
}

void ReosPolygonStructureUndoCommandAddClass::undo()
{
  mStructure->mClasses.remove( mClassId );
  mStructure->removeClassColor( mClassId );
  emit mStructure->classesChanged();
}

ReosPolygonStructureUndoCommandRemoveClass::ReosPolygonStructureUndoCommandRemoveClass( ReosPolygonStructure_p *structure, const QString &classId )
  : mStructure( structure )
  , mClassId( classId )
{}

void ReosPolygonStructureUndoCommandRemoveClass::redo()
{
  mColor = mStructure->color( mClassId );
  mValue = mStructure->mClasses.value( mClassId ).toDouble();

  mStructure->mClasses.remove( mClassId );
  mStructure->removeClassColor( mClassId );
  emit mStructure->classesChanged();
}

void ReosPolygonStructureUndoCommandRemoveClass::undo()
{
  mStructure->mClasses.insert( mClassId, mValue );
  mStructure->addClassColor( mClassId, mColor );

  emit mStructure->classesChanged();
}

double ReosPolygonStructureValues_p::value( double x, double y, bool acceptClose ) const
{
  QgsPointXY pt;
  try
  {
    pt = mTransform.transform( QgsPointXY( x, y ) );
  }
  catch ( ... )
  {
    pt = QgsPointXY( x, y );
  }

  QgsPoint point( pt );

  if ( !acceptClose )
  {
    if ( mCacheGeom && mCacheGeom->contains( &point ) )
      return mCacheValue;

    if ( mZoneWithoutPolygon->contains( &point ) )
      return std::numeric_limits<double>::quiet_NaN();
  }

  QgsRectangle rect( pt - QgsVector( mTolerance, mTolerance ), pt + QgsVector( mTolerance, mTolerance ) );

  QgsFeature feat;
  double value = std::numeric_limits<double>::max();
  int foundValues = 0;

  const QList<QgsFeatureId> featIds = mSpatialIndex->intersects( rect );

  for ( QgsFeatureId fid : featIds )
  {
    QgsGeometryEngine *engine = mGeomEngines.at( fid ).get();

    if ( ( acceptClose && engine->distance( &point ) < mTolerance )
         || ( engine->contains( &point ) ) )
    {
      double v = mValues.value( fid );
      foundValues++;
      if ( v < value )
        value = v;
      if ( !acceptClose )
      {
        mCacheGeom = engine;
        mCacheValue = v;
        break;
      }
    }
  }

  if ( foundValues > 0 )
    return value;
  else
    return std::numeric_limits<double>::quiet_NaN();
}

double ReosPolygonStructureValues_p::defaultValue() const
{
  return mDefaultValue;
}

void ReosPolygonStructureValues_p::setDefaultValue( double defVal )
{
  mDefaultValue = defVal;
}
