/***************************************************************************
  reospolygonwatershed_p.cpp - ReosPolygonWatershed_p

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
#include <memory>

#include <qgslinestring.h>
#include <qgspolygon.h>
#include <qgsfillsymbol.h>
#include <qgsgeometryutils.h>
#include <qgscategorizedsymbolrenderer.h>
#include <qgssinglesymbolrenderer.h>


#include <qgsfillsymbollayer.h>

#include "reospolygonwatershed_p.h"
#include "reoswatershed.h"


ReosPolygonWatershed_p::ReosPolygonWatershed_p( const QString &wktCrs )
  : ReosGeometryComplex_p( QStringLiteral( "Polygon" ), wktCrs )
{
  mVectorLayer->startEditing();
  mVectorLayer->extent();

  std::unique_ptr<QgsFillSymbol> fillSymbol = std::make_unique<QgsFillSymbol>();
  QgsSymbolLayer *symbLayer = fillSymbol->symbolLayers().at( 0 );
  QgsSimpleFillSymbolLayer *fillLayer = static_cast<QgsSimpleFillSymbolLayer *>( symbLayer );
  fillLayer->setBrushStyle( Qt::NoBrush );
  fillLayer->setStrokeColor( QColor( 0, 200, 100 ) );
  fillLayer->setStrokeWidth( 4 );
  fillLayer->setStrokeWidthUnit( Qgis::RenderUnit::Pixels );

  std::unique_ptr<QgsSimpleFillSymbolLayer> externalLayer( new QgsSimpleFillSymbolLayer );
  externalLayer->setBrushStyle( Qt::NoBrush );
  externalLayer->setStrokeColor( QColor( 0, 0, 0 ) );
  externalLayer->setStrokeWidth( 7 );
  externalLayer->setStrokeWidthUnit( Qgis::RenderUnit::Pixels );

  fillSymbol->insertSymbolLayer( 0, externalLayer.release() );

  mRenderer = new QgsSingleSymbolRenderer( fillSymbol.release() );
  mVectorLayer->setRenderer( mRenderer );

  QgsField field;
  field.setType( QMetaType::QString );
  field.setName( QStringLiteral( "watershedId" ) );
  mVectorLayer->addAttribute( field );
  QgsField fieldResidual;
  fieldResidual.setType( QMetaType::Bool );
  fieldResidual.setName( QStringLiteral( "residual" ) );
  mVectorLayer->addAttribute( fieldResidual );
}

ReosPolygonWatershed *ReosPolygonWatershed_p::clone() const
{
  std::unique_ptr<ReosPolygonWatershed_p> other = std::make_unique<ReosPolygonWatershed_p>();
  other->mVectorLayer.reset( mVectorLayer->clone() );

  return other.release();
}

QObject *ReosPolygonWatershed_p::data()
{
  return mVectorLayer.get();
}

void ReosPolygonWatershed_p::addWatershed( const QPolygonF &watershed, const QString &crs, const QString &id, ReosWatershed::Type type )
{
  const QgsCoordinateTransform transform = toLayerTransform( crs );

  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( watershed ) );
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

  QPolygonF poly = layerGeom.asQPolygonF();

  const QgsFields fields = mVectorLayer->fields();
  QgsFeature feat;
  feat.setGeometry( layerGeom );
  feat.setFields( fields, true );
  feat.setAttribute( QStringLiteral( "watershedId" ), id );
  feat.setAttribute( QStringLiteral( "residual" ), type == ReosWatershed::Residual );
  mVectorLayer->addFeature( feat );

  emit geometryChanged();
}

void ReosPolygonWatershed_p::addWatershed( ReosWatershed *watershed )
{
  addWatershed( watershed->delineating(), watershed->crs(), watershed->id(), watershed->watershedType() );
}

void ReosPolygonWatershed_p::removeWatershed( const QString &id )
{
  QgsFeatureRequest request;
  request.setFilterExpression( QStringLiteral( "watershedId = '%1'" ).arg( id ) );
  mVectorLayer->selectedFeatureIds();
  QgsFeatureIterator featIt = mVectorLayer->getFeatures( request );

  QgsFeature feat;
  while ( featIt.nextFeature( feat ) )
    mVectorLayer->deleteFeature( feat.id() );
}

QPointF ReosPolygonWatershed_p::closestVertex( const QString &watershedId, const QPointF &position, const QString &destinationCrs, double tolerance, int &prevIndex, int &index, int &nextIndex ) const
{
  prevIndex = -1;
  index = -1;
  nextIndex = -1;
  QgsGeometry geom = getGeometry( watershedId, destinationCrs );
  if ( geom.isNull() )
    return QPointF();

  return geom.closestVertex( QgsPointXY( position ), index, nextIndex, prevIndex, tolerance ).toQPointF();
}

QPointF ReosPolygonWatershed_p::vertexPosition( const QString &watershedId, int index, const QString &destinationCrs ) const
{
  QgsGeometry geom = getGeometry( watershedId, destinationCrs );
  return geom.vertexAt( index ).toQPointF();
}

ReosMapExtent ReosPolygonWatershed_p::watershedExtent( const QString &watershedI, const QString &destinationCrs ) const
{
  const QPolygonF &delineating = watershedDelineating( watershedI, destinationCrs );
  return ReosMapExtent( delineating, destinationCrs );
}

QPolygonF ReosPolygonWatershed_p::watershedDelineating( const QString &watershedId, const QString &destinationCrs ) const
{
  QgsFeatureRequest request;
  request.setFilterExpression( QStringLiteral( "watershedId = '%1'" ).arg( watershedId ) );
  mVectorLayer->selectedFeatureIds();
  QgsFeatureIterator featIt = mVectorLayer->getFeatures( request );

  QgsFeature feat;
  while ( featIt.nextFeature( feat ) )
  {
    QgsGeometry geom = feat.geometry();
    const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );

    try
    {
      Qgis::GeometryOperationResult res = geom.transform( transform );
      if ( res != Qgis::GeometryOperationResult::Success )
        return feat.geometry().asQPolygonF();
    }
    catch ( QgsCsException & )
    {
      return feat.geometry().asQPolygonF();
    }

    return geom.asQPolygonF();
  }

  return QPolygonF();
}

QString ReosPolygonWatershed_p::watershedUnderPosition( const QPointF &position, const QString &destinationCrs ) const
{
  const QgsFeature &feature = getFeatureUnderPosition( position, destinationCrs );
  if ( feature.isValid() )
    return feature.attribute( QStringLiteral( "watershedId" ) ).toString();

  return QString();
}

static bool segmentsIntersect( const QPointF &p1, const QPointF &p2, const QPointF &q1, const QPointF &q2 )
{
  // Note: QgsGeometryUtils::segmentIntersection() 's "isIntersection" out-parameter is TRUE as soon as the
  // segments touch at all, including when they merely share an endpoint (which is expected for edges
  // that are adjacent in the ring). What we actually want here is a *proper* crossing, which is given by
  // the function's return value (with the default acceptImproperIntersection = false).
  QgsPoint interPoint;
  bool isIntersection = false;
  return QgsGeometryUtils::segmentIntersection( QgsPoint( p1 ), QgsPoint( p2 ), QgsPoint( q1 ), QgsPoint( q2 ), interPoint, isIntersection );
}

bool ReosPolygonWatershed_p::canVertexBeMoved( const QString &watershedId, int index, const QPointF &destinationPosition, const QString &destinationCrs ) const
{
  // Testing self intersection on the whole polygon (QgsGeometryUtils::selfIntersections()) for every single
  // vertex move is O(n²) and becomes far too slow (well beyond a millisecond) on polygons with many vertices.
  // As only the two edges touching the moved vertex actually change, it is enough to check those two edges
  // against all the other edges of the ring, which is O(n) and keeps vertex dragging responsive.
  const QgsGeometry geom = getGeometry( watershedId, destinationCrs );

  QPolygonF ring = geom.asQPolygonF();
  const int vertexCount = ring.size() - 1; // last point duplicates the first one (closed ring)
  if ( vertexCount < 3 )
    return true;

  const int movedIndex = index % vertexCount;
  const int prevIndex = ( movedIndex - 1 + vertexCount ) % vertexCount;
  const int nextIndex = ( movedIndex + 1 ) % vertexCount;
  const int prevPrevIndex = ( prevIndex - 1 + vertexCount ) % vertexCount;

  ring[movedIndex] = destinationPosition;
  if ( movedIndex == 0 )
    ring[vertexCount] = destinationPosition;

  const QPointF prevPoint = ring.at( prevIndex );
  const QPointF movedPoint = ring.at( movedIndex );
  const QPointF nextPoint = ring.at( nextIndex );

  for ( int i = 0; i < vertexCount; ++i )
  {
    if ( i == prevIndex || i == movedIndex )
      continue;

    const QPointF a = ring.at( i );
    const QPointF b = ring.at( i + 1 );

    if ( i != prevPrevIndex && segmentsIntersect( prevPoint, movedPoint, a, b ) )
      return false;

    if ( i != nextIndex && segmentsIntersect( movedPoint, nextPoint, a, b ) )
      return false;
  }

  return true;
}

void ReosPolygonWatershed_p::moveVertex( const QString &watershedId, int index, const QPointF &destinationPosition, const QString &destinationCrs )
{
  QgsFeatureRequest request;
  request.setFilterExpression( QStringLiteral( "watershedId = '%1'" ).arg( watershedId ) );
  QgsFeatureIterator featIt = mVectorLayer->getFeatures( request );

  QgsFeature feat;
  while ( featIt.nextFeature( feat ) )
  {
    QgsGeometry geom = feat.geometry();
    const QgsCoordinateTransform transform = toLayerTransform( destinationCrs );

    QgsPointXY movedPoint;
    try
    {
      movedPoint = transform.transform( QgsPointXY( destinationPosition ) );
    }
    catch ( QgsCsException & )
    {
      movedPoint = QgsPointXY( destinationPosition );
    }

    geom.moveVertex( QgsPoint( movedPoint ), index );

    feat.setGeometry( geom );
    mVectorLayer->updateFeature( feat );
    mVectorLayer->commitChanges( false );
    mCacheGeometry = QPair<QPair<QString, QString>, QgsGeometry>();
  }
}

void ReosPolygonWatershed_p::clear()
{
  mCacheGeometry = QPair<QPair<QString, QString>, QgsGeometry>();

  QgsFeatureIterator features = mVectorLayer->getFeatures();
  QgsFeature feat;
  while ( features.nextFeature( feat ) )
    mVectorLayer->deleteFeature( feat.id() );
}

ReosMapExtent ReosPolygonWatershed_p::extent( const QString &crs ) const
{
  return ReosGeometryComplex_p::extent( crs );
}

void ReosPolygonWatershed_p::setCrs( const QString &crs )
{
  setLayerCrs( crs );
}

QString ReosPolygonWatershed_p::crs() const
{
  return mVectorLayer->crs().toWkt( Qgis::CrsWktVariant::PreferredSimplified );
}

void ReosPolygonWatershed_p::render( void *mapSettings, QPainter *painter, bool highlight, const QPointF &highlightPosition ) const
{
  const QgsMapSettings &qgsMapSettings = *static_cast<QgsMapSettings *>( mapSettings );
  renderGeometry( qgsMapSettings, painter );

  if ( highlight )
  {
    const QString destinationCrs = qgsMapSettings.destinationCrs().toWkt( Qgis::CrsWktVariant::PreferredSimplified );
    QgsPointXY point( highlightPosition );
    bool contained = false;
    QgsGeometry geom;
    if ( mCacheHighlightedGeometry.first == destinationCrs )
    {
      geom = mCacheHighlightedGeometry.second;
      contained = geom.contains( &point );
    }

    if ( !contained )
    {
      QString expression = QStringLiteral( "residual = false" );
      QgsFeature feat = getFeatureUnderPosition( highlightPosition, destinationCrs, expression );
      if ( feat.isValid() )
      {
        QgsGeometry geomCandidate = feat.geometry();
        const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );

        try
        {
          Qgis::GeometryOperationResult res = geomCandidate.transform( transform );
          if ( res != Qgis::GeometryOperationResult::Success )
            geomCandidate = feat.geometry();
        }
        catch ( QgsCsException & )
        {
          geomCandidate = feat.geometry();
        }

        contained = geomCandidate.contains( &point );
        if ( contained )
        {
          geom = geomCandidate;
          mCacheHighlightedGeometry.first = destinationCrs;
          mCacheHighlightedGeometry.second = geomCandidate;
        }
      }
      else
      {
        mCacheHighlightedGeometry.first.clear();
        mCacheHighlightedGeometry.second = QgsGeometry();
      }
    }


    if ( contained && !geom.isNull() )
    {
      const QPolygonF &polygon = geom.asQPolygonF();
      if ( !polygon.isEmpty() )
      {
        const QgsMapToPixel &mapToPixel = qgsMapSettings.mapToPixel();
        QPolygonF mDevicePolygon;
        for ( auto &p : polygon )
        {
          qreal x = p.x();
          qreal y = p.y();
          mapToPixel.transformInPlace( x, y );
          if ( mDevicePolygon.isEmpty() || std::abs( x - mDevicePolygon.last().x() ) > 1 || std::abs( y - mDevicePolygon.last().y() ) > 1 )
            mDevicePolygon.append( QPointF( x, y ) );
        }
        painter->save();
        QPen pen;
        pen.setColor( QColor( 0, 200, 100 ).lighter( 150 ) );
        pen.setWidthF( 5 );
        painter->setPen( pen );
        painter->drawPolygon( mDevicePolygon );
        painter->restore();
      }
    }
  }
}

QgsGeometry ReosPolygonWatershed_p::getGeometry( const QString &watershedId, const QString &destinationCrs ) const
{
  QgsGeometry geom;
  if ( watershedId == mCacheGeometry.first.first && destinationCrs == mCacheGeometry.first.second )
    geom = mCacheGeometry.second;
  else
  {
    QgsFeatureRequest request;
    request.setFilterExpression( QStringLiteral( "watershedId = '%1'" ).arg( watershedId ) );
    QgsFeatureIterator featIt = mVectorLayer->getFeatures( request );

    QgsFeature feat;
    if ( featIt.nextFeature( feat ) )
      geom = feat.geometry();

    if ( geom.isNull() )
      return geom;

    const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );

    try
    {
      Qgis::GeometryOperationResult res = geom.transform( transform );
      if ( res != Qgis::GeometryOperationResult::Success )
        geom = feat.geometry();
    }
    catch ( QgsCsException & )
    {
      geom = feat.geometry();
    }

    mCacheGeometry.first.first = watershedId;
    mCacheGeometry.first.second = destinationCrs;
    mCacheGeometry.second = geom;
  }

  return geom;
}

QgsFeature ReosPolygonWatershed_p::getFeatureUnderPosition( const QPointF &position, const QString &destinationCrs, const QString &expression ) const
{
  QgsPointXY point( position );
  point = toLayerTransform( destinationCrs ).transform( point );
  QgsFeatureRequest request = QgsFeatureRequest().setFilterRect( QgsRectangle( point, point ) );
  if ( !expression.isEmpty() )
    request.setFilterExpression( expression );

  QgsFeatureIterator featIt = mVectorLayer->getFeatures();

  QgsFeature feat;
  QgsFeature selectedFeature;
  while ( featIt.nextFeature( feat ) )
  {
    if ( feat.geometry().contains( &point ) )
      if ( !selectedFeature.isValid() || feat.geometry().area() < selectedFeature.geometry().area() )
        selectedFeature = feat;
  }

  return selectedFeature;
}
