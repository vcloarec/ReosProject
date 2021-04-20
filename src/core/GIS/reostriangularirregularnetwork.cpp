/***************************************************************************
  reostriangularirregularnetwork.cpp - ReosTriangularIrregularNetwork

 ---------------------
 begin                : 12.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostriangularirregularnetwork.h"

#include <qgsdualedgetriangulation.h>

#include <qgsprocessingparametertininputlayers.h>
#include <qgsprocessingcontext.h>
#include <qgsprocessingutils.h>
#include <qgscurvepolygon.h>
#include <qgsmultisurface.h>
#include <qgsmulticurve.h>
#include <qgscurve.h>


bool ReosTriangularIrregularNetwork::autoUpdate() const
{
  return mAutoUpdate;
}

void ReosTriangularIrregularNetwork::setAutoUpdate( bool autoUpdate )
{
  mAutoUpdate = autoUpdate;
}

ReosAddVectorLayersToTinProcess::~ReosAddVectorLayersToTinProcess() = default;

ReosAddVectorLayersToTinProcess::ReosAddVectorLayersToTinProcess( ReosTriangularIrregularNetwork *tin ):
  mTin( tin )
{}

void ReosAddVectorLayersToTinProcess::setLayerData( const QVariant &layerData )
{
  if ( layerData.type() != QVariant::List )
    return;

  const QVariantList layersList = layerData.toList();
  for ( const QVariant &layer : layersList )
  {
    const QVariantMap layerMap = layer.toMap();
    const QString layerSource = layerMap.value( QStringLiteral( "source" ) ).toString();
    const QgsProcessingParameterTinInputLayers::Type type =
      static_cast<QgsProcessingParameterTinInputLayers::Type>( layerMap.value( QStringLiteral( "type" ) ).toInt() );

    int attributeIndex = layerMap.value( QStringLiteral( "attributeIndex" ) ).toInt();

    QgsProcessingContext processingContext;
    processingContext.setProject( QgsProject::instance() );

    QgsProcessingFeatureSource *fs = QgsProcessingUtils::variantToSource( layerSource, processingContext );
    if ( !fs )
      continue;

    switch ( type )
    {
      case QgsProcessingParameterTinInputLayers::Vertices:
        mVertexFeatureSources.emplace_back( fs );
        mVertexTransform.emplace_back( new QgsCoordinateTransform( mVertexFeatureSources.back()->sourceCrs(), QgsProject::instance()->crs(), QgsProject::instance() ) );
        mVertexAttributes.append( attributeIndex );
        break;
      case QgsProcessingParameterTinInputLayers::BreakLines:
        mConstraintFeatureSources.emplace_back( fs );
        mConstraintTransform.emplace_back( new QgsCoordinateTransform( mConstraintFeatureSources.back()->sourceCrs(), QgsProject::instance()->crs(), QgsProject::instance() ) );
        mConstraintAttributes.append( attributeIndex );
        break;
      default:
        break;
    }
  }
}

static void addVerticesFromFeature( ReosTriangularIrregularNetwork *tin, const QgsFeature &feature, int valueAttribute, const QgsCoordinateTransform &transform, ReosProcess *process )
{
  QgsGeometry geom = feature.geometry();
  try
  {
    geom.transform( transform, QgsCoordinateTransform::ForwardTransform, true );
  }
  catch ( QgsCsException &cse )
  {
    return;
  }

  QgsAbstractGeometry::vertex_iterator vit = geom.vertices_begin();

  double value = 0;
  if ( valueAttribute >= 0 )
    value = feature.attribute( valueAttribute ).toDouble();

  while ( vit != geom.vertices_end() )
  {
    if ( process && process->isStop() )
      break;
    ReosTriangularIrregularNetwork::Vertex vert;
    vert.x = ( *vit ).x();
    vert.y = ( *vit ).y();
    if ( valueAttribute < 0 )
      vert.z = ( *vit ).z();
    else
      vert.z = value;

    tin->addVertex( vert );
    ++vit;
  }
}

static void addBreakLinesFromFeature( ReosTriangularIrregularNetwork *tin, const QgsFeature &feature, int valueAttribute, const QgsCoordinateTransform &transform, ReosProcess *process )
{
  double valueOnVertex = 0;
  if ( valueAttribute >= 0 )
    valueOnVertex = feature.attribute( valueAttribute ).toDouble();

  //from QgsTinInterpolator::insertData()
  std::vector<const QgsCurve *> curves;
  QgsGeometry geom = feature.geometry();
  try
  {
    geom.transform( transform, QgsCoordinateTransform::ForwardTransform, true );
  }
  catch ( QgsCsException &cse )
  {
    return;
  }

  if ( QgsWkbTypes::geometryType( geom.wkbType() ) == QgsWkbTypes::PolygonGeometry )
  {
    std::vector< const QgsCurvePolygon * > polygons;
    if ( geom.isMultipart() )
    {
      const QgsMultiSurface *ms = qgsgeometry_cast< const QgsMultiSurface * >( geom.constGet() );
      for ( int i = 0; i < ms->numGeometries(); ++i )
      {
        polygons.emplace_back( qgsgeometry_cast< const QgsCurvePolygon * >( ms->geometryN( i ) ) );
      }
    }
    else
    {
      polygons.emplace_back( qgsgeometry_cast< const QgsCurvePolygon * >( geom.constGet() ) );
    }

    for ( const QgsCurvePolygon *polygon : polygons )
    {
      if ( process && process->isStop() )
        break;
      if ( !polygon )
        continue;

      if ( polygon->exteriorRing() )
        curves.emplace_back( polygon->exteriorRing() );

      for ( int i = 0; i < polygon->numInteriorRings(); ++i )
      {
        if ( process && process->isStop() )
          break;
        curves.emplace_back( polygon->interiorRing( i ) );
      }
    }
  }
  else
  {
    if ( geom.isMultipart() )
    {
      const QgsMultiCurve *mc = qgsgeometry_cast< const QgsMultiCurve * >( geom.constGet() );
      for ( int i = 0; i < mc->numGeometries(); ++i )
      {
        if ( process && process->isStop() )
          break;
        curves.emplace_back( qgsgeometry_cast< const QgsCurve * >( mc->geometryN( i ) ) );
      }
    }
    else
    {
      curves.emplace_back( qgsgeometry_cast< const QgsCurve * >( geom.constGet() ) );
    }
  }

  for ( const QgsCurve *curve : curves )
  {
    if ( !curve )
      continue;

    if ( process && process->isStop() )
      break;

    QgsPointSequence linePoints;
    curve->points( linePoints );
    QVector<ReosTriangularIrregularNetwork::Vertex> vertices( linePoints.count() );
    for ( int i = 0; i < linePoints.count(); ++i )
    {
      if ( process && process->isStop() )
        break;
      vertices[i].x = linePoints.at( i ).x();
      vertices[i].y = linePoints.at( i ).y();
      if ( valueAttribute < 0 )
        vertices[i].z = linePoints.at( i ).z();
      else
        vertices[i].z = valueOnVertex;
    }

    tin->addConstraintLine( vertices );
  }
}

void ReosAddVectorLayersToTinProcess::start()
{
  if ( !mTin )
    return;

  // add vertices
  assert( static_cast<int>( mVertexFeatureSources.size() ) == mVertexAttributes.count() );
  assert( mVertexFeatureSources.size() == mVertexTransform.size() );

  for ( int i = 0; i < static_cast<int>( mVertexFeatureSources.size() ); ++i )
  {
    if ( mVertexFeatureSources.at( i ) )
    {
      QgsFeatureIterator it = mVertexFeatureSources.at( i )->getFeatures();
      setInformation( tr( "Add vertices from: %1" ).arg( mVertexFeatureSources.at( i )->sourceName() ) );
      int featureCount = mVertexFeatureSources.at( i )->featureCount();
      setMaxProgression( 100 );
      QgsFeature feat;
      long f = 0;
      while ( it.nextFeature( feat ) )
      {
        if ( isStop() )
          break;
        setCurrentProgression( ( 100.0 * f ) / featureCount );
        f++;
        addVerticesFromFeature( mTin, feat, mVertexAttributes.at( i ), *( mVertexTransform.at( i ).get() ), this );
      }
    }
  }

  // add constraint lines
  assert( static_cast<int>( mConstraintFeatureSources.size() ) == mConstraintAttributes.count() );
  assert( mConstraintFeatureSources.size() == mConstraintTransform.size() );

  for ( int i = 0; i < static_cast<int>( mConstraintFeatureSources.size() ); ++i )
  {
    if ( mConstraintFeatureSources.at( i ) )
    {
      QgsFeatureIterator it = mConstraintFeatureSources.at( i )->getFeatures();
      setInformation( tr( "Add vertices from: %1" ).arg( mConstraintFeatureSources.at( i )->sourceName() ) );
      int featureCount = mConstraintFeatureSources.at( i )->featureCount();
      setMaxProgression( 100 );
      QgsFeature feat;
      long f = 0;
      while ( it.nextFeature( feat ) )
      {
        if ( isStop() )
          break;
        setCurrentProgression( ( 100.0 * f ) / featureCount );
        f++;
        addBreakLinesFromFeature( mTin, feat, mConstraintAttributes.at( i ), *( mConstraintTransform.at( i ).get() ), this );
      }
    }
  }

}
