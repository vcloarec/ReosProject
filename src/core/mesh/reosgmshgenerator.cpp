/***************************************************************************
  reosgmshgenerator.cpp - ReosGmshGenerator

 ---------------------
 begin                : 14.1.2022
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
#include "reosgmshgenerator.h"

#include<QDebug>

#include <gmsh.h>

#include "reospolylinesstructure.h"

ReosGmshEngine *ReosGmshEngine::sInstance = nullptr;

ReosGmshGenerator::ReosGmshGenerator( QObject *parent )
  : ReosMeshGenerator( parent )
{
}

ReosGmshGenerator::ReosGmshGenerator( const ReosEncodedElement &element, QObject *parent )
  : ReosMeshGenerator( element, parent )
{
  int algInt = 5;
  element.getData( QStringLiteral( "algorithm" ), algInt );
  mAlgorithm = static_cast<Algorithm>( algInt );
}

ReosMeshGeneratorProcess *ReosGmshGenerator::getGenerateMeshProcess(
  ReosPolylinesStructure *structure,
  ReosMeshResolutionController *resolutionControler,
  const QString &destinationCrs ) const
{
  return new ReosMeshGeneratorGmshProcess( structure, resolutionControler, mAlgorithm, destinationCrs );
}

ReosEncodedElement ReosGmshGenerator::encode() const
{
  ReosEncodedElement ret = encodeBase();
  ret.addData( QStringLiteral( "type" ), type() );
  ret.addData( QStringLiteral( "algorithm" ), mAlgorithm );

  return ret;
}

ReosGmshGenerator::Algorithm ReosGmshGenerator::algorithm() const
{
  return mAlgorithm;
}

void ReosGmshGenerator::setAlgorithm( const Algorithm &algorithm )
{
  mAlgorithm = algorithm;
  emit dataChanged();
}

QString ReosGmshGenerator::algorithmName( ReosGmshGenerator::Algorithm alg )
{
  switch ( alg )
  {
    case ReosGmshGenerator::MeshAdapt:
      return tr( "MeshAdapt" );
      break;
    case ReosGmshGenerator::Automatic:
      return tr( "Automatic" );
      break;
    case ReosGmshGenerator::InitialMesh:
      return tr( "Initial mesh" );
      break;
    case ReosGmshGenerator::Delaunay:
      return tr( "Delaunay" );
      break;
    case ReosGmshGenerator::FrontalDelaunay:
      return tr( "Frontal Delaunay" );
      break;
    case ReosGmshGenerator::BAMG:
      return tr( "BAMG" );
      break;
//    case ReosGmshGenerator::FrontalDelaunayForQuads:
//      return tr( "Frontal Delaunay for Quads" );
//      break;
//    case ReosGmshGenerator::PackingOfParallelograms:
//      return tr( "Packing of Parallelograms" );
//      break;
    case ReosGmshGenerator::AlgCount:
      return QString();
      break;
  }
}

ReosMeshGeneratorGmshProcess::ReosMeshGeneratorGmshProcess( ReosPolylinesStructure *structure,
    ReosMeshResolutionController *resolutionControler,
    ReosGmshGenerator::Algorithm alg,
    const QString &destinationCrs )
  : mAlgorithm( alg )
  , mDestinationCrs( destinationCrs )
{
  if ( structure )
    mData = structure->structuredLinesData( destinationCrs );
  if ( resolutionControler )
    mResolutionControler.reset( resolutionControler->clone() );
}

void ReosMeshGeneratorGmshProcess::start()
{
  setMaxProgression( 0 );
  setInformation( tr( "Mesh generation in progress." ) );
  mResult = ReosGmshEngine::instance()->generateMesh( mData, mResolutionControler.get(), mAlgorithm, mDestinationCrs );
  mResult.extent = mData.extent;
  mIsSuccessful = true;
  finish();
}

ReosGmshEngine *ReosGmshEngine::instance()
{
  if ( !sInstance )
    sInstance = new ReosGmshEngine( nullptr );
  return sInstance;
}

ReosMeshFrameData ReosGmshEngine::generateMesh(
  const ReosPolylinesStructure::Data &data,
  ReosMeshResolutionController *resolutionControler,
  ReosGmshGenerator::Algorithm alg,
  const QString &destinationCrs )
{
  QMutexLocker locker( &mMutex );
  ReosMeshFrameData result;
  try
  {
    std::unique_ptr<ReosPolygonStructureValues> sizeValues;
    double defaultSize = 10;
    if ( resolutionControler )
    {
      sizeValues.reset( resolutionControler->resolutionPolygons()->values( destinationCrs ) );
      defaultSize = resolutionControler->defaultSize()->value();
    }

    gmsh::initialize();
    gmsh::model::add( "t1" );

    for ( int i = 0; i < data.vertices.count(); ++i )
    {
      const QPointF &pt = data.vertices.at( i );
      gmsh::model::geo::addPoint( pt.x(), pt.y(), 0, 0, i + 1 );
    }

    int boundVertCount = data.boundaryPointCount;

    std::vector<int> internalLines( data.internalLines.count() );
    for ( int i = 0; i < data.internalLines.count() ; ++i )
    {
      const QVector<int> &dataLine = data.internalLines.at( i );
      gmsh::model::geo::addLine( dataLine.at( 0 ) + 1, dataLine.at( 1 ) + 1, boundVertCount + i + 1 );
      internalLines[i] = boundVertCount + i + 1;
    }

    std::vector<int> externalBoundary;
    for ( int i = 0; i < data.boundaryPointCount ; ++i )
    {
      gmsh::model::geo::addLine( i + 1, ( i + 1 ) % boundVertCount + 1, i + 1 );
      externalBoundary.push_back( i + 1 );
    }

    gmsh::model::geo::addCurveLoop( externalBoundary, 1 );
    std::vector<int> surfaces;
    surfaces.push_back( 1 );

    for ( int i = 0; i < data.holes.count(); ++i )
    {
      std::vector<int> hole( static_cast<size_t>( data.holes.at( i ).count() ) );
      for ( size_t j = 0; j < hole.size(); ++j )
      {
        hole[j] = data.holes.at( i ).at( int( j ) ) + boundVertCount + 1;
      }
      gmsh::model::geo::addCurveLoop( hole, i + 2, true );
      surfaces.push_back( i + 2 );
    }
    gmsh::model::geo::addPlaneSurface( surfaces, 1 );

    gmsh::model::geo::synchronize();
    gmsh::model::mesh::embed( 1, internalLines, 2, 1 );

    gmsh::option::setNumber( "Mesh.Algorithm", alg + 1 );

    auto sizeFallBack = [&sizeValues, defaultSize]( int dim, int, double x, double y, double, double lc )
    {
      if ( !sizeValues )
        return lc;

      double sizeValue = sizeValues->value( x, y, dim == 1 | dim == 0 );

      if ( std::isnan( sizeValue ) )
        sizeValue = defaultSize;

      return sizeValue;
    };

    gmsh::model::mesh::setSizeCallback( sizeFallBack );

    gmsh::model::mesh::generate( 2 );

    std::vector<std::size_t>nodeTags;
    std::vector<double>  coord;
    std::vector<double>  parametricCoord;
    gmsh::model::mesh::getNodes( nodeTags, coord, parametricCoord, -1, -1, false, true );

    result.vertexCoordinates.resize( coord.size() );
    memcpy( result.vertexCoordinates.data(), coord.data(), coord.size()*sizeof( double ) );

    QHash<size_t, int> tagToVertexIndex;
    for ( size_t i = 0; i < nodeTags.size(); ++i )
      tagToVertexIndex.insert( nodeTags.at( i ), int( i ) );

    std::vector<int> elementTypes;
    std::vector<std::vector<std::size_t> > elementTags;
    std::vector<std::vector<std::size_t> > nodeElemTags;

    gmsh::model::mesh::getElements( elementTypes, elementTags, nodeElemTags, 2, -1 );

    for ( size_t type = 0; type < elementTypes.size(); ++type )
    {
      int elementType = elementTypes[type];
      int elementSize = 0;
      switch ( elementType )
      {
        case 2:
          elementSize = 3;
          break;
        case 3:
          elementSize = 4;
          break;
        default:
          break;
      }
      const std::vector<size_t> &elementsNodes = nodeElemTags.at( type );

      if ( elementSize > 2 )
      {
        for ( size_t i = 0; i < elementsNodes.size() / elementSize; ++i )
        {
          QVector<int> face( elementSize );
          for ( int j = 0; j < elementSize; ++j )
            face[j] = tagToVertexIndex.value( elementsNodes.at( static_cast<size_t>( i * elementSize + j ) ) );
          result.facesIndexes.append( face );
        }
      }
    }
  }
  catch ( ... )
  {
    result = ReosMeshFrameData();
  }

  gmsh::finalize();

  return result;
}

void ReosGmshEngine::instantiate( QObject *parent )
{
  if ( !sInstance )
    sInstance = new ReosGmshEngine( parent );
}

ReosGmshEngine::ReosGmshEngine( QObject *parent ): ReosModule( parent )
{
}
