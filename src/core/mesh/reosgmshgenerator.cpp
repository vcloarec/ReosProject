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

ReosMeshGeneratorProcess *ReosGmshGenerator::generatedMesh(
  ReosPolylinesStructure *structure,
  ReosMeshResolutionController *resolutionControler,
  bool *ok ) const
{
  *ok = true;
  return new ReosMeshGeneratorGmshProcess( structure, resolutionControler, mAlgorithm );
}

ReosEncodedElement ReosGmshGenerator::encode() const
{
  ReosEncodedElement ret = encodeBase();
  ret.addData( QStringLiteral( "type" ), QStringLiteral( "gmsh" ) );
  ret.addData( QStringLiteral( "algorithm" ), mAlgorithm );

  return ret;
}

bool ReosGmshEngine::isBusy() const
{
  return mIsBusy;
}

void ReosGmshEngine::initialize()
{
  QMutexLocker locker( &mMutex );

  if ( mIsBusy )
    return;

  gmsh::initialize();
}

ReosMeshGeneratorGmshProcess::ReosMeshGeneratorGmshProcess( ReosPolylinesStructure *structure, ReosMeshResolutionController *resolutionControler, ReosGmshGenerator::Algorithm alg )
{
  if ( structure )
    mData = structure->structuredLinesData();
  if ( resolutionControler )
    mResolutionControler.reset( resolutionControler->clone() );
}

void ReosMeshGeneratorGmshProcess::start()
{
  gmsh::initialize();
  gmsh::model::add( "t1" );

  for ( int i = 0; i < mData.vertices.count(); ++i )
  {
    const QPointF &pt = mData.vertices.at( i );
    gmsh::model::geo::addPoint( pt.x(), pt.y(), 0, 0, i + 1 );
  }


  int boundVertCount = mData.boundaryPointCount;

  std::vector<int> internalLines( mData.internalLines.count() );
  for ( int i = 0; i < mData.internalLines.count() ; ++i )
  {
    const QVector<int> &dataLine = mData.internalLines.at( i );
    gmsh::model::geo::addLine( dataLine.at( 0 ) + 1, dataLine.at( 1 ) + 1, boundVertCount + i + 1 );
    internalLines[i] = boundVertCount + i + 1;
  }

  std::vector<int> externalBoundary;
  for ( int i = 0; i < mData.boundaryPointCount ; ++i )
  {
    gmsh::model::geo::addLine( i + 1, ( i + 1 ) % boundVertCount + 1, i + 1 );
    externalBoundary.push_back( i + 1 );
  }

  gmsh::model::geo::addCurveLoop( externalBoundary, 1 );
  std::vector<int> surfaces;
  surfaces.push_back( 1 );

  for ( int i = 0; i < mData.holes.count(); ++i )
  {
    std::vector<int> hole( static_cast<size_t>( mData.holes.at( i ).count() ) );
    for ( size_t j = 0; j < hole.size(); ++j )
    {
      hole[j] = mData.holes.at( i ).at( int( j ) ) + boundVertCount + 1;
    }
    gmsh::model::geo::addCurveLoop( hole, i + 2, true );
    surfaces.push_back( i + 2 );
  }
  gmsh::model::geo::addPlaneSurface( surfaces, 1 );

  gmsh::model::geo::synchronize();
  gmsh::model::mesh::embed( 1, internalLines, 2, 1 );

  gmsh::option::setNumber( "Mesh.Algorithm", mAlgorithm + 1 );

  try
  {
    gmsh::model::mesh::setSizeCallback( std::bind( &ReosMeshGeneratorGmshProcess::sizeFallBack,
                                        this,
                                        std::placeholders::_1,
                                        std::placeholders::_2,
                                        std::placeholders::_3,
                                        std::placeholders::_4,
                                        std::placeholders::_5,
                                        std::placeholders::_6 ) );

    gmsh::model::mesh::generate( 2 );

    std::vector<std::size_t>nodeTags;
    std::vector<double>  coord;
    std::vector<double>  parametricCoord;
    gmsh::model::mesh::getNodes( nodeTags, coord, parametricCoord, -1, -1, false, true );

    mResult = ReosMeshFrameData();
    mResult.vertexCoordinates.resize( coord.size() );
    memcpy( mResult.vertexCoordinates.data(), coord.data(), coord.size()*sizeof( double ) );

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
          mResult.facesIndexes.append( face );
        }
      }
    }
    mIsSuccessful = true;
  }
  catch ( ... )
  {
    mIsSuccessful = false;
    mResult = ReosMeshFrameData();
  }
}

double ReosMeshGeneratorGmshProcess::sizeFallBack( int dim, int, double x, double y, double, double lc )
{
  if ( !mResolutionControler )
    return lc;

  return mResolutionControler->elementSizeAt( x, y, dim == 1 | dim == 0 );
}
