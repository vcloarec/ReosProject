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


ReosMeshFrameData ReosGmshGenerator::generatedMesh( bool *ok ) const
{
  try
  {

//  gmsh::option::setNumber( "Mesh.Algorithm", 8 );
//  gmsh::model::mesh::setRecombine( 2, 1 );

//  gmsh::model::mesh::field::add( "Constant", 1 );
//  gmsh::model::mesh::field::setNumbers( 1, "SurfacesList", {1} );
//  gmsh::model::mesh::field::setNumber( 1, "VIn", 10 );
//  gmsh::model::mesh::field::setAsBackgroundMesh( 1 );

    auto meshSizeCallback = []( int dim, int tag, double x, double y, double z,
                                double lc )
    {
      return lc;
    };

    gmsh::model::mesh::setSizeCallback( meshSizeCallback );


    gmsh::model::mesh::generate( 2 );
    *ok = true;

    std::vector<std::size_t>nodeTags;
    std::vector<double>  coord;
    std::vector<double>  parametricCoord;
    gmsh::model::mesh::getNodes( nodeTags, coord, parametricCoord, -1, -1, false, true );

    ReosMeshFrameData ret;
    ret.vertexCoordinates.resize( coord.size() );
    memcpy( ret.vertexCoordinates.data(), coord.data(), coord.size()*sizeof( double ) );

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
          ret.facesIndexes.append( face );
        }
      }
    }

    return ret;
  }
  catch ( ... )
  {
    *ok = false;
    return ReosMeshFrameData();
  }
}

void ReosGmshGenerator::setGeometryStructure( ReosPolylinesStructure *structure, const QString &crs )
{
  const ReosPolylinesStructure::Data structureData = structure->structuredLinesData( crs );

  gmsh::initialize();
  gmsh::model::add( "t1" );

  for ( int i = 0; i < structureData.vertices.count(); ++i )
  {
    const QPointF &pt = structureData.vertices.at( i );
    gmsh::model::geo::addPoint( pt.x(), pt.y(), 0, 0, i + 1 );
  }

  std::vector<int> surface;
  int boundVertCount = structureData.boundaryPointCount;
  for ( int i = 0; i < structureData.boundaryPointCount ; ++i )
  {
    gmsh::model::geo::addLine( i + 1, ( i + 1 ) % boundVertCount + 1, i + 1 );
    surface.push_back( i + 1 );
  }

  gmsh::model::geo::addCurveLoop( surface, 1 );
  gmsh::model::geo::addPlaneSurface( {1}, 1 );

  std::vector<int> internalLines( structureData.internalLines.count() );
  for ( int i = 0; i < structureData.internalLines.count() ; ++i )
  {
    const std::array<int, 2> &dataLine = structureData.internalLines.at( i );
    gmsh::model::geo::addLine( dataLine.at( 0 ) + 1, dataLine.at( 1 ) + 1, boundVertCount + i + 1 );
    internalLines[i] = boundVertCount + i + 1;
  }

  gmsh::model::geo::synchronize();
  gmsh::model::mesh::embed( 1, internalLines, 2, 1 );

}
