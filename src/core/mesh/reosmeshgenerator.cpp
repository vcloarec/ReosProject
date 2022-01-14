/***************************************************************************
  reosmeshgenerator.cpp - ReosMeshGenerator

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
#include "reosmeshgenerator.h"

#include<QHash>
#include "poly2tri.h"
#include "memory"

ReosMeshFrameData ReosMeshGeneratorPoly2Tri::generatedMesh() const
{
  std::vector<p2t::Point *> polyDomain;

  polyDomain.resize( mDomain.count() );

  try
  {
    QHash<p2t::Point *, int> mapPoly2TriPointToVertex;

    ReosMeshFrameData ret;
    ret.vertexCoordinates.resize( mDomain.count() * 3 );

    for ( int i = 0; i < mDomain.count(); ++i )
    {
      const QPointF &pt = mDomain.at( i );
      polyDomain[i] = new p2t::Point( pt.x(), pt.y() );
      mapPoly2TriPointToVertex.insert( polyDomain[i], i );

      ret.vertexCoordinates[i * 3] = pt.x();
      ret.vertexCoordinates[i * 3 + 1] = pt.x();
      ret.vertexCoordinates[i * 3 + 2] = 0;
    }
    std::unique_ptr<p2t::CDT> cdt( new p2t::CDT( polyDomain ) );
    cdt->Triangulate();

    const std::vector<p2t::Triangle *> &triangles = cdt->GetTriangles();

    if ( triangles.size() > __INT32_MAX__ )
      throw std::exception();

    int triangleCount = triangles.size();

    ret.facesIndexes.fill( QVector<int>( 3 ), triangleCount );

    for ( int t = 0; t < triangleCount; ++t )
    {
      p2t::Triangle *triangle = triangles.at( t );

      QVector<int> &reosTriangle = ret.facesIndexes[t];

      for ( int s = 0; s < 3; ++s )
      {
        reosTriangle[s] = mapPoly2TriPointToVertex.value( triangle->GetPoint( s ), -1 );
        if ( reosTriangle[s] == -1 )
          throw std::exception();
      }
    }

    qDeleteAll( polyDomain );

    return ret;
  }
  catch ( ... )
  {
    qDeleteAll( polyDomain );
    return ReosMeshFrameData();
  }
}

void ReosMeshGeneratorPoly2Tri::setDomain( const QPolygonF &domain )
{
  mDomain = domain;
}
