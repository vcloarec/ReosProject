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
#include<QDebug>

#include "poly2tri.h"
#include "memory"

#include "reosparameter.h"
#include "reospolylinesstructure.h"
#include "reospolygonstructure.h"
#include "reosgmshgenerator.h"

ReosMeshGeneratorProcess *ReosMeshGeneratorPoly2Tri::generatedMesh(
  ReosPolylinesStructure *structure,
  ReosMeshResolutionController *,
  bool *ok ) const
{
  if ( ok )
    *ok = true;

  return new  ReosMeshGeneratorPoly2TriProcess( structure->boundary() );
}

void ReosMeshGeneratorPoly2Tri::setDomain( const QPolygonF &domain )
{
  mDomain = domain;
}

ReosEncodedElement ReosMeshGeneratorPoly2Tri::encode() const {return ReosEncodedElement( QString() );}

ReosMeshGenerator::ReosMeshGenerator( QObject *parent )
  : QObject( parent )
  , mAutoUpdateParameter( new ReosParameterBoolean( tr( "Auto update mesh" ), false, this ) )
{
  mAutoUpdateParameter->setValue( true );
}

ReosParameterBoolean *ReosMeshGenerator::autoUpdateParameter() const
{
  return mAutoUpdateParameter;
}

ReosMeshGenerator *ReosMeshGenerator::createMeshGenerator( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() == QStringLiteral( "mesh-generator" ) )
  {
    QString type;
    element.getData( QStringLiteral( "type" ), type );

    if ( type == QStringLiteral( "gmsh" ) )
      return new ReosGmshGenerator( element, parent );
  }

  return new ReosGmshGenerator( parent );
}

ReosMeshGenerator::ReosMeshGenerator( const ReosEncodedElement &element, QObject *parent )
  : ReosMeshGenerator( parent )
{
  if ( element.description() != QStringLiteral( "mesh-generator" ) )
    return;
  bool autoUpdate = false;
  element.getData( QStringLiteral( "auto-update" ), autoUpdate );
  mAutoUpdateParameter->setValue( autoUpdate );
}

ReosEncodedElement ReosMeshGenerator::encodeBase() const
{
  ReosEncodedElement element( QStringLiteral( "mesh-generator" ) );
  element.addData( QStringLiteral( "auto-update" ), mAutoUpdateParameter->value() );

  return element;
}

ReosMeshResolutionController::ReosMeshResolutionController( QObject *parent, const QString &wktCrs )
  : ReosDataObject( parent )
  , mDefaultSize( new ReosParameterDouble( tr( "Default element size" ), false, this ) )
  , mPolygonStructure( ReosPolygonStructure::createPolygonStructure( wktCrs ) )
{
  mDefaultSize->setValue( 10 );

  connect( mDefaultSize, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mPolygonStructure.get(), &ReosDataObject::dataChanged, this, &ReosDataObject::dataChanged );
}

ReosMeshResolutionController::~ReosMeshResolutionController()
{
}

ReosMeshResolutionController *ReosMeshResolutionController::clone() const
{
  return new ReosMeshResolutionController( this );
}

double ReosMeshResolutionController::elementSizeAt( double x, double y, bool exact )
{
  double value = mPolygonStructure->value( ReosSpatialPosition( x, y ), exact );

  if ( !std::isnan( value ) )
    return value;

  return mDefaultSize->value();
}

ReosPolygonStructure *ReosMeshResolutionController::resolutionPolygons() const
{
  return mPolygonStructure.get();
}

ReosMeshResolutionController::ReosMeshResolutionController( const ReosMeshResolutionController *other )
{
  mPolygonStructure.reset( other->mPolygonStructure->clone() );
  mDefaultSize = new ReosParameterDouble( other->mDefaultSize->name(), false, this );
  mDefaultSize->setValue( other->mDefaultSize->value() );
}

ReosParameterDouble *ReosMeshResolutionController::defaultSize() const
{
  return mDefaultSize;
}

ReosMeshGeneratorPoly2TriProcess::ReosMeshGeneratorPoly2TriProcess( const QPolygonF &domain )
  : mDomain( domain )
{}

void ReosMeshGeneratorPoly2TriProcess::start()
{
  mIsSuccessful = false;
  std::vector<p2t::Point *> polyDomain;
  mResult = ReosMeshFrameData();
  polyDomain.resize( mDomain.count() );
  try
  {
    QHash<p2t::Point *, int> mapPoly2TriPointToVertex;

    mResult.vertexCoordinates.resize( mDomain.count() * 3 );

    for ( int i = 0; i < mDomain.count(); ++i )
    {
      const QPointF &pt = mDomain.at( i );
      polyDomain[i] = new p2t::Point( pt.x(), pt.y() );
      mapPoly2TriPointToVertex.insert( polyDomain[i], i );

      mResult.vertexCoordinates[i * 3] = pt.x();
      mResult.vertexCoordinates[i * 3 + 1] = pt.y();
      mResult.vertexCoordinates[i * 3 + 2] = 0;
    }
    std::unique_ptr<p2t::CDT> cdt( new p2t::CDT( polyDomain ) );
    cdt->Triangulate();

    const std::vector<p2t::Triangle *> &triangles = cdt->GetTriangles();

    if ( triangles.size() > __INT32_MAX__ )
      throw std::exception();

    int triangleCount = triangles.size();

    mResult.facesIndexes.fill( QVector<int>( 3 ), triangleCount );

    for ( int t = 0; t < triangleCount; ++t )
    {
      p2t::Triangle *triangle = triangles.at( t );

      QVector<int> &reosTriangle = mResult.facesIndexes[t];

      for ( int s = 0; s < 3; ++s )
      {
        reosTriangle[s] = mapPoly2TriPointToVertex.value( triangle->GetPoint( s ), -1 );
        if ( reosTriangle[s] == -1 )
          throw std::exception();
      }
    }

    qDeleteAll( polyDomain );
    mIsSuccessful = true;

  }
  catch ( ... )
  {
    emit sendInformation( "Mesh generator poly2tri: Unable to triangulate" );
    qDeleteAll( polyDomain );
    mIsSuccessful = false;
    mResult = ReosMeshFrameData();
  }
}
