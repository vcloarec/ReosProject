/***************************************************************************
  reosmesh.cpp - ReosMesh

 ---------------------
 begin                : 13.1.2022
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
#include "reosmesh.h"
#include "reosmesh_p.h"

#include "reosparameter.h"
#include "reosmeshdatasetsource.h"

ReosMesh *ReosMesh::createMeshFrame( const QString &crs, QObject *parent )
{
  return new ReosMeshFrame_p( crs, parent );
}

ReosMesh *ReosMesh::createMeshFrameFromFile( const QString &dataPath, const QString &destinationCrs )
{
  return new ReosMeshFrame_p( dataPath, destinationCrs );
}


ReosMesh::ReosMesh( QObject *parent )
  : ReosRenderedObject( parent )
{
  mQualityMeshParameters.minimumAngle = new ReosParameterDouble( tr( "Minimum angle" ), false, this );
  mQualityMeshParameters.minimumAngle->setValue( 5 );
  mQualityMeshParameters.maximumAngle = new ReosParameterDouble( tr( "Maximum angle" ), false, this );
  mQualityMeshParameters.maximumAngle->setValue( 150 );
  mQualityMeshParameters.connectionCount = new ReosParameterInteger( tr( "Maximum count" ), false, this );
  mQualityMeshParameters.connectionCount->setValue( 8 );
  mQualityMeshParameters.connectionCountBoundary = new ReosParameterInteger( tr( "Maximum count from boundary" ), false, this );
  mQualityMeshParameters.connectionCountBoundary->setValue( 4 );
  mQualityMeshParameters.maximumSlope = new ReosParameterSlope( tr( "Maximum slope" ), false, this );
  mQualityMeshParameters.maximumSlope->setValue( 0.1 );
  mQualityMeshParameters.minimumArea = new ReosParameterArea( tr( "Minimum area" ), false, this );
  mQualityMeshParameters.minimumArea->setValue( ReosArea( 0.1, ReosArea::m2 ) );
  mQualityMeshParameters.maximumArea = new ReosParameterArea( tr( "Maximum area" ), false, this );
  mQualityMeshParameters.maximumArea->setValue( ReosArea( 100000, ReosArea::m2 ) );
  mQualityMeshParameters.maximumAreaChange = new ReosParameterDouble( tr( "Maximum area change" ), false, this );
  mQualityMeshParameters.maximumAreaChange->setValue( 2 );
}

QMap<QString, QByteArray> ReosMesh::datasetVectorSymbologies() const
{
  return mDatasetVectorSymbologies;
}

void ReosMesh::setDatasetVectorSymbologies( const QMap<QString, QByteArray> &datasetVectorSymbologies )
{
  mDatasetVectorSymbologies = datasetVectorSymbologies;
}

QMap<QString, QByteArray> ReosMesh::datasetScalarSymbologies() const
{
  return mDatasetScalarSymbologies;
}

void ReosMesh::setDatasetScalarSymbologies( const QMap<QString, QByteArray> &datasetScalarSymbologies )
{
  mDatasetScalarSymbologies = datasetScalarSymbologies;
}


double ReosMesh::verticaleSCale() const
{
  return mVerticaleSCale;
}

void ReosMesh::setVerticaleSCale( double verticaleSCale )
{
  mVerticaleSCale = verticaleSCale;
}

ReosMesh::QualityMeshParameters ReosMesh::qualityMeshParameters() const
{
  return mQualityMeshParameters;
}

void ReosMesh::setQualityMeshParameter( const ReosEncodedElement &element )
{
  mQualityMeshParameters.decode( element, this );
}

void ReosMesh::setBoundariesVertices( const QVector<QVector<int>> &vertices )
{
  mBoundaryVerticesSet.clear();
  for ( const QVector<int> &boundLine : vertices )
    for ( int i : boundLine )
      mBoundaryVerticesSet.insert( i );
}

void ReosMesh::setHolesVertices( const QVector<QVector<QVector<int>> > &vertices )
{
  mHolesVerticesSet.clear();
  for ( const QVector<QVector<int>> &hole : vertices )
    for ( const QVector<int> &holeLine : hole )
      for ( int i : holeLine )
        mHolesVerticesSet.insert( i );
}

bool ReosMesh::vertexIsOnBoundary( int vertexIndex ) const
{
  return mBoundaryVerticesSet.contains( vertexIndex );
}

bool ReosMesh::vertexIsOnHoleBorder( int vertexIndex ) const
{
  return mHolesVerticesSet.contains( vertexIndex );
}

ReosEncodedElement ReosMesh::QualityMeshParameters::encode() const
{
  ReosEncodedElement encodedElement( QStringLiteral( "qualtiy-mesh-parameters" ) );

  encodedElement.addEncodedData( QStringLiteral( "minimum-angle" ), minimumAngle->encode() );
  encodedElement.addEncodedData( QStringLiteral( "maximum-angle" ), maximumAngle->encode() );
  encodedElement.addEncodedData( QStringLiteral( "connection-count" ), connectionCount->encode() );
  encodedElement.addEncodedData( QStringLiteral( "connection-boundary-count" ), connectionCountBoundary->encode() );
  encodedElement.addEncodedData( QStringLiteral( "maximum-slope" ), maximumSlope->encode() );
  encodedElement.addEncodedData( QStringLiteral( "minimum-area" ), minimumArea->encode() );
  encodedElement.addEncodedData( QStringLiteral( "maximum-area" ), maximumArea->encode() );
  encodedElement.addEncodedData( QStringLiteral( "maximum-area-change" ), maximumAreaChange->encode() );

  return encodedElement;
}

void ReosMesh::QualityMeshParameters::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.hasEncodedData( QStringLiteral( "minimum-angle" ) ) )
  {
    minimumAngle->deleteLater();
    minimumAngle = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "minimum-angle" ) ), false, tr( "Minimum angle" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-angle" ) ) )
  {
    maximumAngle->deleteLater();
    maximumAngle = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "maximum-angle" ) ), false, tr( "Maximum angle" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "connection-count" ) ) )
  {
    connectionCount->deleteLater();
    connectionCount = ReosParameterInteger::decode( element.getEncodedData( QStringLiteral( "connection-count" ) ), false, tr( "Maximum count" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "connection-boundary-count" ) ) )
  {
    connectionCountBoundary->deleteLater();
    connectionCountBoundary = ReosParameterInteger::decode( element.getEncodedData( QStringLiteral( "connection-boundary-count" ) ), false, tr( "Maximum count from boundary" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-slope" ) ) )
  {
    maximumSlope->deleteLater();
    maximumSlope = ReosParameterSlope::decode( element.getEncodedData( QStringLiteral( "maximum-slope" ) ), false, tr( "Maximum slope" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "minimum-area" ) ) )
  {
    minimumArea->deleteLater();
    minimumArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "minimum-area" ) ), false, tr( "Minimum area" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-area" ) ) )
  {
    maximumArea->deleteLater();
    maximumArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "maximum-area" ) ), false, tr( "Maximum area" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-area-change" ) ) )
  {
    maximumAreaChange->deleteLater();
    maximumAreaChange = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "maximum-area-change" ) ), false, tr( "Maximum area change" ), parent );
  }
}

ReosMeshPointValue::ReosMeshPointValue()
{}

ReosMeshPointValue::ReosMeshPointValue( ReosMeshPointValue_p *poinValue )
  : d( poinValue )
{
  d->ref++;
}

ReosMeshPointValue::~ReosMeshPointValue()
{
  d->ref--;
  if ( d->ref == 0 )
    delete d;
}

ReosMeshPointValue::ReosMeshPointValue( const ReosMeshPointValue &other )
{
  d = other.d;
  d->ref++;
}

ReosMeshPointValue &ReosMeshPointValue::operator=( const ReosMeshPointValue &other )
{
  if ( this != &other )
  {
    if ( d )
    {
      d->ref--;
      if ( d->ref == 0 )
        delete d;
    }
    d = other.d;
    d->ref++;
  }
  return *this;
}

ReosMeshPointValue::ReosMeshPointValue( ReosMeshPointValue &&other )
{
  d = other.d;
  other.d = nullptr;
}

ReosMeshPointValue &ReosMeshPointValue::operator=( ReosMeshPointValue &&other )
{
  if ( this != &other )
  {
    if ( d )
    {
      d->ref--;
      if ( d->ref == 0 )
        delete d;
    }
    d = other.d;
    other.d = nullptr;
  }
  return *this;
}

double ReosMeshPointValue::value( ReosMeshDatasetSource *source, int groupIndex, int index ) const
{
  if ( d )
    return d->interpolateValue( source->datasetValues( groupIndex, index ) );

  return std::numeric_limits<double>::quiet_NaN();
}

double ReosMeshPointValue::terrainElevation( ReosMesh *mesh ) const
{
  if ( d )
    return d->interpolateTerrainElevation( mesh );

  return std::numeric_limits<double>::quiet_NaN();
}

ReosMeshPointValue_p::ReosMeshPointValue_p( const QPointF &point )
  : mPoint( point )
{}

ReosMeshPointValue_p::~ReosMeshPointValue_p() {}

ReosMeshPointValueOnVertex::ReosMeshPointValueOnVertex( int vertexIndex, const QPointF &point )
  : ReosMeshPointValue_p( point )
  , mVertexIndex( vertexIndex )
{}

int ReosMeshPointValueOnVertex::vertex() const
{
  return mVertexIndex;
}

double ReosMeshPointValueOnVertex::interpolateValue( const QVector<double> &values ) const
{
  return values.at( mVertexIndex );
}

double ReosMeshPointValueOnVertex::interpolateTerrainElevation( ReosMesh *mesh ) const
{
  return mesh->vertexElevation( mVertexIndex );
}

ReosMeshPointValueOnEdge::ReosMeshPointValueOnEdge( int vertexIndex1, int vertexIndex2, double posInEdge, const QPointF &point )
  : ReosMeshPointValue_p( point )
  , mVertex1( vertexIndex1 )
  , mVertex2( vertexIndex2 )
  , mPosInEdge( posInEdge )
{
}

int ReosMeshPointValueOnEdge::vertex1() const
{
  return mVertex1;
}

int ReosMeshPointValueOnEdge::vertex2() const
{
  return mVertex2;
}

double ReosMeshPointValueOnEdge::interpolateValue( const QVector<double> &values ) const
{
  double value1 = values.at( mVertex1 );
  double value2 = values.at( mVertex2 );

  return interpolateValueOnEdge( value1, value2 );
}

double ReosMeshPointValueOnEdge::interpolateTerrainElevation( ReosMesh *mesh ) const
{
  double z1 = mesh->vertexElevation( mVertex1 );
  double z2 = mesh->vertexElevation( mVertex2 );

  return interpolateValueOnEdge( z1, z2 );
}

double ReosMeshPointValueOnEdge::interpolateValueOnEdge( double value1, double value2 ) const
{
  return value1 + ( value2 - value1 ) * mPosInEdge;
}

ReosMeshPointValueOnFace::ReosMeshPointValueOnFace( int vertexIndex1, int vertexIndex2, int vertexIndex3, double lam1, double lam2, double lam3, const QPointF &point )
  : ReosMeshPointValue_p( point )
  , mVertex1( vertexIndex1 )
  , mVertex2( vertexIndex2 )
  , mVertex3( vertexIndex3 )
  , mLam1( lam1 )
  , mLam2( lam2 )
  , mLam3( lam3 )
{}

double ReosMeshPointValueOnFace::interpolateValue( const QVector<double> &values ) const
{
  double value1 = values.at( mVertex1 );
  double value2 = values.at( mVertex2 );
  double value3 = values.at( mVertex3 );

  return interpolateValueOnFace( value1, value2, value3 );
}

double ReosMeshPointValueOnFace::interpolateTerrainElevation( ReosMesh *mesh ) const
{
  double value1 = mesh->vertexElevation( mVertex1 );
  double value2 = mesh->vertexElevation( mVertex2 );
  double value3 = mesh->vertexElevation( mVertex3 );

  return interpolateValueOnFace( value1, value2, value3 );
}

double ReosMeshPointValueOnFace::interpolateValueOnFace( double v1, double v2, double v3 ) const
{
  return mLam3 * v1 + mLam2 * v2 + mLam1 * v3;
}
