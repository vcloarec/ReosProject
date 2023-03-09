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

ReosMesh *ReosMesh::createMeshFrameFromFile( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message )
{
  return new ReosMeshFrame_p( dataPath, destinationCrs, message );
}


ReosMesh::ReosMesh( QObject *parent )
  : ReosRenderedObject( parent )
{
  mQualityMeshParameters.minimumAngle = new ReosParameterDouble( tr( "Min. angle" ), false, this );
  mQualityMeshParameters.minimumAngle->setValue( 5 );
  mQualityMeshParameters.maximumAngle = new ReosParameterDouble( tr( "Max. angle" ), false, this );
  mQualityMeshParameters.maximumAngle->setValue( 150 );
  mQualityMeshParameters.connectionCount = new ReosParameterInteger( tr( "Max. count" ), false, this );
  mQualityMeshParameters.connectionCount->setValue( 8 );
  mQualityMeshParameters.connectionCountBoundary = new ReosParameterInteger( tr( "Max.count boundary" ), false, this );
  mQualityMeshParameters.connectionCountBoundary->setValue( 4 );
  mQualityMeshParameters.maximumSlope = new ReosParameterSlope( tr( "Max. slope" ), false, this );
  mQualityMeshParameters.maximumSlope->setValue( 0.1 );
  mQualityMeshParameters.minimumArea = new ReosParameterArea( tr( "Min. area" ), false, this );
  mQualityMeshParameters.minimumArea->setValue( ReosArea( 0.1, ReosArea::m2 ) );
  mQualityMeshParameters.maximumArea = new ReosParameterArea( tr( "Max. area" ), false, this );
  mQualityMeshParameters.maximumArea->setValue( ReosArea( 100000, ReosArea::m2 ) );
  mQualityMeshParameters.maximumAreaChange = new ReosParameterDouble( tr( "Max. area change" ), false, this );
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
    minimumAngle = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "minimum-angle" ) ), false, tr( "Min. angle" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-angle" ) ) )
  {
    maximumAngle->deleteLater();
    maximumAngle = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "maximum-angle" ) ), false, tr( "Max. angle" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "connection-count" ) ) )
  {
    connectionCount->deleteLater();
    connectionCount = ReosParameterInteger::decode( element.getEncodedData( QStringLiteral( "connection-count" ) ), false, tr( "Max. count" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "connection-boundary-count" ) ) )
  {
    connectionCountBoundary->deleteLater();
    connectionCountBoundary = ReosParameterInteger::decode( element.getEncodedData( QStringLiteral( "connection-boundary-count" ) ), false, tr( "Max. count boundary" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-slope" ) ) )
  {
    maximumSlope->deleteLater();
    maximumSlope = ReosParameterSlope::decode( element.getEncodedData( QStringLiteral( "maximum-slope" ) ), false, tr( "Max. slope" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "minimum-area" ) ) )
  {
    minimumArea->deleteLater();
    minimumArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "minimum-area" ) ), false, tr( "Min. area" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-area" ) ) )
  {
    maximumArea->deleteLater();
    maximumArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "maximum-area" ) ), false, tr( "Max. area" ), parent );
  }

  if ( element.hasEncodedData( QStringLiteral( "maximum-area-change" ) ) )
  {
    maximumAreaChange->deleteLater();
    maximumAreaChange = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "maximum-area-change" ) ), false, tr( "Max. area change" ), parent );
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
  ReosMeshDatasetSource::Location location = source->groupLocation( groupIndex );
  if ( d )
  {
    if ( source->groupIsScalar( groupIndex ) )
      return d->interpolateValue( source->datasetValues( groupIndex, index ),  location );
    else
      return d->interpolateVectorValue( source->datasetValues( groupIndex, index ),  location );
  }

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

ReosMeshPointValueOnVertex::ReosMeshPointValueOnVertex( int vertexIndex, int faceindex, const QPointF &point )
  : ReosMeshPointValue_p( point )
  , mVertexIndex( vertexIndex )
  , mFaceIndex( faceindex )
{}

int ReosMeshPointValueOnVertex::vertex() const
{
  return mVertexIndex;
}

double ReosMeshPointValueOnVertex::interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const
{
  switch ( location )
  {
    case ReosMeshDatasetSource::Location::Vertex:
      return values.at( mVertexIndex );
      break;
    case ReosMeshDatasetSource::Location::Face:
      return values.at( mFaceIndex );
      break;
  }

  return std::numeric_limits<double>::quiet_NaN();
}

double ReosMeshPointValueOnVertex::interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const
{
  switch ( location )
  {
    case ReosMeshDatasetSource::Location::Vertex:
      return sqrt( pow( values.at( mVertexIndex * 2 ), 2 ) + pow( values.at( mVertexIndex * 2 + 1 ), 2 ) );
      break;
    case ReosMeshDatasetSource::Location::Face:
      return sqrt( pow( values.at( mFaceIndex * 2 ), 2 ) + pow( values.at( mFaceIndex * 2 + 1 ), 2 ) );
      break;
  }

  return std::numeric_limits<double>::quiet_NaN();
}

double ReosMeshPointValueOnVertex::interpolateTerrainElevation( ReosMesh *mesh ) const
{
  return mesh->vertexElevation( mVertexIndex );
}

ReosMeshPointValueOnEdge::ReosMeshPointValueOnEdge( int vertexIndex1, int vertexIndex2, int face1, int face2, double posInEdge, const QPointF &point )
  : ReosMeshPointValue_p( point )
  , mVertex1( vertexIndex1 )
  , mVertex2( vertexIndex2 )
  , mFace1( face1 )
  , mFace2( face2 )
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

double ReosMeshPointValueOnEdge::interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const
{
  switch ( location )
  {
    case ReosMeshDatasetSource::Location::Vertex:
    {
      double value1 = values.at( mVertex1 );
      double value2 = values.at( mVertex2 );
      return interpolateValueOnEdge( value1, value2 );
    }
    break;
    case ReosMeshDatasetSource::Location::Face:
    {
      double value1 = mFace1 >= 0 ? values.at( mFace1 ) : 0;
      double value2 =  mFace2 >= 0 ? values.at( mFace2 ) : 0;
      int count = ( mFace1 >= 0 && !std::isnan( value1 )  ? 1 : 0 ) + ( mFace2 >= 0 && !std::isnan( value1 ) ? 1 : 0 );
      if ( std::isnan( value1 ) )
        value1 = 0;
      if ( std::isnan( value2 ) )
        value2 = 0;

      if ( count != 0 )
        return ( value1 + value2 ) / count;
    }
    break;
  }
  return std::numeric_limits<double>::quiet_NaN();
}

double ReosMeshPointValueOnEdge::interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const
{
  switch ( location )
  {
    case ReosMeshDatasetSource::Location::Vertex:
    {
      double value11 = values.at( mVertex1 * 2 );
      double value12 = values.at( mVertex1 * 2 + 1 );
      double value21 = values.at( mVertex2 * 2 );
      double value22 = values.at( mVertex2 * 2 + 1 );

      return interpolateValueOnEdge( sqrt( pow( value11, 2 ) + pow( value12, 2 ) ),
                                     sqrt( pow( value21, 2 ) + pow( value22, 2 ) ) );
    }
    break;
    case ReosMeshDatasetSource::Location::Face:
    {
      double value11 = mFace1 >= 0 ? values.at( mFace1 * 2 ) : 0;
      double value12 = mFace1 >= 0 ? values.at( mFace1 * 2 + 1 ) : 0;
      double value21 = mFace2 >= 0 ? values.at( mFace2 * 2 ) : 0;
      double value22 = mFace2 >= 0 ? values.at( mFace2 * 2 + 1 ) : 0;
      int count = ( mFace1 >= 0 && !std::isnan( value11 ) && !std::isnan( value12 ) ? 1 : 0 ) +
                  ( mFace2 >= 0 && !std::isnan( value21 ) && !std::isnan( value22 ) ? 1 : 0 );

      if ( std::isnan( value11 ) )
        value11 = 0;
      if ( std::isnan( value12 ) )
        value12 = 0;

      if ( std::isnan( value21 ) )
        value21 = 0;
      if ( std::isnan( value22 ) )
        value22 = 0;

      if ( count != 0 )
        return ( sqrt( pow( value11, 2 ) + pow( value12, 2 ) ) +
                 sqrt( pow( value21, 2 ) + pow( value22, 2 ) ) ) / count;
    }
    break;
  }

  return std::numeric_limits<double>::quiet_NaN();
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

ReosMeshPointValueOnFace::ReosMeshPointValueOnFace(
  int vertexIndex1, int vertexIndex2, int vertexIndex3,
  int face,
  double lam1, double lam2, double lam3, const QPointF &point )
  : ReosMeshPointValue_p( point )
  , mVertex1( vertexIndex1 )
  , mVertex2( vertexIndex2 )
  , mVertex3( vertexIndex3 )
  , mFace( face )
  , mLam1( lam1 )
  , mLam2( lam2 )
  , mLam3( lam3 )
{}

double ReosMeshPointValueOnFace::interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const
{
  switch ( location )
  {
    case ReosMeshDatasetSource::Location::Vertex:
    {
      double value1 = values.at( mVertex1 );
      double value2 = values.at( mVertex2 );
      double value3 = values.at( mVertex3 );

      return interpolateValueOnFace( value1, value2, value3 );
    }
    break;
    case ReosMeshDatasetSource::Location::Face:
      return values.at( mFace );
      break;
  }

  return std::numeric_limits<double>::quiet_NaN();

}

double ReosMeshPointValueOnFace::interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const
{
  switch ( location )
  {
    case ReosMeshDatasetSource::Location::Vertex:
    {
      double value11 = values.at( 2 * mVertex1 );
      double value12 = values.at( 2 * mVertex1 + 1 );
      double value21 = values.at( 2 * mVertex2 );
      double value22 = values.at( 2 * mVertex2 + 1 );
      double value31 = values.at( 2 * mVertex3 );
      double value32 = values.at( 2 * mVertex3 + 1 );

      return interpolateValueOnFace( sqrt( pow( value11, 2 ) + pow( value12, 2 ) ),
                                     sqrt( pow( value21, 2 ) + pow( value22, 2 ) ),
                                     sqrt( pow( value31, 2 ) + pow( value32, 2 ) ) );
    }
    break;
    case ReosMeshDatasetSource::Location::Face:
    {
      double valueX = values.at( 2 * mFace );
      double valueY = values.at( 2 * mFace + 1 );
      return sqrt( pow( valueX, 2 ) + pow( valueY, 2 ) );
    }
    break;
  }

  return std::numeric_limits<double>::quiet_NaN();
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

ReosMeshData::ReosMeshData( Data *data )
{
  mData.reset( data );
}

const  void *ReosMeshData::data() const
{
  return mData->data() ;
}
