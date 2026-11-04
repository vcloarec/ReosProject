/***************************************************************************
  reosselafin.h - ReosSelafin

 ---------------------
 begin                : 5.10.2026
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

#include "reosselafin.h"
#include <QPointF>
#include <QFile>
#include <QDataStream>
#include <QRectF>

#include <algorithm>
#include <limits>
#include <memory>

#include "reosmesh.h"
#include "reosmeshgenerator.h"

template<typename T> void writeValue( T &value, QDataStream &out, bool changeEndianness = false )
{
  T v = value;
  char *const p = reinterpret_cast<char *>( &v );

  if ( changeEndianness )
    std::reverse( p, p + sizeof( T ) );

  out.writeRawData( p, sizeof( T ) );
}

static bool isNativeLittleEndian()
{
  int n = 1;
  return ( *( char * ) &n == 1 );
}

template<typename T> static void writeValue( QDataStream &stream, T value )
{
  writeValue( value, stream, isNativeLittleEndian() );
}

static void writeInt( QDataStream &stream, int i )
{
  writeValue( i, stream, isNativeLittleEndian() );
}

template<typename T> static void writeValueArrayRecord( QDataStream &stream, const QVector<T> &array )
{
  writeValue( stream, int( array.size() * sizeof( T ) ) );
  for ( const T value : array )
    writeValue( stream, value );
  writeValue( stream, int( array.size() * sizeof( T ) ) );
}

static void writeStringRecord( QDataStream &stream, const QString &str )
{
  writeInt( stream, str.length() );
  stream.writeRawData( str.toStdString().c_str(), str.length() );
  writeInt( stream, str.length() );
}

static bool readInt( QDataStream &stream, int &value )
{
  char *const p = reinterpret_cast<char *>( &value );
  if ( stream.readRawData( p, sizeof( int ) ) != sizeof( int ) )
    return false;
  if ( isNativeLittleEndian() )
    std::reverse( p, p + sizeof( int ) );
  return true;
}

static bool readRawRecord( QDataStream &stream, QByteArray &data )
{
  int size = 0;
  if ( !readInt( stream, size ) || size < 0 )
    return false;
  data.resize( size );
  if ( stream.readRawData( data.data(), size ) != size )
    return false;
  int sizeEnd = 0;
  return readInt( stream, sizeEnd ) && sizeEnd == size;
}

template<typename T> static QVector<T> valuesFromRawData( const QByteArray &data )
{
  QVector<T> ret( data.size() / int( sizeof( T ) ) );
  for ( int i = 0; i < ret.size(); ++i )
  {
    T v;
    char *const p = reinterpret_cast<char *>( &v );
    std::copy( data.constData() + i * sizeof( T ), data.constData() + ( i + 1 ) * sizeof( T ), p );
    if ( isNativeLittleEndian() )
      std::reverse( p, p + sizeof( T ) );
    ret[i] = v;
  }
  return ret;
}

template<typename T> static bool readValueArrayRecord( QDataStream &stream, QVector<T> &array )
{
  QByteArray data;
  if ( !readRawRecord( stream, data ) || data.size() % sizeof( T ) != 0 )
    return false;
  array = valuesFromRawData<T>( data );
  return true;
}

static void setCounterClockwise( QVector<int> &triangle, const QPointF &v0, const QPointF &v1, const QPointF &v2 )
{
  //To have consistent clock wise orientation of triangles which is necessary for 3D rendering
  //Check the clock wise, and if it is not counter clock wise, swap indexes to make the oientation counter clock wise
  double ux = v1.x() - v0.x();
  double uy = v1.y() - v0.y();
  double vx = v2.x() - v0.x();
  double vy = v2.y() - v0.y();

  double crossProduct = ux * vy - uy * vx;
  if ( crossProduct < 0 ) //CW -->change the orientation
  {
    std::swap( triangle[1], triangle[2] );
  }
}


ReosSelafin::ReosSelafin( const QString &filePath )
  : mFilePath( filePath )
{}

bool ReosSelafin::createMeshFrame( const ReosMesh *mesh, const QList<int> verticesPosInBoundary ) const
{
  QFile file( mFilePath );
  if ( !file.open( QIODevice::WriteOnly ) )
    return false;
  QDataStream stream( &file );

  QString header( "Selafin file created by Lekan" );
  int remainingSpace = 72 - header.size();
  QString remainingString;
  remainingString.fill( ' ', remainingSpace );
  header.append( remainingString );
  header.append( "SERAFIND" );
  Q_ASSERT( header.size() == 80 );
  writeStringRecord( stream, header );

  // NBV(1) NBV(2) size
  QVector<int> nbvSize( 2 );
  nbvSize[0] = 0;
  nbvSize[1] = 0;
  writeValueArrayRecord( stream, nbvSize );

  //don't write variable name

  //parameter table, all values are 0
  QVector<int> param( 10, 0 );
  writeValueArrayRecord( stream, param );

  //NELEM,NPOIN,NDP,1
  int verticesPerFace = 3;
  int verticesCount = mesh->vertexCount();
  int facesCount = mesh->faceCount();
  QVector<int> elem( 4 );
  elem[0] = facesCount;
  elem[1] = verticesCount;
  elem[2] = verticesPerFace;
  elem[3] = 1;
  writeValueArrayRecord( stream, elem );

  //connectivity table
  writeInt( stream, facesCount * verticesPerFace * 4 );

  for ( int i = 0; i < facesCount; ++i )
  {
    QVector<int> face = mesh->face( i );
    const QPointF vp0 = mesh->vertexPosition( face.at( 0 ) );
    const QPointF vp1 = mesh->vertexPosition( face.at( 1 ) );
    const QPointF vp2 = mesh->vertexPosition( face.at( 2 ) );
    setCounterClockwise( face, vp0, vp1, vp2 );
    for ( int f : std::as_const( face ) )
      writeInt( stream, f + 1 );
  }
  writeInt( stream, facesCount * verticesPerFace * 4 );

  writeValueArrayRecord( stream, verticesPosInBoundary );

  //Vertices
  QVector<double> xValues( verticesCount );
  QVector<double> yValues( verticesCount );
  for ( int i = 0; i < verticesCount; ++i )
  {
    const QPointF vert = mesh->vertexPosition( i );
    xValues[i] = vert.x();
    yValues[i] = vert.y();
  }

  writeValueArrayRecord( stream, xValues );
  writeValueArrayRecord( stream, yValues );

  file.close();

  return true;
}

ReosMesh *ReosSelafin::loadMeshFrame( QList<int> &verticesPosInBoundary ) const
{
  verticesPosInBoundary.clear();

  QFile file( mFilePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return nullptr;
  QDataStream stream( &file );

  //title
  QByteArray title;
  if ( !readRawRecord( stream, title ) )
    return nullptr;

  // NBV(1) NBV(2)
  QVector<int> nbv;
  if ( !readValueArrayRecord( stream, nbv ) || nbv.size() < 2 )
    return nullptr;

  //variable names, not used
  QByteArray dummy;
  for ( int i = 0; i < nbv.at( 0 ) + nbv.at( 1 ); ++i )
    if ( !readRawRecord( stream, dummy ) )
      return nullptr;

  //parameter table
  QVector<int> param;
  if ( !readValueArrayRecord( stream, param ) || param.size() < 10 )
    return nullptr;
  const double xOrigin = param.at( 2 );
  const double yOrigin = param.at( 3 );

  //date record
  if ( param.at( 9 ) == 1 )
    if ( !readRawRecord( stream, dummy ) )
      return nullptr;

  //NELEM,NPOIN,NDP,1
  QVector<int> elem;
  if ( !readValueArrayRecord( stream, elem ) || elem.size() < 3 )
    return nullptr;
  const int facesCount = elem.at( 0 );
  const int verticesCount = elem.at( 1 );
  const int verticesPerFace = elem.at( 2 );
  if ( facesCount < 0 || verticesCount < 0 || verticesPerFace <= 0 )
    return nullptr;

  //connectivity table
  QVector<int> connectivity;
  if ( !readValueArrayRecord( stream, connectivity ) || connectivity.size() != facesCount * verticesPerFace )
    return nullptr;

  //boundary positions
  QVector<int> ipobo;
  if ( !readValueArrayRecord( stream, ipobo ) || ipobo.size() != verticesCount )
    return nullptr;

  //vertices coordinates, single or double precision
  QByteArray rawX;
  QByteArray rawY;
  if ( !readRawRecord( stream, rawX ) || !readRawRecord( stream, rawY ) )
    return nullptr;

  file.close();

  QVector<double> xValues;
  QVector<double> yValues;
  if ( verticesCount > 0 && rawX.size() == verticesCount * int( sizeof( double ) ) )
  {
    xValues = valuesFromRawData<double>( rawX );
    yValues = valuesFromRawData<double>( rawY );
  }
  else if ( verticesCount == 0 || rawX.size() == verticesCount * int( sizeof( float ) ) )
  {
    const QVector<float> xf = valuesFromRawData<float>( rawX );
    const QVector<float> yf = valuesFromRawData<float>( rawY );
    xValues = QVector<double>( xf.begin(), xf.end() );
    yValues = QVector<double>( yf.begin(), yf.end() );
  }

  if ( xValues.size() != verticesCount || yValues.size() != verticesCount )
    return nullptr;

  ReosMeshFrameData data;
  data.hasZ = false;
  data.vertexCoordinates.resize( verticesCount * 3 );
  double xMin = std::numeric_limits<double>::max();
  double xMax = -std::numeric_limits<double>::max();
  double yMin = std::numeric_limits<double>::max();
  double yMax = -std::numeric_limits<double>::max();
  for ( int i = 0; i < verticesCount; ++i )
  {
    const double x = xValues.at( i ) + xOrigin;
    const double y = yValues.at( i ) + yOrigin;
    data.vertexCoordinates[i * 3] = x;
    data.vertexCoordinates[i * 3 + 1] = y;
    data.vertexCoordinates[i * 3 + 2] = std::numeric_limits<double>::quiet_NaN();
    xMin = std::min( xMin, x );
    xMax = std::max( xMax, x );
    yMin = std::min( yMin, y );
    yMax = std::max( yMax, y );
  }
  if ( verticesCount > 0 )
    data.extent = QRectF( QPointF( xMin, yMin ), QPointF( xMax, yMax ) );

  data.facesIndexes.resize( facesCount );
  for ( int i = 0; i < facesCount; ++i )
  {
    QVector<int> &face = data.facesIndexes[i];
    face.resize( verticesPerFace );
    for ( int j = 0; j < verticesPerFace; ++j )
    {
      const int index = connectivity.at( i * verticesPerFace + j ) - 1;
      if ( index < 0 || index >= verticesCount )
        return nullptr;
      face[j] = index;
    }
  }

  verticesPosInBoundary = QList<int>( ipobo.begin(), ipobo.end() );

  std::unique_ptr<ReosMesh> mesh( ReosMesh::createMeshFrame() );
  mesh->generateMesh( data );

  return mesh.release();
}
