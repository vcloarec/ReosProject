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


bool ReosSelafin::writeMeshFrame( const QString &filePath, const ReosMesh *mesh, const QList<int> verticalPosInBoundary, bool writeZvalues )
{
  QMap<QString, QVector<double> > variables;

  if ( writeZvalues )
  {
    QVector<double> bottomZValues( mesh->vertexCount() );
    for ( int i = 0; i < mesh->vertexCount(); ++i )
      bottomZValues[i] = mesh->vertexElevation( i );
    variables["BOTTOM"] = bottomZValues;
  }

  return writeStaticMesh( filePath, mesh, verticalPosInBoundary, variables );
}

ReosMeshFrameData ReosSelafin::loadMeshFrame( const QString &filePath, QList<int> &verticesPosInBoundary, bool &ok )
{
  verticesPosInBoundary.clear();
  ok = false;
  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return ReosMeshFrameData();
  QDataStream stream( &file );

  //title
  QByteArray title;
  if ( !readRawRecord( stream, title ) )
    return ReosMeshFrameData();

  // NBV(1) NBV(2)
  QVector<int> nbv;
  if ( !readValueArrayRecord( stream, nbv ) || nbv.size() < 2 )
    return ReosMeshFrameData();

  //variable names, not used
  QByteArray variablesArray;
  for ( int i = 0; i < nbv.at( 0 ) + nbv.at( 1 ); ++i )
    if ( !readRawRecord( stream, variablesArray ) )
      return ReosMeshFrameData();
  QList<QString> variableNames;
  for ( int i = 0; i < variablesArray.size() / 32; ++i )
  {
    QByteArray varName = variablesArray.mid( i * 32, 32 );
    variableNames.append( QString::fromUtf8( varName ).trimmed() );
  }

  //parameter table
  QVector<int> param;
  if ( !readValueArrayRecord( stream, param ) || param.size() < 10 )
    return ReosMeshFrameData();
  const double xOrigin = param.at( 2 );
  const double yOrigin = param.at( 3 );

  //date record
  QByteArray refTime;
  if ( param.at( 9 ) == 1 )
    if ( !readRawRecord( stream, refTime ) )
      return ReosMeshFrameData();

  //NELEM,NPOIN,NDP,1
  QVector<int> elem;
  if ( !readValueArrayRecord( stream, elem ) || elem.size() < 3 )
    return ReosMeshFrameData();
  const int facesCount = elem.at( 0 );
  const int verticesCount = elem.at( 1 );
  const int verticesPerFace = elem.at( 2 );
  if ( facesCount < 0 || verticesCount < 0 || verticesPerFace <= 0 )
    return ReosMeshFrameData();

  //connectivity table
  QVector<int> connectivity;
  if ( !readValueArrayRecord( stream, connectivity ) || connectivity.size() != facesCount * verticesPerFace )
    return ReosMeshFrameData();

  //boundary positions
  QVector<int> ipobo;
  if ( !readValueArrayRecord( stream, ipobo ) || ipobo.size() != verticesCount )
    return ReosMeshFrameData();

  //vertices coordinates, single or double precision
  QByteArray rawX;
  QByteArray rawY;
  if ( !readRawRecord( stream, rawX ) || !readRawRecord( stream, rawY ) )
    return ReosMeshFrameData();

  QByteArray rawZ;
  for ( int i = 0; i < variableNames.size(); ++i )
  {
    QString varName = variableNames.at( i );
    if ( varName.contains( QStringLiteral( "BOTTOM" ) ) )
    {
      QByteArray rawTime;
      readRawRecord( stream, rawTime );
      readRawRecord( stream, rawZ );
      break;
    }
  }

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
    return ReosMeshFrameData();

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

  if ( !rawZ.isEmpty() )
  {
    QVector<double> zValues;
    if ( rawZ.size() == verticesCount * int( sizeof( double ) ) )
      zValues = valuesFromRawData<double>( rawZ );
    else if ( rawZ.size() == verticesCount * int( sizeof( float ) ) )
    {
      const QVector<float> zf = valuesFromRawData<float>( rawZ );
      zValues = QVector<double>( zf.begin(), zf.end() );
    }

    if ( zValues.size() == verticesCount )
    {
      data.hasZ = true;
      for ( int i = 0; i < verticesCount; ++i )
        data.vertexCoordinates[i * 3 + 2] = zValues.at( i );
    }
  }

  data.facesIndexes.resize( facesCount );
  for ( int i = 0; i < facesCount; ++i )
  {
    QVector<int> &face = data.facesIndexes[i];
    face.resize( verticesPerFace );
    for ( int j = 0; j < verticesPerFace; ++j )
    {
      const int index = connectivity.at( i * verticesPerFace + j ) - 1;
      if ( index < 0 || index >= verticesCount )
        return ReosMeshFrameData();
      face[j] = index;
    }
  }

  verticesPosInBoundary = QList<int>( ipobo.begin(), ipobo.end() );
  ok = true;
  return data;
}


bool ReosSelafin::writeStaticMesh( const QString &filePath, const ReosMesh *mesh, const QVector<int> &verticesPosInBoundary, QMap<QString, QVector<double> > variables )
{
  /**
   * Serafin format (also called Selafin)
   *
   * Binary format for triangular mesh with datasets defined on vertices
   * Source of this doc come from :
   * http://www.opentelemac.org/downloads/MANUALS/TELEMAC-2D/telemac-2d_user_manual_en_v7p0.pdf Appendix 3
   * https://www.gdal.org/drv_selafin.html
   *
   * The Selafin file records are listed below:
   * - 1 record containing the title of the study (72 characters) and a 8 characters string indicating the type
   *    of format (SERAFIN or SERAFIND)
   * - record containing the two integers NBV(1)and NBV(2)(number of linear and quadratic variables, NBV(2)with the value of 0 for Telemac,
   * cas quadratic values are not saved so far),
   * - NBV(1)records containing the names and units of each variable (over 32 characters),
   * - 1 record containing the integers table IPARAM(10 integers, of which only the 6are currently being used),
   *        - if IPARAM (3)!=0: the value corresponds to the x-coordinate of the origin of the mesh,
   *        - if IPARAM (4)!=0: the value corresponds to the y-coordinate of the origin of the mesh,
   *        - if IPARAM (7): the value corresponds to the number of  planes on the vertical (3D computation),
   *        - if IPARAM (8)!=0: the value corresponds to the number of boundary points (in parallel),
   *        - if IPARAM (9)!=0: the value corresponds to the number of interface points (in parallel),
   *        - if IPARAM(8 ) or IPARAM(9) !=0: the array IPOBO below is replaced by the array KNOLG(total initial number of points).
   *            All the other numbers are local to the sub-domain, including IKLE.
   *
   * - if IPARAM(10)= 1: a record containing the computation starting date,
   * - 1 record containing the integers NELEM,NPOIN,NDP,1(number of elements, number of points, number of points per element and the value 1),
   * - 1 record containing table IKLE(integer array of dimension (NDP,NELEM) which is the connectivity table.
   *    N.B.: in TELEMAC-2D, the dimensions of this array are (NELEM,NDP)),
   * - 1 record containing table IPOBO(integer array of dimension NPOIN);
   *    the value of one element is 0 for an internal point, and gives the numbering of boundary points for the others,
   * - 1 record containing table X(real array of dimension NPOINcontaining the abscissae of the points),
   * - 1 record containing table Y(real array of dimension NPOINcontaining the ordinates of the points),
   *
   * Next, for each time step, the following are found:
   * - 1 record containing time T(real),
   * - NBV(1)+NBV(2)records containing the results tables for each variable at time T.
   */

  QFile file( filePath );
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
  nbvSize[0] = variables.size();
  nbvSize[1] = 0;
  writeValueArrayRecord( stream, nbvSize );

  // write pre-existing dataset name
  const QList<QString> variableNames = variables.keys();
  for ( size_t i = 0; i < variableNames.count(); ++i )
  {
    QString variableName = variableNames.at( i );
    variableName.resize( 32, ' ' );
    writeStringRecord( stream, variableName );
  }

  //parameter table
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

  for ( auto it = variables.constBegin(); it != variables.constEnd(); ++it )
  {
    const QVector<double> &values = it.value();
    if ( values.size() != verticesCount )
      return false;
    writeValueArrayRecord( stream, QVector<double>( { 0.0 } ) ); // write time record, here 0.0 for static mesh
    writeValueArrayRecord( stream, values );
  }

  file.close();

  return true;
}
