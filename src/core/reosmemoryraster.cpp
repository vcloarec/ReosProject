/***************************************************************************
                      reosmemoryraster.h
                     --------------------------------------
Date                 : 23-05-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmemoryraster.h"

double ReosRasterExtent::xMapOrigin() const
{
  return mXOrigin;
}

double ReosRasterExtent::yMapOrigin() const
{
  return mYOrigin;
}

double ReosRasterExtent::xCellSize() const
{
  return mXCellSize;
}

double ReosRasterExtent::yCellSize() const
{
  return mYCellSize;
}

int ReosRasterExtent::xCellCount() const
{
  return mXCellCount;
}

int ReosRasterExtent::yCellCount() const
{
  return mYCellCount;
}

ReosRasterExtent ReosRasterExtent::operator*( const ReosRasterExtent &other ) const
{

  ReosMapExtent retMapExtent = ReosMapExtent::operator*( other );

  if ( retMapExtent == ReosMapExtent() )
    return ReosRasterExtent();

  QRectF rect( QPointF( xMapMin(), yMapMin() ), QPointF( xMapMax(), yMapMax() ) );
  QRectF rectOther( QPointF( other.xMapMin(), other.yMapMin() ), QPointF( other.xMapMax(), other.yMapMax() ) );

  if ( retMapExtent == QRectF() )
  {
    return ReosRasterExtent();
  }
  else
  {
    double xMin = int( ( retMapExtent.xMapMin() - xMapMin() ) / abs( mXCellSize ) ) * abs( mXCellSize ) + xMapMin();
    double xMax = xMapMax() - int( ( xMapMax() - retMapExtent.xMapMax() ) / abs( mXCellSize ) ) * abs( mXCellSize );
    double yMin = int( ( retMapExtent.yMapMin() - yMapMin() ) / abs( mYCellSize ) ) * abs( mYCellSize ) + yMapMin();
    double yMax = yMapMax() - int( ( yMapMax() - retMapExtent.yMapMax() ) / abs( mYCellSize ) ) * abs( mYCellSize );

    double xOrigin = ( mXCellSize > 0 ? xMin : xMax );
    double yOrigin = ( mYCellSize > 0 ? yMin : yMax );

    int xCount = int( ( xMax - xMin ) / abs( mXCellSize ) + 0.5 );
    int yCount = int( ( yMax - yMin ) / abs( mYCellSize ) + 0.5 );


    return ReosRasterExtent( xOrigin, yOrigin, xCount, yCount, mXCellSize, mYCellSize );
  }
}

bool ReosRasterExtent::operator==( const ReosRasterExtent &other ) const
{
  if ( ReosMapExtent::operator!=( other ) )
    return false;
  if ( mIsValid != other.mIsValid )
    return false;
  if ( mXOrigin != other.mXOrigin )
    return false;
  if ( mXOrigin != other.mXOrigin )
    return false;
  if ( mYOrigin != other.mYOrigin )
    return false;
  if ( mXCellSize != other.mXCellSize )
    return false;
  if ( mYCellSize != other.mYCellSize )
    return false;
  if ( mXCellCount != other.mXCellCount )
    return false;
  if ( mYCellCount != other.mYCellCount )
    return false;
  return true;
}

bool ReosRasterExtent::operator!=( const ReosRasterExtent &other ) const
{
  return !operator==( other );
}

ReosEncodedElement ReosRasterExtent::encode() const
{
  ReosEncodedElement ret( QStringLiteral( "raster-extent" ) );

  ret.addEncodedData( QStringLiteral( "map-extent" ), ReosMapExtent::encode() );
  ret.addData( QStringLiteral( "valid" ), mIsValid );
  ret.addData( QStringLiteral( "x-origin" ),  mXOrigin );
  ret.addData( QStringLiteral( "y-origin" ), mYOrigin );
  ret.addData( QStringLiteral( "x-cell-size" ), mXCellSize );
  ret.addData( QStringLiteral( "y-cell-size" ), mYCellSize );
  ret.addData( QStringLiteral( "x-cell-count" ), mXCellCount );
  ret.addData( QStringLiteral( "y-cell-count" ), mYCellCount );

  return ret;
}

ReosRasterExtent ReosRasterExtent::decode( const ReosEncodedElement &element )
{
  ReosMapExtent me = ReosMapExtent::decode( element.getEncodedData( QStringLiteral( "map-extent" ) ) );
  ReosRasterExtent ret( me );

  if ( !element.getData( QStringLiteral( "valid" ), ret.mIsValid ) )
    return ReosRasterExtent();

  if ( !element.getData( QStringLiteral( "x-origin" ),  ret.mXOrigin ) )
    return ReosRasterExtent();
  if ( !element.getData( QStringLiteral( "y-origin" ), ret.mYOrigin ) )
    return ReosRasterExtent();
  if ( !element.getData( QStringLiteral( "x-cell-size" ), ret.mXCellSize ) )
    return ReosRasterExtent();
  if ( !element.getData( QStringLiteral( "y-cell-size" ), ret.mYCellSize ) )
    return ReosRasterExtent();
  if ( !element.getData( QStringLiteral( "x-cell-count" ), ret.mXCellCount ) )
    return ReosRasterExtent();
  if ( !element.getData( QStringLiteral( "y-cell-count" ), ret.mYCellCount ) )
    return ReosRasterExtent();

  return ret;
}

//double ReosRasterExtent::xMapMax() const
//{
//  if ( mXCellSize > 0 )
//  {
//    return mXOrigin + mXCellSize * mXCellCount;
//  }
//  else
//    return mXOrigin;
//}

//double ReosRasterExtent::yMapMax() const
//{
//  if ( mYCellSize > 0 )
//  {
//    return mYOrigin + mYCellSize * mYCellCount;
//  }
//  else
//    return mYOrigin;
//}

//double ReosRasterExtent::xMapMin() const
//{
//  if ( mXCellSize < 0 )
//  {
//    return mXOrigin + mXCellSize * mXCellCount;
//  }
//  else
//    return mXOrigin;
//}

//double ReosRasterExtent::yMapMin() const
//{
//  if ( mYCellSize < 0 )
//  {
//    return mYOrigin + mYCellSize * mYCellCount;
//  }
//  else
//    return mYOrigin;
//}

QPoint ReosRasterExtent::mapToCell( const QPointF &point ) const
{
  int x = int( ( point.x() - xMapOrigin() ) / xCellSize() );
  int y = int( ( point.y() - yMapOrigin() ) / yCellSize() );

  return QPoint( x, y );
}

ReosRasterCellPos ReosRasterExtent::mapToCellPos( const QPointF &point ) const
{
  int col = int( ( point.x() - xMapOrigin() ) / xCellSize() );
  int row = int( ( point.y() - yMapOrigin() ) / yCellSize() );

  return ReosRasterCellPos( row, col );
}

QPointF ReosRasterExtent::interCellToMap( const QPoint &cellPos ) const
{
  return QPointF( cellPos.x() * mXCellSize + xMapOrigin(), yMapOrigin() + cellPos.y() * mYCellSize );
}

double ReosRasterExtent::cellXBeforeToMap( int i ) const
{
  return xMapOrigin() + mXCellSize * ( i /*+ ( mXCellSize > 0 ? 0 : 1 ) */ );
}

double ReosRasterExtent::cellXAfterToMap( int i )const
{
  return xMapOrigin() + mXCellSize * ( i + 1/*+ ( mXCellSize > 0 ? 1 : 0 ) */ );
}

double ReosRasterExtent::cellYBeforeToMap( int i )const
{
  return yMapOrigin() + mYCellSize * ( i /*+ ( mYCellSize > 0 ? 0 : 1 )*/ );
}

double ReosRasterExtent::cellYAfterToMap( int i )const
{
  return yMapOrigin() + mYCellSize * ( i + 1/*+ ( mYCellSize > 0 ? 1 : 0 ) */ );
}

QPointF ReosRasterExtent::cellCenterToMap( const QPoint &cellPos ) const
{
  double x = xMapOrigin() + mXCellSize * ( cellPos.x() + 0.5 );
  double y = yMapOrigin() + mYCellSize * ( cellPos.y() + 0.5 );

  return QPointF( x, y );
}

QPointF ReosRasterExtent::cellCenterToMap( const ReosRasterCellPos &cellPos ) const
{
  double x = xMapOrigin() + mXCellSize * ( cellPos.column() + 0.5 );
  double y = yMapOrigin() + mYCellSize * ( cellPos.row() + 0.5 );

  return QPointF( x, y );
}

QPointF ReosRasterExtent::cellMinMinCornerToMap( const QPoint &cellPos ) const
{
  double x = cellXBeforeToMap( cellPos.x() );
  double y = cellYBeforeToMap( cellPos.y() );

  return QPointF( x, y );
}

QPointF ReosRasterExtent::cellMaxMaxCornerToMap( const QPoint &cellPos ) const
{
  double x = cellXAfterToMap( cellPos.x() );
  double y = cellYAfterToMap( cellPos.y() );

  return QPointF( x, y );
}

QPointF ReosRasterExtent::cellMinMaxCornerToMap( const QPoint &cellPos ) const
{
  double x = cellXBeforeToMap( cellPos.x() );
  double y = cellYAfterToMap( cellPos.y() );

  return QPointF( x, y );
}

QPointF ReosRasterExtent::cellMaxMinCornerToMap( const QPoint &cellPos ) const
{
  double x = cellXAfterToMap( cellPos.x() );
  double y = cellYAfterToMap( cellPos.y() );

  return QPointF( x, y );
}

QRect ReosRasterExtent::mapExtentToCellRect( const ReosMapExtent &mapExtent ) const
{
  double x0;
  double y0;
  double x1;
  double y1;

  if ( mXCellSize > 0 )
  {
    x0 = mapExtent.xMapMin();
    x1 = mapExtent.xMapMax();
  }
  else
  {
    x0 = mapExtent.xMapMax();
    x1 = mapExtent.xMapMin();
  }

  if ( mYCellSize > 0 )
  {
    y0 = mapExtent.yMapMax();
    y1 = mapExtent.yMapMin();
  }
  else
  {
    y0 = mapExtent.yMapMin();
    y1 = mapExtent.yMapMax();
  }

  QPoint pt0;
  QPoint pt1;

  pt0 = mapToCell( QPointF( x0, y0 ) );
  pt1 = mapToCell( QPointF( x1, y1 ) );

  QRect ret( pt0, pt1 );

  return ret.normalized();;
}

ReosMapExtent ReosRasterExtent::cellRectToMapExtent( const QRect &cellRect, const Position &position ) const
{
  QRect cr = cellRect.normalized();

  QPointF pt0;
  QPointF pt1;

  switch ( position )
  {
    case Center:
      pt0 = cellCenterToMap( cr.topLeft() );
      pt1 = cellCenterToMap( cr.bottomRight() );
      break;
    case Interior:
      pt0 = cellMaxMaxCornerToMap( cr.topLeft() );
      pt1 = cellMinMinCornerToMap( cr.bottomRight() );
      break;
    case Exterior:
      pt0 = cellMinMinCornerToMap( cr.topLeft() );
      pt1 = cellMaxMaxCornerToMap( cr.bottomRight() );
      break;
  }

  QRectF ret( pt0, pt1 );
  ret = ret.normalized();
  return ReosMapExtent( ret );
}

double ReosRasterExtent::cellSurface() const {return fabs( mXCellSize * mYCellSize );}

ReosRasterExtent::ReosRasterExtent( double xOrigine, double yOrigine, int XCellCount, int YCellCount, double XCellSize, double YCellSize ):
  ReosMapExtent( XCellSize > 0 ? xOrigine : xOrigine + XCellSize * XCellCount,
                 YCellSize > 0 ? yOrigine : yOrigine + YCellSize * YCellCount,
                 ( XCellSize > 0 ? xOrigine : xOrigine + XCellSize * XCellCount ) + fabs( XCellSize * XCellCount ),
                 ( YCellSize > 0 ? yOrigine : yOrigine + YCellSize * YCellCount ) + fabs( YCellSize * YCellCount ) ),
  mXOrigin( xOrigine ),
  mYOrigin( yOrigine ),
  mXCellSize( XCellSize ),
  mYCellSize( YCellSize ),
  mXCellCount( XCellCount ),
  mYCellCount( YCellCount )
{
  mIsValid = true;
}

ReosRasterExtent::ReosRasterExtent( const ReosMapExtent &extent, int XCellCount, int YcellCount, bool xAscendant, bool yAscendant ):
  ReosMapExtent( extent ), mXCellCount( XCellCount ), mYCellCount( YcellCount )
{
  mIsValid = true;
  mXCellSize = extent.width() / mXCellCount * ( xAscendant ? 1 : -1 );
  mYCellSize = extent.height() / mYCellCount * ( yAscendant ? 1 : -1 );

  mXOrigin = xAscendant ? extent.xMapMin() : extent.xMapMax();
  mYOrigin = yAscendant ? extent.yMapMin() : extent.yMapMax();
}

ReosRasterExtent::ReosRasterExtent( const ReosMapExtent &extent ): ReosMapExtent( extent )
{}

bool ReosRasterExtent::isValid() const
{
  return mIsValid;
}

ReosRasterCellPos::ReosRasterCellPos( int r, int c ): mRow( r ), mColumn( c ) {}

int ReosRasterCellPos::row() const
{
  return mRow;
}

void ReosRasterCellPos::setRow( int value )
{
  mRow = value;
}

int ReosRasterCellPos::column() const
{
  return mColumn;
}

void ReosRasterCellPos::setColumn( int value )
{
  mColumn = value;
}

ReosRasterCellPos ReosRasterCellPos::operator+( const ReosRasterCellPos &other ) const
{
  return ReosRasterCellPos( mRow + other.mRow, mColumn + other.mColumn );
}

ReosRasterCellPos ReosRasterCellPos::operator-( const ReosRasterCellPos &other ) const
{
  return ReosRasterCellPos( mRow - other.mRow, mColumn - other.mColumn );
}

bool ReosRasterCellPos::operator==( const ReosRasterCellPos &other ) const
{
  return ( ( mRow == other.mRow ) && ( mColumn == other.mColumn ) );
}

bool ReosRasterCellPos::operator!=( const ReosRasterCellPos &other ) const
{
  return !( operator==( other ) );
}

ReosRasterCellPos ReosRasterCellPos::neighbourWithDirection( unsigned char direction )
{
  return ReosRasterCellPos( direction % 3 - 1 + mRow, direction / 3 - 1 + mColumn );
}

void ReosRasterCellPos::goInDirection( unsigned char direction )
{
  mRow += direction % 3 - 1;
  mColumn += direction / 3 - 1;
}

bool ReosRasterCellPos::isValid() const
{
  return mRow != -1 && mColumn != -1;
}

RasterNeighborCirculator::RasterNeighborCirculator( const ReosRasterCellPos &central ): mCentral( central )
{}

RasterNeighborCirculator::RasterNeighborCirculator( const ReosRasterCellPos &central, short delta_Row, short delta_Column ): mCentral( central )
{
  setRelativePosition( delta_Row, delta_Column );
}

RasterNeighborCirculator &RasterNeighborCirculator::operator++()
{
  ++mPos;
  if ( mPos == 8 )
    mPos = 0;

  return ( *this );
}

RasterNeighborCirculator &RasterNeighborCirculator::operator--()
{
  if ( mPos == 0 )
    mPos = 7;
  else
    mPos--;

  return ( *this );
}

bool RasterNeighborCirculator::operator==( const RasterNeighborCirculator &other ) const
{
  return ( getPosition() == other.getPosition() );
}

bool RasterNeighborCirculator::operator!=( const RasterNeighborCirculator &other ) const
{
  return !operator==( other );
}

ReosRasterCellPos RasterNeighborCirculator::getPosition() const
{
  return mCentral + cyclePositionToRelativePosition[mPos];
}

void RasterNeighborCirculator::setRelativePosition( short delta_Row, short delta_Column )
{
  if ( ( abs( delta_Row ) > 1 ) || ( abs( delta_Column ) > 1 ) )
    return;
  mPos = relativePositionToCyclePosition[unsigned( delta_Row + 1 )][unsigned( delta_Column + 1 )];
}

const std::array<ReosRasterCellPos, 8> RasterNeighborCirculator::cyclePositionToRelativePosition =
{
  ReosRasterCellPos( 0, 1 ), // 0
  ReosRasterCellPos( -1, 1 ), // 1
  ReosRasterCellPos( -1, 0 ), // 2
  ReosRasterCellPos( -1, -1 ), // 3
  ReosRasterCellPos( 0, -1 ), // 4
  ReosRasterCellPos( 1, -1 ), // 5
  ReosRasterCellPos( 1, 0 ), // 6
  ReosRasterCellPos( 1, 1 ) // 7
};

const std::array<std::array<unsigned, 3>, 3> RasterNeighborCirculator::relativePositionToCyclePosition =
{
  std::array<unsigned, 3>( {3, 2, 1} ),
  std::array<unsigned, 3>( {4, 8, 0} ),
  std::array<unsigned, 3>( {5, 6, 7} )
};

ReosRasterTestingCellInPolygon::ReosRasterTestingCellInPolygon( ReosRasterExtent emprise, const QPolygonF &polygon ): mExtent( emprise ), mPolygon( polygon ) {}

bool ReosRasterTestingCellInPolygon::testCell( const ReosRasterCellPos &cell ) const
{
  QPointF pointMap = mExtent.cellCenterToMap( cell );
  return mPolygon.containsPoint( pointMap, Qt::OddEvenFill );
}

bool ReosRasterTestingCell::testCell( const ReosRasterCellPos &cell ) const
{
  Q_UNUSED( cell );
  return true;
}

