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

double ReosRasterExtent::xOrigin() const
{
  return mXOrigin;
}

double ReosRasterExtent::yOrigin() const
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
  ReosRasterExtent ret;

  QRectF rect( QPointF( xMin(), yMin() ), QPointF( xMax(), yMax() ) );
  QRectF rectOther( QPointF( other.xMin(), other.yMin() ), QPointF( other.xMax(), other.yMax() ) );

  QRectF result = rect.intersected( rectOther );

  if ( result == QRectF() )
  {
    return ret;
  }
  else
  {
    const QPointF pointOrigine = cellMinMinCornerToMap( mapToCell( result.topLeft() ) );
    ret.mXOrigin = pointOrigine.x();
    ret.mYOrigin = pointOrigine.y();
    ret.mXCellSize = mXCellSize;
    ret.mYCellSize = mYCellSize;
    ret.mXCellCount = result.width() / abs( mXCellSize );
    ret.mYCellCount = result.height() / abs( mYCellSize );
    ret.mIsValid = true;
  }

  return ret;
}

double ReosRasterExtent::xMax() const
{
  if ( mXCellSize > 0 )
  {
    return mXOrigin + mXCellSize * mXCellCount;
  }
  else
    return mXOrigin;
}

double ReosRasterExtent::yMax() const
{
  if ( mYCellSize > 0 )
  {
    return mYOrigin + mYCellSize * mYCellCount;
  }
  else
    return mYOrigin;
}

double ReosRasterExtent::xMin() const
{
  if ( mXCellSize < 0 )
  {
    return mXOrigin + mXCellSize * mXCellCount;
  }
  else
    return mXOrigin;
}

double ReosRasterExtent::yMin() const
{
  if ( mYCellSize < 0 )
  {
    return mYOrigin + mYCellSize * mYCellCount;
  }
  else
    return mYOrigin;
}

QPoint ReosRasterExtent::mapToCell( const QPointF &point ) const
{
  int x = int( ( point.x() - xOrigin() ) / xCellSize() );
  int y = int( ( point.y() - yOrigin() ) / yCellSize() );

  return QPoint( x, y );
}

ReosRasterCellPos ReosRasterExtent::mapToCellPos( const QPointF &point ) const
{
  int col = int( ( point.x() - xOrigin() ) / xCellSize() );
  int row = int( ( point.y() - yOrigin() ) / yCellSize() );

  return ReosRasterCellPos( row, col );
}

QPointF ReosRasterExtent::interCellToMap( const QPoint &cellPos ) const
{
  return QPointF( cellPos.x() * mXCellSize + mXOrigin, mYOrigin + cellPos.y() * mYCellSize );
}

double ReosRasterExtent::cellXBeforeToMap( int i ) const
{
  return mXOrigin + mXCellSize * ( i /*+ ( mXCellSize > 0 ? 0 : 1 ) */ );
}

double ReosRasterExtent::cellXAfterToMap( int i )const
{
  return mXOrigin + mXCellSize * ( i + 1/*+ ( mXCellSize > 0 ? 1 : 0 ) */ );
}

double ReosRasterExtent::cellYBeforeToMap( int i )const
{
  return mYOrigin + mYCellSize * ( i /*+ ( mYCellSize > 0 ? 0 : 1 )*/ );
}

double ReosRasterExtent::cellYAfterToMap( int i )const
{
  return mYOrigin + mYCellSize * ( i + 1/*+ ( mYCellSize > 0 ? 1 : 0 ) */ );
}

QPointF ReosRasterExtent::cellCenterToMap( const QPoint &cellPos ) const
{
  double x = mXOrigin + mXCellSize * ( cellPos.x() + 0.5 );
  double y = mYOrigin + mYCellSize * ( cellPos.y() + 0.5 );

  return QPointF( x, y );
}

QPointF ReosRasterExtent::cellCenterToMap( const ReosRasterCellPos &cellPos ) const
{
  double x = mXOrigin + mXCellSize * ( cellPos.column() + 0.5 );
  double y = mYOrigin + mYCellSize * ( cellPos.row() + 0.5 );

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

QRect ReosRasterExtent::mapRectToCellRect( const QRectF &mapRect ) const
{
  double x0;
  double y0;
  double x1;
  double y1;

  if ( mXCellSize > 0 )
  {
    x0 = mapRect.left();
    x1 = mapRect.right();
  }
  else
  {
    x0 = mapRect.right();
    x1 = mapRect.left();
  }

  if ( mYCellSize > 0 )
  {
    y0 = mapRect.bottom();
    y1 = mapRect.top();
  }
  else
  {
    y0 = mapRect.top();
    y1 = mapRect.bottom();
  }

  QPoint pt0;
  QPoint pt1;

  pt0 = mapToCell( QPointF( x0, y0 ) );
  pt1 = mapToCell( QPointF( x1, y1 ) );

  QRect ret( pt0, pt1 );

  return ret.normalized();;
}

QRectF ReosRasterExtent::cellRectToMapRect( const QRect &cellRect, const Position &position ) const
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

  return ret.normalized();
}

double ReosRasterExtent::cellSurface() const {return fabs( mXCellSize * mYCellSize );}

ReosRasterExtent::ReosRasterExtent( const QRectF &extent, double XCellSize, double YCellSize )
{
  QRectF ext = extent.normalized();
  mIsValid = true;
  mXCellSize = XCellSize;
  mYCellSize = YCellSize;

  if ( mXCellSize > 0 )
    mXOrigin = ext.left();
  else
    mXOrigin = ext.right();

  if ( mYCellSize < 0 )
    mYOrigin = ext.bottom();
  else
    mYOrigin = ext.top();

  mXCellCount = int( fabs( ext.width() / XCellSize ) + 0.5 );
  mYCellCount = int( fabs( ext.height() / YCellSize ) + 0.5 );
}

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
