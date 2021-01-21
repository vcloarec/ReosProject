/***************************************************************************
                      reosrastercompressed.h
                     --------------------------------------
Date                 : 16-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosrastercompressed.h"


ReosRasterByteCompressed::ReosRasterByteCompressed( const ReosRasterMemory<unsigned char> &raster ):
  mRowCount( raster.rowCount() ), mColumnCount( raster.columnCount() )
{
  mData.clear();
  for ( int row = 0; row < mRowCount; ++row )
  {
    int column = 0;
    while ( column < mColumnCount )
    {
      unsigned char currentValue = raster.value( row, column );
      unsigned char sameValueCount = 0;
      unsigned char nextValue = currentValue;
      while ( ( column < mColumnCount ) &&
              ( nextValue == currentValue ) &&
              ( sameValueCount < 15 ) )
      {
        sameValueCount++;
        column++;
        nextValue = raster.value( row, column );
      }

      unsigned char valuetoStore = static_cast<char>( ( sameValueCount << 4 ) | currentValue );
      mData.append( static_cast<char>( valuetoStore ) );
    }
  }
}

ReosRasterMemory<unsigned char> ReosRasterByteCompressed::uncompressRaster() const
{
  ReosRasterMemory<unsigned char> raster( mRowCount, mColumnCount );
  raster.reserveMemory();
  QByteArray::const_iterator iterator = mData.constBegin();
  int row = 0;
  int column = 0;

  while ( iterator != mData.constEnd() )
  {
    char valueChar = ( *iterator );
    unsigned char storedValue = static_cast<unsigned char>( valueChar );
    unsigned char value = static_cast<unsigned char>( storedValue & ( 15 ) );
    unsigned char valueCount = static_cast<unsigned char>( storedValue >> 4 );
    iterator++;

    int beginColumn = column;
    column = column + valueCount - 1;
    while ( beginColumn <= column )
    {
      raster.setValue( row, beginColumn, value );
      beginColumn++;
    }

    column++;

    if ( column > mColumnCount - 1 )
    {
      row++;
      column = 0;
    }
  }

  return raster;
}

bool ReosRasterByteCompressed::hasData() const
{
  return !mData.isEmpty();
}

ReosEncodedElement ReosRasterByteCompressed::encode() const
{
  ReosEncodedElement ret( QStringLiteral( "compress-byte-raster" ) );
  ret.addData( QStringLiteral( "row-count" ), mRowCount );
  ret.addData( QStringLiteral( "column-count" ), mColumnCount );
  ret.addData( QStringLiteral( "data" ), mData );

  return ret;
}

ReosRasterByteCompressed ReosRasterByteCompressed::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "compress-byte-raster" ) )
    return ReosRasterByteCompressed();

  int rowCount;
  if ( !element.getData( QStringLiteral( "row-count" ), rowCount ) )
    return ReosRasterByteCompressed();

  int columnCount;
  if ( !element.getData( QStringLiteral( "column-count" ), columnCount ) )
    return ReosRasterByteCompressed();

  QByteArray data;
  if ( !element.getData( QStringLiteral( "data" ), data ) )
    return ReosRasterByteCompressed();

  ReosRasterByteCompressed ret;
  ret.mRowCount = rowCount;
  ret.mColumnCount = columnCount;
  ret.mData = data;

  return ret;
}

bool ReosRasterByteCompressed::operator==( const ReosRasterByteCompressed &other ) const
{
  if ( mRowCount != other.mRowCount )
    return false;

  if ( mColumnCount != other.mColumnCount )
    return false;

  if ( mData != other.mData )
    return false;

  return true;
}

bool ReosRasterByteCompressed::operator!=( const ReosRasterByteCompressed &other ) const
{
  return !operator==( other );
}
