/***************************************************************************
                      reosrasterline.cpp
                     --------------------------------------
Date                 : 18-11-2018
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

#include "reosrasterline.h"

ReosRasterLine::ReosRasterLine( bool thick ): mThick( thick ) {}

void ReosRasterLine::addPoint( int row, int column )
{
  const ReosRasterCellPos &lastPos = lastCellPosition();
  if ( lastPos.isValid() )
  {
    if ( lastPos != ReosRasterCellPos( row, column ) )
      drawLine( lastPos.row(), lastPos.column(), row, column );
  }
  else
    addCell( row, column );

  for ( unsigned i = 0; i < cellCount(); ++i )
  {
    const ReosRasterCellPos &cell = cellPosition( i );
    int row = cell.row();
    int col = cell.column();

    if ( row < mRowMin )
      mRowMin = row;

    if ( row > mRowMax )
      mRowMax = row;

    if ( col < mColMin )
      mColMin = col;

    if ( col > mColMax )
      mColMax = col;
  }
}

void ReosRasterLine::addPoint( const ReosRasterCellPos &cell )
{
  addPoint( cell.row(), cell.column() );
}

bool ReosRasterLine::contains( const ReosRasterCellPos &cell )
{
  int row = cell.row();
  int col = cell.column();
  if ( row < mRowMin )
    return false;

  if ( row > mRowMax )
    return false;

  if ( col < mColMin )
    return false;

  if ( col > mColMax )
    return false;

  return vectorContain( row, col );
}

void ReosRasterLine::drawLine( int ri, int ci, int rf, int cf )
{
  //source :http://raphaello.univ-fcomte.fr/IG/Algorithme/Algorithmique.htm
  int dr, dc, i, rinc, cinc, cumul, r, c ;

  r = ri ;
  c = ci ;
  dr = rf - ri ;
  dc = cf - ci ;
  rinc = ( dr > 0 ) ? 1 : -1 ;
  cinc = ( dc > 0 ) ? 1 : -1 ;

  dr = abs( dr ) ;
  dc = abs( dc ) ;


  if ( dr > dc )
  {
    cumul = dr / 2 ;
    for ( i = 1 ; i < dr ; i++ )
    {
      r += rinc ;
      cumul += dc ;
      if ( cumul >= dr )
      {
        cumul -= dr ;
        c += cinc ;
      }

      if ( mThick )
      {
        const ReosRasterCellPos &lastPos = lastCellPosition();
        int d_r = r - lastPos.row();
        int d_c = c - lastPos.column();
        if ( d_r * d_c != 0 )
          addCell( r - d_r, c );
      }
      addCell( r, c );
    }
  }
  else
  {
    cumul = dc / 2 ;
    for ( i = 1 ; i < dc ; i++ )
    {
      c += cinc ;
      cumul += dr ;
      if ( cumul >= dc )
      {
        cumul -= dc ;
        r += rinc ;
      }

      if ( mThick )
      {
        const ReosRasterCellPos &lastPos = lastCellPosition();
        int d_r = r - lastPos.row();
        int d_c = c - lastPos.column();
        if ( d_r * d_c != 0 )
          addCell( r - d_r, c );
      }
      addCell( r, c );
    }
  }

  if ( mThick )
  {
    const ReosRasterCellPos &lastPos = lastCellPosition();
    int d_r = rf - lastPos.row();
    int d_c = cf - lastPos.column();
    if ( d_r * d_c != 0 )
      addCell( rf - d_r, cf );
  }
  addCell( rf, cf );
}

const ReosRasterCellPos ReosRasterLine::lastCellPosition() const
{
  if ( mCells.size() > 0 )
    return mCells.back();
  else
    return ReosRasterCellPos();
}

const ReosRasterCellPos ReosRasterLine::cellPosition( unsigned int i ) const
{
  if ( i < mCells.size() )
    return mCells.at( i );
  else
    return ReosRasterCellPos();
}

void ReosRasterLine::addCell( int row, int column )
{
  mCells.push_back( ReosRasterCellPos( row, column ) );
}

bool ReosRasterLine::vectorContain( int row, int col ) const
{
  const ReosRasterCellPos cell( row, col );
  return ( std::find( mCells.begin(), mCells.end(), cell ) != mCells.end() );
}

unsigned ReosRasterLine::cellCount() const { return unsigned( mCells.size() );}
