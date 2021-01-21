/***************************************************************************
                      reosrasterfilling.cpp
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

#include "reosrasterfilling.h"

ReosRasterFilling::~ReosRasterFilling() {}

float ReosRasterFilling::mimimumSlope() const
{
  return mMimimumSlope;
}

void ReosRasterFilling::setMimimumSlope( float value )
{
  mMimimumSlope = value;
}

void ReosRasterFilling::setXSize( float value )
{
  mXSize = value;
}

void ReosRasterFilling::setYSize( float value )
{
  mYSize = value;
}

ReosRasterMemory<float> ReosRasterFilling::filledDEM() {return mDem;}

unsigned char ReosRasterFilling::calculateDirection( int row, int column )
{
  unsigned char dir = 4;
  float min = mDem.value( row, column );

  if ( min == mDem.noData() )
    return 9;

  for ( int i = 0; i < 3; ++i )
    for ( int j = 0; j < 3; ++j )
    {
      float z = mDem.value( row - 1 + i, column - 1 + j );
      if ( ( z < min ) && ( z != mDem.noData() ) && ( ( i * j ) != 1 ) )
      {
        dir = static_cast<unsigned char>( i + 3 * j );
        min = z;
      }
    }

  return dir;

}

ReosRasterFillingWangLiu::ReosRasterFillingWangLiu( const ReosRasterMemory<float> &dem, double XSize, double YSize ):
  ReosRasterFilling( dem, XSize, YSize )
{}

bool ReosRasterFillingWangLiu::initialize()
{
  mRasterChar = ReosRasterMemory<unsigned char>();

  if ( !( mDem.isValid() ) )
    return false;

  if ( !mRasterChar.reserveMemory( mDem.rowCount(), mDem.columnCount() ) )
    return false;

  mRasterChar.fill( 255 );
  mRasterChar.setNodata( 9 );
  setMaxProgression( mDem.rowCount()*mDem.columnCount() );
  return true;
}

bool ReosRasterFillingWangLiu::makePriorityStack()
{
  std::set<ReosRasterCellValue<float>>::iterator insertion = mPriorityStack.begin();
  int r = 0;
  int c = 0;
  float noData = mDem.noData();

  //! Take borders cells and insert them in the prioriry stack if no noData

  while ( r < mDem.rowCount() - 1 )
  {
    mRasterChar.setValue( r, c, 254 );
    if ( mDem.value( r, c ) == noData )
    {
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( mNoDataStack.back() );
    }
    else
    {
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( ( *insertion ) );
    }

    ++r;
  }

  while ( c < mDem.columnCount() - 1 )
  {
    mRasterChar.setValue( r, c, 254 );
    if ( mDem.value( r, c ) == noData )
    {
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( mNoDataStack.back() );
    }
    else
    {
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( ( *insertion ) );
    }
    ++c;
  }

  while ( r > 0 )
  {
    mRasterChar.setValue( r, c, 254 );
    if ( mDem.value( r, c ) == noData )
    {
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( mNoDataStack.back() );
    }
    else
    {
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( ( *insertion ) );
    }
    --r;
  }

  while ( c > 0 )
  {
    mRasterChar.setValue( r, c, 254 );
    if ( mDem.value( r, c ) == noData )
    {
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( mNoDataStack.back() );
    }
    else
    {
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
      mCelllsToCalculateDirectionAtTheEnd.push_back( ( *insertion ) );
    }
    --c;
  }

  return true;
}

void ReosRasterFillingWangLiu::processCell( const ReosRasterCellValue<float> &central )
{
  //! For the given cell, turn arround it and fill the neighbor if the value is bellow the central value
  float Zcentral = central.value();
  int r = central.row();
  int c = central.column();
  for ( int i = 0; i < 3; ++i )
    for ( int j = 0; j < 3; ++j )
    {
      int RowNeigh = r + i - 1;
      int ColNeigh = c + j - 1;
      if ( ( mRasterChar.value( RowNeigh, ColNeigh ) == 255 ) && ( ( i * j ) != 1 ) )
      {
        ReosRasterCellValue<float> neigh( mDem, RowNeigh, ColNeigh );
        if ( neigh.value() == mDem.noData() )
        {
          mNoDataStack.push_back( neigh );
          setDirectionValue( RowNeigh, ColNeigh, 9 );
        }
        else
        {
          float delta = ( sqrt( powf( ( i - 1 ) * mXSize, 2 ) + powf( ( j - 1 ) * mYSize, 2 ) ) ) * mMimimumSlope;
          if ( neigh.value() <= Zcentral + delta )
          {
            neigh.setValue( Zcentral + delta );
          }
          unsigned char dir = static_cast<unsigned char>( ( r - RowNeigh + 1 ) + 3 * ( c - ColNeigh + 1 ) );
          mPriorityStack.insert( neigh );
          setDirectionValue( RowNeigh, ColNeigh, dir );
        }

      }
    }

}

void ReosRasterFillingWangLiu::processNoDataCell( const ReosRasterCellValue<float> &central )
{
  int r = central.row();
  int c = central.column();
  for ( int i = 0; i < 3; ++i )
    for ( int j = 0; j < 3; ++j )
    {
      int RowNeigh = r + i - 1;
      int ColNeigh = c + j - 1;
      if ( ( mRasterChar.value( RowNeigh, ColNeigh ) == 255 ) && ( ( i * j ) != 1 ) )
      {
        ReosRasterCellValue<float> neigh( mDem, RowNeigh, ColNeigh );
        if ( neigh.value() == mDem.noData() )
        {
          mNoDataStack.push_back( neigh );
          setDirectionValue( RowNeigh, ColNeigh, 9 );
        }
        else
        {
          mPriorityStack.insert( neigh );
          mCelllsToCalculateDirectionAtTheEnd.push_back( neigh );
        }

      }
    }
}

bool ReosRasterFillingWangLiu::calculateBorderDirections()
{

  for ( auto cell : mCelllsToCalculateDirectionAtTheEnd )
  {
    int r = cell.row();
    int c = cell.column();
    setDirectionValue( r, c, calculateDirection( r, c ) );
  }

  return true;
}

void ReosRasterFillingWangLiu::setDirectionValue( int row, int column, unsigned char value )
{
  mRasterChar.setValue( row, column, value );
  mProgession++;
  if ( mProgession % 100 == 0 )
  {
    setCurrentProgression( mProgession );
    if ( isStop() )
      stop( true );
  }
}

void ReosRasterFillingWangLiu::start()
{
  mIsSuccessful = false;

  if ( !initialize() )
    return;

  if ( !mDem.isValid() )
    return;

  if ( !makePriorityStack() )
    return;

  while ( !( ( mPriorityStack.empty() && mNoDataStack.empty() ) || isStop() ) )
  {
    ReosRasterCellValue<float> cell( mDem );
    if ( mNoDataStack.empty() )
    {
      cell = ( *mPriorityStack.begin() );
      mPriorityStack.erase( mPriorityStack.begin() );
    }
    else
    {
      cell = mNoDataStack.back();
      mNoDataStack.pop_back();
    }

    processCell( cell );
  }

  if ( isStop() )
    return;

  calculateBorderDirections();

  mIsSuccessful = true;

  return;
}








