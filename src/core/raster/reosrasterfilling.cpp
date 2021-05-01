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

ReosRasterFilling::ReosRasterFilling( const ReosRasterMemory<float> &dem, double XSize, double YSize ):
  mDem( dem ), mXSize( XSize ), mYSize( YSize )
{
  if ( mMimimumSlope < ( 1e-4f / std::min( fabs( mYSize ), fabs( mXSize ) ) ) )
    mMimimumSlope = ( 1e-4f / std::min( fabs( mYSize ), fabs( mXSize ) ) );
}

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

const ReosRasterMemory<float> &ReosRasterFilling::filledDEM() const {return mDem;}

ReosRasterFillingWangLiu::ReosRasterFillingWangLiu( const ReosRasterMemory<float> &dem, double XSize, double YSize ):
  ReosRasterFilling( dem, XSize, YSize )
{}

bool ReosRasterFillingWangLiu::initialize()
{
  mRasterChar = ReosRasterMemory<bool>();

  if ( !( mDem.isValid() ) )
    return false;

  if ( !mRasterChar.reserveMemory( mDem.rowCount(), mDem.columnCount() ) )
    return false;

  mRasterChar.fill( false );
  mRasterChar.setNodata( true );
  setMaxProgression( mDem.rowCount()*mDem.columnCount() );
  return true;
}

bool ReosRasterFillingWangLiu::makePriorityStack()
{
  std::set<ReosRasterCellValue<float>>::iterator insertion = mPriorityStack.begin();
  int r = 0;
  int c = 0;
  float noData = mDem.noData();

  while ( r < mDem.rowCount() - 1 )
  {
    markPixel( r, c );
    if ( mDem.value( r, c ) == noData )
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
    else
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
    ++r;
  }

  while ( c < mDem.columnCount() - 1 )
  {
    markPixel( r, c );
    if ( mDem.value( r, c ) == noData )
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
    else
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
    ++c;
  }

  while ( r > 0 )
  {
    markPixel( r, c );
    if ( mDem.value( r, c ) == noData )
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
    else
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
    --r;
  }

  while ( c > 0 )
  {
    markPixel( r, c );
    if ( mDem.value( r, c ) == noData )
      mNoDataStack.push_back( ReosRasterCellValue<float>( mDem, r, c ) );
    else
      insertion = mPriorityStack.insert( insertion, ReosRasterCellValue<float>( mDem, r, c ) );
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
      if ( !( mRasterChar.value( RowNeigh, ColNeigh ) ) && ( ( i * j ) != 1 ) )
      {
        ReosRasterCellValue<float> neigh( mDem, RowNeigh, ColNeigh );
        if ( neigh.value() == mDem.noData() )
          mNoDataStack.push_back( neigh );
        else
        {
          float delta = ( sqrt( powf( ( i - 1 ) * float( mXSize ), 2 ) + powf( ( j - 1 ) * float( mYSize ), 2 ) ) ) * mMimimumSlope;
          if ( neigh.value() <= Zcentral + delta )
            neigh.setValue( Zcentral + delta );

          mPriorityStack.insert( neigh );
        }
        markPixel( RowNeigh, ColNeigh );
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
      if ( !( mRasterChar.value( RowNeigh, ColNeigh ) ) && ( ( i * j ) != 1 ) )
      {
        ReosRasterCellValue<float> neigh( mDem, RowNeigh, ColNeigh );
        if ( neigh.value() == mDem.noData() )
          mNoDataStack.push_back( neigh );
        else
          mPriorityStack.insert( neigh );

        markPixel( RowNeigh, ColNeigh );
      }
    }
}

void ReosRasterFillingWangLiu::markPixel( int row, int column )
{
  mRasterChar.setValue( row, column, true );
  mProgession++;
  if ( mProgession % 100 == 0 )
    setCurrentProgression( mProgession );
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

  mIsSuccessful = true;

  return;
}








