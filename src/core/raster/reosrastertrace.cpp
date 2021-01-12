/***************************************************************************
                      reosrastertrace.cpp
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

#include "reosrastertrace.h"

ReosRasterTraceBetweenCells::ReosRasterTraceBetweenCells( const QPoint &start,
    const QPoint &origin,
    const QVector<QPoint> &stopLine,
    QList<QPoint> &elimination ):
  mStart( start ),
  mStopLine( stopLine ),
  mCellsToEliminate( elimination ),
  mOrigin( origin ),
  mTreatedCells( 0 ),
  mCellStart( fromBetweenAndDirectionToCells( mStart, mOrigin ) ),
  mErrorCode( -100 ),
  mIsStopped( false )
{}

void ReosRasterTraceBetweenCells::savePosition( bool saveBetweePosition )
{
  if ( saveBetweePosition )
  {
    if ( !mTrace.isEmpty() )
    {
      if ( mPosition != mTrace.last() )
        mTrace.append( mPosition );
    }
    else
      mTrace.append( mPosition );
  }

  if ( mDirection.x() == 0 ) //vertical
  {
    if ( mDirection.y() == -1 )
      mVerticalSegments.append( mPosition + QPoint( 0, -1 ) );
    else
      mVerticalSegments.append( mPosition );
  }

  if ( mDirection.y() == 0 ) //horizontal
  {
    if ( mDirection.x() == -1 )
      mHorizontalSegments.append( mPosition + QPoint( -1, 0 ) );
    else
      mHorizontalSegments.append( mPosition );
  }
}

void ReosRasterTraceBetweenCells::move( bool forceSaving )
{
  bool directionChange = ( mOrigin + mDirection != QPoint() );
  savePosition( ( directionChange ) || ( forceSaving ) );
  mOrigin = QPoint() - mDirection;
  mPosition = mPosition + mDirection;
  mTreatedCells++;
}

void ReosRasterTraceBetweenCells::move()
{
  move( false );
}

int ReosRasterTraceBetweenCells::isInStopLine( const QPoint &p, const QPoint &dir )
{
  int retour;

  QVector<QPoint> pixelsATester = fromBetweenAndDirectionToCells( p, dir );

  retour = mStopLine.indexOf( pixelsATester[0] );

  if ( retour == -1 )
    retour = mStopLine.indexOf( pixelsATester[1] );

  return retour;
}

QPolygon ReosRasterTraceBetweenCells::trace() {return mTrace;}

int ReosRasterTraceBetweenCells::error() const {return mErrorCode;}

ReosRasterTraceBetweenCells::~ReosRasterTraceBetweenCells()
{}

bool ReosRasterTraceBetweenCells::startTracing()
{
  if ( !isValid() )
    return false;

  mPosition = mStart;
  mDirection = QPoint() - mOrigin;

  while ( !isStopped() )
  {
    //Consider all possible directions and remove origin one
    QList<QPoint> listDir = {QPoint( 0, 1 ), QPoint( 1, 0 ), QPoint( 0, -1 ), QPoint( -1, 0 )};
    listDir.removeOne( mOrigin );

    // Reduvce direction list to retain ony valid one (valid ones are following criteria and not "has to be eliminated";
    defineDirection( listDir );

    //No valid direction anymore --> return with error
    if ( listDir.count() == 0 )
    {
      mErrorCode = 3;
      break;
    }

    //Only one valid direction, simply move to this direction
    if ( listDir.count() == 1 )
    {

      mDirection = listDir.last();

      mErrorCode = testPosition( mPosition, mDirection, mTreatedCells );

      if ( mErrorCode != -100 )
      {
        mTrace.append( mPosition );
        break;
      }
      else
        move();
    }

    //************************************** several directions possible
    if ( listDir.count() > 1 )
    {
      mThrownTrace.clear();
      QVector<int> errorsInDirections;
      ReosRasterTraceBetweenCells *selectedTrace = nullptr;

      // Test all direction and construct new trace if test==-100
      int tr = 0;
      bool end = false;
      while ( ( tr < listDir.count() ) && ( !end ) )
      {
        const QPoint dir = listDir.at( tr );

        int testNewPoint = testPosition( mPosition, dir, mTreatedCells );

        if ( testNewPoint == -100 )
        {
          const QPoint newPosition = mPosition + dir;
          const QPoint newOrigin = QPoint() - dir;
          mThrownTrace.append( newTrace( newPosition, newOrigin, mCellsToEliminate ) );
          errorsInDirections.append( testNewPoint );
        }
        else
        {
          errorsInDirections.append( testNewPoint );
          mThrownTrace.append( nullptr );
        }
        tr++;
        end = ( errorsInDirections.last() == 0 );
      }

      setToBeEliminated( mPosition );


      // Start created trace
      for ( int i = 0; i < mThrownTrace.count(); ++i )
      {
        if ( mThrownTrace.at( i ) )
        {
          mThrownTrace.at( i )->startTracing();
          errorsInDirections[i] = mThrownTrace.at( i )->error();
        }
      }

      // Handle result in each direction
      // Retains the shortest trace with error code 0
      int selectedTraceIndex = errorsInDirections.indexOf( 0 );

      if ( selectedTraceIndex >= 0 )
      {
        selectedTrace = mThrownTrace.at( selectedTraceIndex );
        for ( int i = 0; i < errorsInDirections.count(); ++i )
        {
          if ( errorsInDirections.at( i ) == 0 )
          {
            int lastCount = -1;
            if ( selectedTrace )
              lastCount = selectedTrace->trace().count();

            int newCount = -1;
            if ( mThrownTrace.at( i ) )
              newCount = mThrownTrace.at( i )->trace().count();

            if ( newCount > lastCount )
              selectedTraceIndex = i;
          }
        }
      }
      else
      {
        for ( int i = 0; i < errorsInDirections.count(); ++i )
          if ( ( errorsInDirections.at( i ) == 2 ) || ( errorsInDirections.at( i ) == 4 ) )
          {
            selectedTraceIndex = i;
          }

        if ( selectedTraceIndex < 0 )
          selectedTraceIndex = 0;

      }

      // join resulting trace with current trace
      mErrorCode = errorsInDirections.at( selectedTraceIndex );
      selectedTrace = mThrownTrace.at( selectedTraceIndex );

      if ( ( mErrorCode == 3 ) || ( mErrorCode > 4 ) )
      {
        for ( int i = 0; i < mThrownTrace.count(); ++i )
          if ( mThrownTrace.at( i ) )
            mTrace << mThrownTrace.at( i )->trace();
      }
      else
      {
        if ( selectedTrace )
        {
          mDirection = listDir.at( selectedTraceIndex );
          move();
          mTrace << selectedTrace->trace();
          selectedTrace->addVerticalSegments( mVerticalSegments );
          selectedTrace->addHorizontalSegments( mHorizontalSegments );
          mPosition = selectedTrace->position();
          mDirection = selectedTrace->direction();
        }
        else
        {
          mDirection = listDir.at( selectedTraceIndex );
        }
      }

      for ( int i = 0; i < mThrownTrace.count(); ++i )
        if ( mThrownTrace.at( i ) )
          delete mThrownTrace.at( i );

      break;
    }
  }

  if ( mErrorCode == 0 )
    return true;
  else
    return false;
}

int ReosRasterTraceBetweenCells::testPosition( const QPoint &p, const QPoint &dir, int cellCount )
{
  int ret = -100;

  int arrivalTestResult = arrivalTest( p, dir );

  if ( arrivalTestResult != -100 )
    return arrivalTestResult;


  if ( hasToBeEliminated( p ) )
    return 1;

  if ( cellCount > mLimit )
    return 2;

  return ret;

}

void ReosRasterTraceBetweenCells::setToBeEliminated( const QPoint &p )
{
  mCellsToEliminate.append( p );
}

bool ReosRasterTraceBetweenCells::hasToBeEliminated( const QPoint &p ) const
{
  return mCellsToEliminate.contains( p );
}


QVector<QPoint> ReosRasterTraceBetweenCells::fromBetweenAndDirectionToCells( const QPoint &interpixel, const QPoint &dir )
{
  int termeX = interpixel.x() + ( abs( dir.x() ) + dir.x() ) / 2 - 1;
  int termeY = interpixel.y() + ( abs( dir.y() ) + dir.y() ) / 2 - 1;
  int invX = abs( dir.y() );
  int invY = abs( dir.x() );
  QPoint px1( termeX + invX, termeY + invY );
  QPoint px2( termeX, termeY );

  if ( dir.y() == -1 )
  {
    px1 = interpixel + QPoint( -1, -1 );
    px2 = interpixel + QPoint( 0, -1 );;
  }

  if ( dir.y() == 1 )
  {
    px1 = interpixel + QPoint( 0, 0 );
    px2 = interpixel + QPoint( -1, 0 );
  }

  if ( dir.x() == -1 )
  {
    px1 = interpixel + QPoint( -1, 0 );
    px2 = interpixel + QPoint( -1, -1 );
  }

  if ( dir.x() == 1 )
  {
    px1 = interpixel + QPoint( 0, -1 );
    px2 = interpixel + QPoint( 0, 0 );
  }

  QVector<QPoint> retour = {px1, px2};

  return retour;
}

void ReosRasterTraceBetweenCells::stopTrace()
{
  mIsStopped = true;
  for ( int i = 0; i < mThrownTrace.count(); ++i )
    if ( mThrownTrace.at( i ) )
      mThrownTrace.at( i )->stopTrace();
}

bool ReosRasterTraceBetweenCells::isStopped()
{
  return mIsStopped;
}

void ReosRasterTraceBetweenCells::eliminateDirection( QList<QPoint> &listDir )
{
  int i = 0;
  while ( i < listDir.count() )
  {
    const QPoint p = mPosition + listDir.at( i );

    if ( hasToBeEliminated( p ) )
      listDir.removeAt( i );
    else
      ++i;
  }
}

int ReosRasterTraceBetweenCells::arrivalTest( const QPoint &p, const QPoint &dir )
{
  int endIndex = isInStopLine( p, dir );
  if ( ( endIndex != -1 ) && ( mTrace.count() > 2 ) )
    return 0;

  return -100;
}
