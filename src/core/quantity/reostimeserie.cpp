/***************************************************************************
  reostimeserie.cpp - ReosTimeSerie

 ---------------------
 begin                : 26.1.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostimeserie.h"



ReosTimeSerieModel::ReosTimeSerieModel( QObject *parent ): QAbstractTableModel( parent )
{

}

QModelIndex ReosTimeSerieModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTimeSerieModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTimeSerieModel::rowCount( const QModelIndex & ) const
{
  if ( mData )
    return mData->valueCount();
  else
    return 0;
}

int ReosTimeSerieModel::columnCount( const QModelIndex & ) const
{
  return 2;
}

QVariant ReosTimeSerieModel::data( const QModelIndex &index, int role ) const
{
  if ( !mData )
    return QVariant();

  int maxRowCount = mIsEditable ? mData->valueCount() + 1 : mData->valueCount();

  if ( !index.isValid() || index.row() >= maxRowCount )
    return QVariant();


  if ( role == Qt::DisplayRole )
  {
    if ( index.column() == 0 && index.row() < maxRowCount )
    {
      if ( mData->referenceTime()->value().isValid() )
      {
        return mData->referenceTime()->value().addSecs( mData->relativeTimeAt( index.row() ).valueSeconde() );
      }
      else
        return mData->relativeTimeAt( index.row() ).valueSeconde();
    }


    if ( index.column() == 1 && index.row() < mData->valueCount() )
      return mData->valueAt( index.row() );

    if ( mIsEditable )
    {
      if ( index.column() == 1 && index.row() == mData->valueCount() )
        return QString();
    }

  }

  return QVariant();
}

void ReosTimeSerieModel::setSerieData( std::weak_ptr<ReosTimeSerie> data )
{
  mData = data.lock();
}

void ReosTimeSerieModel::setTimeUnit( ReosDuration::Unit unit )
{
  mTimeUnit = unit;
}

void ReosTimeSerieModel::setEditable( bool b )
{
  mIsEditable = b;
}

ReosTimeSerieConstantInterval::ReosTimeSerieConstantInterval( QObject *parent ):
  ReosTimeSerie( parent )
  , mTimeStep( new ReosParameterDuration( tr( "Time step" ), this ) )
{

}

ReosParameterDuration *ReosTimeSerieConstantInterval::timeStep() const
{
  return mTimeStep;
}

ReosDuration ReosTimeSerieConstantInterval::relativeTimeAt( int i ) const
{
  return mTimeStep->value() * i;
}

ReosTimeSerie::ReosTimeSerie( QObject *parent ):
  QObject( parent )
  , mReferenceTime( new ReosParameterDateTime( tr( "Reference time" ), this ) )
{
}

int ReosTimeSerie::valueCount() const
{
  return mValues.count();
}

double ReosTimeSerie::valueAt( int i ) const
{
  return mValues.at( i );
}
