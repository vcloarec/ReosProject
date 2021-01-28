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



ReosTimeSerieConstantIntervalModel::ReosTimeSerieConstantIntervalModel( QObject *parent ): QAbstractTableModel( parent )
{

}

QModelIndex ReosTimeSerieConstantIntervalModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTimeSerieConstantIntervalModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTimeSerieConstantIntervalModel::rowCount( const QModelIndex & ) const
{
  if ( mData )
    return mIsEditable ? mData->valueCount() + 1 : mData->valueCount();
  else
    return 0;
}

int ReosTimeSerieConstantIntervalModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosTimeSerieConstantIntervalModel::data( const QModelIndex &index, int role ) const
{
  if ( !mData )
    return QVariant();

  int maxRowCount = mIsEditable ? mData->valueCount() + 1 : mData->valueCount();

  if ( !index.isValid() || index.row() >= maxRowCount )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    if ( index.row() < mData->valueCount() )
      return mData->valueAt( index.row() );

    if ( mIsEditable )
    {
      if ( index.row() == mData->valueCount() )
        return QString();
    }
  }

  return QVariant();
}

bool ReosTimeSerieConstantIntervalModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !index.isValid() || !mIsEditable || index.row() > mData->valueCount() )
    return false;

  if ( role == Qt::EditRole )
  {
    bool ok = false;
    double v = value.toString().toDouble( &ok );
    if ( ok )
    {
      if ( index.row() == mData->valueCount() )
      {
        beginInsertRows( QModelIndex(), index.row() + 1, index.row() + 1 );
        mData->appendValue( v );
        endInsertRows();
      }
      else
        mData->setValueAt( index.row(), value.toDouble() );
      return true;
    }
  }
  return false;
}

QVariant ReosTimeSerieConstantIntervalModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Horizontal )
    return QVariant();

  if ( role != Qt::DisplayRole )
    return QVariant();

  int maxRowCount = mIsEditable ? mData->valueCount() + 1 : mData->valueCount();

  if ( section < maxRowCount )
  {
    if ( mData->referenceTime()->value().isValid() )
    {
      return mData->referenceTime()->value().
             addSecs( mData->relativeTimeAt( section ).valueSeconde() ).toString( QStringLiteral( "yyyy.MM.dd HH:mm::ss" ) );
    }
    else
      return mData->relativeTimeAt( section ).valueSeconde();
  }

  return QVariant();
}

Qt::ItemFlags ReosTimeSerieConstantIntervalModel::flags( const QModelIndex &index ) const
{
  if ( mIsEditable )
    return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;
  else
    return QAbstractTableModel::flags( index );
}

void ReosTimeSerieConstantIntervalModel::setSerieData( ReosTimeSerieConstantInterval *data )
{
  mData = data;
  connect( data->timeStep(), &ReosParameter::valueChanged, this, [this]
  {
    emit headerDataChanged( Qt::Vertical, 0, rowCount( QModelIndex() ) ) ;
  } );

  connect( data->referenceTime(), &ReosParameter::valueChanged, this, [this]
  {
    emit headerDataChanged( Qt::Vertical, 0, rowCount( QModelIndex() ) ) ;
  } );
}


void ReosTimeSerieConstantIntervalModel::setEditable( bool b )
{
  mIsEditable = b;
}

ReosTimeSerieConstantInterval::ReosTimeSerieConstantInterval( QObject *parent ):
  ReosTimeSerie( parent )
  , mTimeStep( new ReosParameterDuration( tr( "Time step" ), this ) )
{
  mTimeStep->setValue( ReosDuration( 5, ReosDuration::minute ) );
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
  ReosDataObject( parent )
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
