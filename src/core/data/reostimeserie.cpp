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

  switch ( role )
  {
    case Qt::DisplayRole:
      if ( index.row() < mData->valueCount() )
        return mData->valueAt( index.row() );

      if ( mIsEditable )
      {
        if ( index.row() == mData->valueCount() )
          return QString();
      }
      break;
    case Qt::TextAlignmentRole:
      return Qt::AlignRight;
      break;
    default:
      return QVariant();
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
  {
    if ( section == 0 && role == Qt::DisplayRole )
    {
      switch ( mData->valueMode() )
      {
        case ReosTimeSerieConstantInterval::Value:
        case ReosTimeSerieConstantInterval::Cumulative:
          return  mData->valueUnit();
          break;
        case ReosTimeSerieConstantInterval::Intensity:
          return tr( "Intensity (%1/%2)" ).arg( mData->valueUnit(), mData->timeStep()->value().unitToString() );
          break;
      }
    }
    return QVariant();
  }

  if ( role != Qt::DisplayRole )
    return QVariant();

  int maxRowCount = mIsEditable ? mData->valueCount() + 1 : mData->valueCount();

  if ( section < maxRowCount )
  {
    if ( mData->referenceTime()->value().isValid() )
    {
      return mData->referenceTime()->value().
             addSecs( mData->relativeTimeAt( section ).valueSecond() ).toString( QStringLiteral( "yyyy.MM.dd HH:mm:ss" ) );
    }
    else
      return mData->relativeTimeAt( section ).valueSecond();
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

  connect( data->timeStep(), &ReosParameter::unitChanged, this, [this]
  {
    emit headerDataChanged( Qt::Horizontal, 0, 1 ) ;
  } );

  connect( data->referenceTime(), &ReosParameter::valueChanged, this, [this]
  {
    emit headerDataChanged( Qt::Vertical, 0, rowCount( QModelIndex() ) ) ;
  } );

  connect( data, &ReosDataObject::dataChanged, this, [this]
  {
    emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 0, QModelIndex() ) );
    emit headerDataChanged( Qt::Horizontal, 0, 1 ) ;
  } );
}


void ReosTimeSerieConstantIntervalModel::setEditable( bool b )
{
  mIsEditable = b;
}

void ReosTimeSerieConstantIntervalModel::setValues( const QModelIndex &fromIndex, const QList<double> &values )
{
  if ( !fromIndex.isValid() )
    return;

  int startRow = fromIndex.row();
  int endRow = startRow + values.count() - 1;
  int insertedRowCount = endRow - mData->valueCount() + 1;
  if ( insertedRowCount > 0 )
    beginInsertRows( QModelIndex(), rowCount( QModelIndex() ) - 1, rowCount( QModelIndex() ) + insertedRowCount - 2 );

  for ( int i = 0; i < values.count(); ++i )
  {
    if ( i + startRow == mData->valueCount() )
      mData->appendValue( values.at( i ) );
    else
      mData->setValueAt( i + startRow, values.at( i ) );
  }

  if ( insertedRowCount > 0 )
    endInsertRows();

  emit dataChanged( index( startRow, 0, QModelIndex() ), index( endRow, 0, QModelIndex() ) );
}

void ReosTimeSerieConstantIntervalModel::deleteValueRows( const QModelIndex &fromIndex, int count )
{
  if ( !fromIndex.isValid() )
    return;
  int maxCount = std::min( count, mData->valueCount() - fromIndex.row() );
  beginRemoveRows( QModelIndex(), fromIndex.row(), fromIndex.row() + maxCount - 1 );
  mData->removeValues( fromIndex.row(), count );
  endRemoveRows();
}

void ReosTimeSerieConstantIntervalModel::insertValueRows( const QModelIndex &fromIndex, int count )
{
  beginInsertRows( QModelIndex(), fromIndex.row(), fromIndex.row() + count - 1 );
  mData->insertValues( fromIndex.row(), count, defaultValue );
  endInsertRows();
}

ReosTimeSerieConstantInterval::ReosTimeSerieConstantInterval( QObject *parent ):
  ReosTimeSerie( parent )
  , mTimeStep( new ReosParameterDuration( tr( "Time step" ), this ) )
{
  mTimeStep->setValue( ReosDuration( 5, ReosDuration::minute ) );
  connectParameters();
}

ReosParameterDuration *ReosTimeSerieConstantInterval::timeStep() const
{
  return mTimeStep;
}

ReosDuration ReosTimeSerieConstantInterval::relativeTimeAt( int i ) const
{
  return mTimeStep->value() * i;
}

QPair<QDateTime, QDateTime> ReosTimeSerieConstantInterval::timeExtent() const
{
  QPair<QDateTime, QDateTime> ret;
  if ( mValues.size() > 0 )
    ret.first = timeAt( 0 );
  else
    return ret;

  if ( mValues.size() > 1 )
    ret.second = timeAt( mValues.size() - 1 ).addMSecs( mTimeStep->value().valueMilliSecond() );
  else
    ret.second = ret.first;

  return ret;
}

double ReosTimeSerieConstantInterval::valueAt( int i ) const
{
  return valueWithMode( i, mValueMode );

}

double ReosTimeSerieConstantInterval::valueWithMode( int i, ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  switch ( mode )
  {
    case ReosTimeSerieConstantInterval::Value:
      return mValues.at( i );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      return mValues.at( i ) / mTimeStep->value().valueUnit();
      break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      double sum = 0;
      for ( int v = 0; v < i; ++v )
        sum += mValues.at( v );
      return sum;
    }
    break;
  }

  return 0;
}

QPair<double, double> ReosTimeSerieConstantInterval::extentValueWithMode( ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  if ( mValues.isEmpty() )
    return QPair<double, double>( std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN() );
  double min = std::numeric_limits<double>::max();
  double max = -std::numeric_limits<double>::max();

  for ( int i = 0; i < valueCount(); ++i )
  {
    double v = valueWithMode( i, mode );
    if ( v <= min )
      min = v;
    if ( v >= max )
      max = v;
  }

  return QPair<double, double>( min, max );
}

void ReosTimeSerie::setValueAt( int i, double value )
{
  if ( i < mValues.count() )
  {
    mValues[i] = value;
    emit dataChanged();
  }
}

void ReosTimeSerie::appendValue( double value )
{
  mValues.append( value );
  emit dataChanged();
}

QString ReosTimeSerieConstantInterval::type() const {return QStringLiteral( "time-serie-constant-interval" );}

ReosEncodedElement ReosTimeSerieConstantInterval::encode() const
{
  ReosEncodedElement element( QStringLiteral( "time-serie-constant-interval" ) );
  ReosTimeSerie::baseEncode( element );
  element.addEncodedData( QStringLiteral( "time-step" ), mTimeStep->encode() );

  return element;
}

ReosTimeSerieConstantInterval *ReosTimeSerieConstantInterval::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "time-serie-constant-interval" ) )
    return nullptr;

  std::unique_ptr<ReosTimeSerieConstantInterval> ret = std::make_unique<ReosTimeSerieConstantInterval>( parent );

  if ( !ret->decodeBase( element ) )
    return nullptr;

  ReosParameterDuration *newTimeStep =
    ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ), false, parent );

  if ( newTimeStep )
  {
    ret->mTimeStep->deleteLater();
    ret->mTimeStep = newTimeStep;
  }
  else
    return nullptr;

  ret->connectParameters();

  return ret.release();

}

ReosTimeSerieConstantInterval::ValueMode ReosTimeSerieConstantInterval::valueMode() const
{
  return mValueMode;
}

void ReosTimeSerieConstantInterval::setValueMode( const ValueMode &valueMode )
{
  mValueMode = valueMode;
  emit dataChanged();
  emit settingsChanged();
}

void ReosTimeSerieConstantInterval::connectParameters()
{
  ReosTimeSerie::connectParameters();
  connect( mTimeStep, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mTimeStep, &ReosParameter::unitChanged, this, &ReosDataObject::dataChanged );
}

bool ReosTimeSerieConstantInterval::addCumultive() const
{
  return mAddCumultive;
}

void ReosTimeSerieConstantInterval::setAddCumultive( bool addCumultive )
{
  mAddCumultive = addCumultive;
}

QString ReosTimeSerie::valueUnit() const
{
  return mValueUnit;
}

void ReosTimeSerie::setValueUnit( const QString &valueUnit )
{
  mValueUnit = valueUnit;
}

QString ReosTimeSerieConstantInterval::valueModeName( ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  if ( mValueModeName.contains( mode ) )
    return mValueModeName.value( mode );
  else
    return QString();
}

void ReosTimeSerieConstantInterval::setValueModeName( ReosTimeSerieConstantInterval::ValueMode mode, const QString &name )
{
  mValueModeName[mode] = name;
}

QColor ReosTimeSerieConstantInterval::valueModeColor( ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  if ( mValueModeName.contains( mode ) )
    return mValueModeColor.value( mode );
  else
    return Qt::black;
}

QColor ReosTimeSerieConstantInterval::currentValueModeColor() const
{
  return valueModeColor( mValueMode );
}

void ReosTimeSerieConstantInterval::setValueModeColor( ReosTimeSerieConstantInterval::ValueMode mode, const QColor &color )
{
  mValueModeColor[mode] = color;
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

QDateTime ReosTimeSerie::timeAt( int i ) const
{
  if ( mReferenceTime->value().isValid() )
    return mReferenceTime->value().addMSecs( relativeTimeAt( i ).valueMilliSecond() );
  else
    return QDateTime();
}

double ReosTimeSerie::valueAt( int i ) const
{
  return mValues.at( i );
}

void ReosTimeSerie::removeValues( int fromPos, int count )
{
  int maxCount = std::min( count, mValues.count() - fromPos );
  QVector<double>::iterator itStart = mValues.begin() + fromPos;
  QVector<double>::iterator itEnd = itStart + maxCount;
  mValues.erase( itStart, itEnd );

  emit dataChanged();
}

void ReosTimeSerie::insertValues( int fromPos, int count, double value )
{
  for ( int i = 0; i < count; ++i )
    mValues.insert( fromPos, value );

  emit dataChanged();
}

QPair<double, double> ReosTimeSerie::valueExent() const
{
  if ( mValues.isEmpty() )
    return QPair<double, double>( std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN() );
  double min = std::numeric_limits<double>::max();
  double max = -std::numeric_limits<double>::max();

  for ( int i = 0; i < valueCount(); ++i )
  {
    double v = valueAt( i );
    if ( v <= min )
      min = v;
    if ( v >= max )
      max = v;
  }

  return QPair<double, double>( min, max );
}

void ReosTimeSerie::baseEncode( ReosEncodedElement &element ) const
{
  element.addEncodedData( QStringLiteral( "reference-time" ), mReferenceTime->encode() );
  element.addData( QStringLiteral( "values" ), mValues );
}

bool ReosTimeSerie::decodeBase( const ReosEncodedElement &element )
{
  if ( !element.getData( QStringLiteral( "values" ), mValues ) )
    return true;

  ReosParameterDateTime *newTimeref = ReosParameterDateTime::decode(
                                        element.getEncodedData( QStringLiteral( "reference-time" ) ),
                                        false, this );
  if ( newTimeref )
  {
    mReferenceTime->deleteLater();
    mReferenceTime = newTimeref;
    return true;
  }
  else
    return false;
}

void ReosTimeSerie::connectParameters()
{
  connect( mReferenceTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

QString ReosDataObject::name() const
{
  return mName;
}

void ReosDataObject::setName( const QString &name )
{
  mName = name;
}
