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
    case Qt::EditRole:
      if ( index.row() < mData->valueCount() )
        return ReosParameter::doubleToString( mData->valueAt( index.row() ), 2 );

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
    double v = ReosParameter::stringToDouble( value.toString(), &ok );
    if ( ok )
    {
      if ( index.row() == mData->valueCount() )
      {
        beginInsertRows( QModelIndex(), index.row() + 1, index.row() + 1 );
        mData->appendValue( v );
        endInsertRows();
      }
      else
        mData->setValueAt( index.row(), v );
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
      return mData->unitStringCurrentMode();
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
    ret.first = referenceTime()->value();

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

void ReosTimeSerieConstantInterval::setValueAt( int i, double value )
{
  switch ( mValueMode )
  {
    case ReosTimeSerieConstantInterval::Value:
      ReosTimeSerie::setValueAt( i, value );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      ReosTimeSerie::setValueAt( i, convertFromIntensityValue( value ) );
      break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      if ( i > 0 )
      {
        double cumulValueBefore = valueWithMode( i - 1 );
        ReosTimeSerie::setValueAt( i, value - cumulValueBefore );
      }
      else
      {}     // nothing to do, cumulative value at i=0 is always 0, so it is wrong to set another value
    }
    break;
  }
}

void ReosTimeSerieConstantInterval::appendValue( double value )
{
  switch ( mValueMode )
  {
    case ReosTimeSerieConstantInterval::Value:
      mValues.append( value );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      mValues.append( convertFromIntensityValue( value ) );
      break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      if ( mValues.count() > 0 )
      {
        double cumulValueBefore = valueWithMode( mValues.count() - 1 );
        mValues.append( value - cumulValueBefore );
      }
      else
      {
        mValues.append( value );
      }
    }
    break;
  }

  emit dataChanged();
}

void ReosTimeSerieConstantInterval::insertValues( int fromPos, int count, double value )
{
  switch ( mValueMode )
  {
    case ReosTimeSerieConstantInterval::Value:
      for ( int i = 0; i < count; ++i )
        mValues.insert( fromPos, value );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
    {
      double intensity = convertFromIntensityValue( value );
      for ( int i = 0; i < count; ++i )
        mValues.insert( fromPos, convertFromIntensityValue( intensity ) );
    }
    break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      double cumulValueBefore = valueWithMode( mValues.count() - 1 );
      if ( fromPos > 0 )
        mValues.insert( fromPos, 1, value - cumulValueBefore );
      if ( count > 1 )
        for ( int i = 0; i < count - 1; ++i )
          mValues.insert( fromPos + 1,  0.0 );
    }
    break;
  }
}

double ReosTimeSerieConstantInterval::valueWithMode( int i, ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  switch ( mode )
  {
    case ReosTimeSerieConstantInterval::Value:
      return mValues.at( i );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      return mValues.at( i ) / mTimeStep->value().valueUnit( mIntensityTimeUnit );
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
  double min = 0;
  double max = 0;

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

ReosEncodedElement ReosTimeSerieConstantInterval::encode( const QString &descritpion ) const
{
  QString descript = descritpion;
  if ( descript.isEmpty() )
    descript = QStringLiteral( "time-serie-constant-interval" );

  ReosEncodedElement element( descript );
  ReosTimeSerie::baseEncode( element );
  element.addEncodedData( QStringLiteral( "time-step" ), mTimeStep->encode() );

  return element;
}

ReosTimeSerieConstantInterval *ReosTimeSerieConstantInterval::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "time-serie-constant-interval" ) )
    return nullptr;

  return new ReosTimeSerieConstantInterval( element, parent );
}


ReosTimeSerieConstantInterval::ReosTimeSerieConstantInterval( const ReosEncodedElement &element, QObject *parent ): ReosTimeSerie( parent )
{
  decodeBase( element );
  mTimeStep = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ), false, tr( "Time step" ), this );
  connectParameters();
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

double ReosTimeSerieConstantInterval::convertFromIntensityValue( double v )
{
  return v * mTimeStep->value().valueUnit( mIntensityTimeUnit );
}

ReosDuration::Unit ReosTimeSerieConstantInterval::intensityTimeUnit() const
{
  return mIntensityTimeUnit;
}

void ReosTimeSerieConstantInterval::setIntensityTimeUnit( const ReosDuration::Unit &intensityTimeUnit )
{
  mIntensityTimeUnit = intensityTimeUnit;
  emit dataChanged();
  emit settingsChanged();
}

bool ReosTimeSerieConstantInterval::addCumultive() const
{
  return mAddCumulative;
}

void ReosTimeSerieConstantInterval::setAddCumulative( bool addCumulative )
{
  mAddCumulative = addCumulative;
}

void ReosTimeSerieConstantInterval::syncWith( ReosTimeSerieConstantInterval *other )
{
  connect( other, &ReosDataObject::dataChanged, this, [this, other]
  {
    copyAttribute( other );
  } );
}

void ReosTimeSerieConstantInterval::copyAttribute( ReosTimeSerieConstantInterval *other )
{
  timeStep()->setValue( other->timeStep()->value() );
  referenceTime()->setValue( other->referenceTime()->value() );
  setValueMode( other->valueMode() );
  setValueUnit( other->valueUnit() );
  setIntensityTimeUnit( other->intensityTimeUnit() );
  setAddCumulative( other->addCumultive() );
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

QString ReosTimeSerieConstantInterval::unitStringCurrentMode() const
{
  switch ( valueMode() )
  {
    case ReosTimeSerieConstantInterval::Value:
    case ReosTimeSerieConstantInterval::Cumulative:
      return  valueModeName( valueMode() ) + QStringLiteral( " (%1)" ).arg( valueUnit() );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      return tr( "Intensity (%1/%2)" ).arg( valueUnit(), ReosDuration().unitToString( intensityTimeUnit() ) );
      break;
  }

  return QString();
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

void ReosTimeSerie::clear()
{
  mValues.clear();
  emit dataChanged();
}

QPair<double, double> ReosTimeSerie::valueExent( bool withZero ) const
{
  if ( mValues.isEmpty() )
    return QPair<double, double>( std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN() );
  double min;
  double max;
  if ( withZero )
  {
    min = 0;
    max = 0;
  }
  else
  {
    min = std::numeric_limits<double>::max();
    max = -std::numeric_limits<double>::max();
  }

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
                                        false,
                                        tr( "Reference time" ),
                                        this );
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


ReosTimeSerieVariableTimeStep::ReosTimeSerieVariableTimeStep( QObject *parent ): ReosTimeSerie( parent ) {}

ReosDuration ReosTimeSerieVariableTimeStep::relativeTimeAt( int i ) const
{
  return mTimeValues.at( i );
}

QPair<QDateTime, QDateTime> ReosTimeSerieVariableTimeStep::timeExtent() const
{
  if ( mTimeValues.isEmpty() )
    return {referenceTime()->value(), referenceTime()->value() };

  QDateTime refTime = referenceTime()->value();
  return {refTime.addMSecs( mTimeValues.first().valueMilliSecond() ), refTime.addMSecs( mTimeValues.last().valueMilliSecond() )};
}

void ReosTimeSerieVariableTimeStep::setValue( const ReosDuration &relativeTime, double value )
{
  if ( mTimeValues.isEmpty() || relativeTime > mTimeValues.last() )
  {
    mValues.append( value );
    mTimeValues.append( relativeTime );
    return;
  }

  if ( relativeTime < mTimeValues.first() )
  {
    mValues.prepend( value );
    mTimeValues.prepend( relativeTime );
    return;
  }

  bool exact = false;
  int index = timeValueIndex( relativeTime, exact );

  if ( exact )
    mValues[index] = value;
  else
  {
    mValues.insert( index + 1, value );
    mTimeValues.insert( index + 1, relativeTime );
  }

  emit dataChanged();
}

double ReosTimeSerieVariableTimeStep::valueAtTime( const ReosDuration &relativeTime ) const
{
  bool exact = false;
  int index = timeValueIndex( relativeTime, exact );

  if ( exact )
    return mValues.at( index );

  if ( index < 0 || index >= mValues.count() - 1 )
    return 0;

  const ReosDuration time1 = mTimeValues.at( index );
  const ReosDuration time2 = mTimeValues.at( index + 1 );

  double ratio = ( relativeTime - time1 ) / ( time2 - time1 );

  return ( mValues.at( index + 1 ) - mValues.at( index ) ) * ratio + mValues.at( index );
}

void ReosTimeSerieVariableTimeStep::addOther( const ReosTimeSerieVariableTimeStep &other, double factor )
{
  blockSignals( true );

  bool allowInterpolation = false;

  ReosDuration offset( referenceTime()->value().msecsTo( other.referenceTime()->value() ), ReosDuration::millisecond );

  //need to store apart and then apply, if not changed value will disturb the addiion for following
  QVector<double> newValue_1( mTimeValues.count() );
  for ( int i = 0; i < mTimeValues.count(); ++i )
  {
    ReosDuration thisTimeValue = mTimeValues.at( i );
    newValue_1[i] = mValues.at( i ) + factor * other.valueAtTime( thisTimeValue - offset );
    //setValue( thisTimeValue, mValues.at( i ) + factor * other.valueAtTime( thisTimeValue - offset ) );
  }

  // now add time steps not existing in this instance,
  QMap<ReosDuration, double> newValue_2;
  for ( int i = 0; i < other.mTimeValues.count(); ++i )
  {
    bool exact = false;
    ReosDuration otherTimeValue = other.mTimeValues.at( i ) + offset;
    int index = timeValueIndex( otherTimeValue, exact );
    if ( !exact )
    {
      if ( index < 0 || index >= ( mTimeValues.count() - 1 ) || allowInterpolation )
        newValue_2[otherTimeValue] = valueAtTime( otherTimeValue ) + factor * other.valueAt( i );
    }
  }

  // Then apply the value
  for ( int i = 0; i < mTimeValues.count(); ++i )
    setValueAt( i, newValue_1.at( i ) );

  const QList<ReosDuration> &keys = newValue_2.keys();
  for ( const ReosDuration &key : keys )
  {
    setValue( key, newValue_2.value( key ) );
  }

  blockSignals( false );

  emit dataChanged();

}

int ReosTimeSerieVariableTimeStep::timeValueIndex( const ReosDuration &time, bool &exact ) const
{
  if ( mTimeValues.empty() || time < mTimeValues.first() )
  {
    exact = false;
    return -1;
  }

  if ( time > mTimeValues.last() )
  {
    exact = false;
    return mTimeValues.count() - 1;
  }

  int i1 = 0;
  int i2 = mTimeValues.count() - 1;
  while ( true )
  {
    if ( mTimeValues.at( i1 ) == time )
    {
      exact = true;
      return i1;
    }
    if ( mTimeValues.at( i2 ) == time )
    {
      exact = true;
      return i2;
    }

    if ( i1 == i2 || i1 + 1 == i2 )
    {
      exact = false;
      return i1;
    }

    int inter = ( i1 + i2 ) / 2;
    if ( time < mTimeValues.at( inter ) )
      i2 = inter;
    else
      i1 = inter;
  }
}
