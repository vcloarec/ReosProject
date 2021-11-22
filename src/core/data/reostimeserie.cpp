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


ReosTimeSerieConstantIntervalModel::ReosTimeSerieConstantIntervalModel( QObject *parent ): ReosTimeSerieModel( parent )
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

int ReosTimeSerieConstantIntervalModel::rowCount( const QModelIndex & ) const
{
  if ( mData )
    return isEditable() ? mData->valueCount() + 1 : mData->valueCount();
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

  int maxRowCount = isEditable() ? mData->valueCount() + 1 : mData->valueCount();

  if ( !index.isValid() || index.row() >= maxRowCount )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
    case Qt::EditRole:
      if ( index.row() < mData->valueCount() )
        return ReosParameter::doubleToString( mData->valueAt( index.row() ), 2 );

      if ( isEditable() )
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
  if ( !index.isValid() || !isEditable() || index.row() > mData->valueCount() )
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

  int maxRowCount = isEditable() ? mData->valueCount() + 1 : mData->valueCount();

  if ( section < maxRowCount )
  {
    if ( mData->referenceTime()->value().isValid() )
    {
      return mData->timeAt( section ).toString( QStringLiteral( "yyyy.MM.dd HH:mm:ss" ) );
    }
    else
      return mData->relativeTimeAt( section ).valueSecond();
  }

  return QVariant();
}

Qt::ItemFlags ReosTimeSerieConstantIntervalModel::flags( const QModelIndex &index ) const
{
  if ( isEditable() )
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

void ReosTimeSerieConstantIntervalModel::setValues( const QModelIndex &fromIndex, const QList<QVariantList> &values )
{
  if ( !fromIndex.isValid() )
    return;

  setValues( fromIndex, doubleFromVariant( values ) );
}

void ReosTimeSerieConstantIntervalModel::insertValues( const QModelIndex &fromIndex, const QList<QVariantList> &values )
{
  const QList<double> doubleValues = doubleFromVariant( values );
  if ( doubleValues.isEmpty() )
    return;

  insertRows( fromIndex, doubleValues.count() );
  setValues( fromIndex, doubleValues );
}

bool ReosTimeSerieConstantIntervalModel::isEditable() const
{
  if ( mData && mData->dataProvider() )
    return mData->dataProvider()->isEditable();

  return false;
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

QList<double> ReosTimeSerieConstantIntervalModel::doubleFromVariant( const QList<QVariantList> &values )
{
  QList<double> doubleValues;
  doubleValues.reserve( values.count() );

  for ( const QVariantList &row : values )
  {
    if ( row.count() != 1 )
      return QList<double>();
    bool ok = false;
    doubleValues.append( ReosParameter::stringToDouble( row.at( 0 ).toString(), &ok ) );
    if ( !ok )
      return QList<double>();
  }

  return doubleValues;
}

void ReosTimeSerieConstantIntervalModel::deleteRows( const QModelIndex &fromIndex, int count )
{
  if ( !fromIndex.isValid() )
    return;
  int maxCount = std::min( count, mData->valueCount() - fromIndex.row() );
  beginRemoveRows( QModelIndex(), fromIndex.row(), fromIndex.row() + maxCount - 1 );
  mData->removeValues( fromIndex.row(), count );
  endRemoveRows();
}

void ReosTimeSerieConstantIntervalModel::insertRows( const QModelIndex &fromIndex, int count )
{
  beginInsertRows( QModelIndex(), fromIndex.row(), fromIndex.row() + count - 1 );
  mData->insertValues( fromIndex.row(), count, defaultValue );
  endInsertRows();
}

ReosTimeSerieConstantInterval::ReosTimeSerieConstantInterval( QObject *parent, const QString &providerKey, const QString &dataSource ):
  ReosTimeSerie( parent, providerKey, dataSource )
  , mTimeStep( new ReosParameterDuration( tr( "Time step" ), this ) )
{
  if ( constantTimeStepDataProvider() )
  {
    ReosDuration timeStep = constantTimeStepDataProvider()->timeStep();
    timeStep.setAdaptedUnit();
    mTimeStep->setValue( timeStep );
  }
  else
  {
    mProvider.reset( static_cast<ReosTimeSerieProvider *>( ReosDataProviderRegistery::instance()->createProvider( QStringLiteral( "constant-time-step-memory" ) ) ) );
    if ( mProvider )
      connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSerie::onDataProviderChanged );
  }

  if ( mTimeStep->value() == ReosDuration() )
  {
    mTimeStep->setValue( ReosDuration( 5, ReosDuration::minute ) );
  }

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
  if ( mProvider->valueCount() > 0 )
    ret.first = timeAt( 0 );
  else
    ret.first = referenceTime()->value();

  if ( mProvider->valueCount() > 1 )
    ret.second = timeAt( mProvider->valueCount() - 1 ).addMSecs( mTimeStep->value().valueMilliSecond() );
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
  ReosTimeSerieConstantTimeStepProvider *dataValues = constantTimeStepDataProvider();

  if ( !dataValues || !dataValues->isEditable() )
    return;

  switch ( mValueMode )
  {
    case ReosTimeSerieConstantInterval::Value:
      dataValues->appendValue( value );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      dataValues->appendValue( convertFromIntensityValue( value ) );
      break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      if ( dataValues->valueCount() > 0 )
      {
        double cumulValueBefore = valueWithMode( mProvider->valueCount() - 1 );
        dataValues->appendValue( value - cumulValueBefore );
      }
      else
      {
        dataValues->appendValue( value );
      }
    }
    break;
  }

  emit dataChanged();
}

void ReosTimeSerieConstantInterval::insertValues( int fromPos, int count, double value )
{
  ReosTimeSerieConstantTimeStepProvider *dataValues = constantTimeStepDataProvider();

  if ( !dataValues || !dataValues->isEditable() )
    return;

  switch ( mValueMode )
  {
    case ReosTimeSerieConstantInterval::Value:
      for ( int i = 0; i < count; ++i )
        dataValues->insertValue( fromPos, value );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
    {
      double intensity = convertFromIntensityValue( value );
      for ( int i = 0; i < count; ++i )
        dataValues->insertValue( fromPos, convertFromIntensityValue( intensity ) );
    }
    break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      double cumulValueBefore = valueWithMode( mProvider->valueCount() - 1 );
      if ( fromPos > 0 )
        dataValues->insertValue( fromPos, value - cumulValueBefore );
      if ( count > 1 )
        for ( int i = 0; i < count - 1; ++i )
          dataValues->insertValue( fromPos + 1,  0.0 );
    }
    break;
  }
}

double ReosTimeSerieConstantInterval::valueWithMode( int i, ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  updateData();
  switch ( mode )
  {
    case ReosTimeSerieConstantInterval::Value:
      return mProvider->value( i );
      break;
    case ReosTimeSerieConstantInterval::Intensity:
      return mProvider->value( i ) / mTimeStep->value().valueUnit( mIntensityTimeUnit );
      break;
    case ReosTimeSerieConstantInterval::Cumulative:
    {
      double sum = 0;
      for ( int v = 0; v < i; ++v )
        sum += mProvider->value( v );
      return sum;
    }
    break;
  }

  return 0;
}

QPair<double, double> ReosTimeSerieConstantInterval::extentValueWithMode( ReosTimeSerieConstantInterval::ValueMode mode ) const
{
  updateData();
  if ( mProvider->valueCount() == 0 )
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
  if ( !mProvider || !mProvider->isEditable() )
    return;

  if ( i < mProvider->valueCount() )
  {
    mProvider->setValue( i, value );
    emit dataChanged();
  }
}

ReosEncodedElement ReosTimeSerieConstantInterval::encode( const QString &descritpion ) const
{
  updateData();
  QString descript = descritpion;
  if ( descript.isEmpty() )
    descript = QStringLiteral( "time-serie-constant-interval" );

  if ( constantTimeStepDataProvider()->isEditable() )
    constantTimeStepDataProvider()->setTimeStep( mTimeStep->value() );

  ReosEncodedElement element( descript );
  ReosTimeSerie::baseEncode( element );

  return element;
}

ReosTimeSerieConstantInterval *ReosTimeSerieConstantInterval::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "time-serie-constant-interval" ) )
    return nullptr;

  return new ReosTimeSerieConstantInterval( element, parent );
}

ReosTimeSerieConstantTimeStepProvider *ReosTimeSerieConstantInterval::constantTimeStepDataProvider() const
{
  return static_cast<ReosTimeSerieConstantTimeStepProvider *>( mProvider.get() );
}

void ReosTimeSerieConstantInterval::copyFrom( ReosTimeSerieConstantInterval *other )
{
  if ( !other || !constantTimeStepDataProvider() || !constantTimeStepDataProvider()->isEditable() )
    return;

  constantTimeStepDataProvider()->copy( other->constantTimeStepDataProvider() );
}

ReosTimeSerieConstantInterval::ReosTimeSerieConstantInterval( const ReosEncodedElement &element, QObject *parent )
  : ReosTimeSerie( parent )
{
  decodeBase( element );

  if ( constantTimeStepDataProvider() )
  {
    constantTimeStepDataProvider()->decode( element.getEncodedData( QStringLiteral( "data-provider" ) ) );
    mTimeStep = new ReosParameterDuration( tr( "Time step" ), false, this );
    mTimeStep->setValue( constantTimeStepDataProvider()->timeStep() );
  }
  else
  {
    //set default one as memory
    QVector<double> values;
    element.getData( QStringLiteral( "values" ), values ); //before Lekan 2.2, values were store in this element
    mProvider = std::make_unique < ReosTimeSerieConstantTimeStepMemoryProvider>( values );
    mTimeStep = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ), false, tr( "Time step" ), this );
  }

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
  if ( mProvider )
    mTimeStep->setEditable( mProvider->isEditable() );

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

double *ReosTimeSerie::data()
{
  updateData();
  return mProvider->data();
}

const QVector<double> &ReosTimeSerie::constData() const
{
  updateData();
  return  mProvider->constData();
}

ReosTimeSerieProvider *ReosTimeSerie::dataProvider() const
{
  return mProvider.get();
}

void ReosTimeSerie::onDataProviderChanged()
{
  if ( mProvider )
    mReferenceTime->setValue( mProvider->referenceTime() );

  emit dataChanged();
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

ReosTimeSerie::ReosTimeSerie( QObject *parent, const QString &providerKey, const QString &dataSource ):
  ReosDataObject( parent )
  , mReferenceTime( new ReosParameterDateTime( tr( "Reference time" ), this ) )
{
  if ( !providerKey.isEmpty() )
    mProvider.reset( static_cast<ReosTimeSerieProvider *>( ReosDataProviderRegistery::instance()->createProvider( providerKey ) ) );

  if ( mProvider )
  {
    connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSerie::onDataProviderChanged );
    mProvider->setDataSource( dataSource );
  }
}

int ReosTimeSerie::valueCount() const
{
  updateData();
  return mProvider->valueCount();
}

QDateTime ReosTimeSerie::timeAt( int i ) const
{
  updateData();
  if ( mReferenceTime->value().isValid() )
    return mReferenceTime->value().addMSecs( relativeTimeAt( i ).valueMilliSecond() );
  else
    return QDateTime();
}

double ReosTimeSerie::valueAt( int i ) const
{
  updateData();
  return mProvider->value( i );
}

void ReosTimeSerie::removeValues( int fromPos, int count )
{
  mProvider->removeValues( fromPos, count );
  emit dataChanged();
}

void ReosTimeSerie::clear()
{
  mProvider->clear();
  emit dataChanged();
}

QPair<double, double> ReosTimeSerie::valueExent( bool withZero ) const
{
  updateData();
  if ( mProvider->valueCount() == 0 )
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
  updateData();
  ReosDataObject::encode( element );
  if ( mProvider )
  {
    if ( mProvider->isEditable() )
      mProvider->setReferenceTime( mReferenceTime->value() );
    element.addData( QStringLiteral( "provider-key" ), mProvider->key() );
    element.addEncodedData( QStringLiteral( "provider-data" ), mProvider->encode() );
  }
}

bool ReosTimeSerie::decodeBase( const ReosEncodedElement &element )
{
  ReosDataObject::decode( element );

  mProvider.reset();
  if ( element.hasEncodedData( QStringLiteral( "provider-key" ) ) )
  {
    QString providerKey;
    element.getData( QStringLiteral( "provider-key" ), providerKey );
    mProvider.reset( static_cast<ReosTimeSerieProvider *>( ReosDataProviderRegistery::instance()->createProvider( providerKey ) ) );
    if ( mProvider && element.hasEncodedData( QStringLiteral( "provider-data" ) ) )
    {
      connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSerie::onDataProviderChanged );
      mProvider->decode( element.getEncodedData( QStringLiteral( "provider-data" ) ) );
      mReferenceTime->setValue( mProvider->referenceTime() );
      mReferenceTime->setEditable( mProvider->isEditable() );
    }
  }
  else
  {
    if ( element.hasEncodedData( QStringLiteral( "reference-time" ) ) )
    {
      //no provider and we has a  reference time encoded, so old version < 2.2 ->take this one
      mReferenceTime->deleteLater();
      mReferenceTime = ReosParameterDateTime::decode(
                         element.getEncodedData( QStringLiteral( "reference-time" ) ), false, tr( "Reference time" ), this );
    }
  }

  return true;
}

void ReosTimeSerie::connectParameters()
{
  if ( mProvider )
    mReferenceTime->setEditable( mProvider->isEditable() );
  connect( mReferenceTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}


ReosTimeSerieVariableTimeStep::ReosTimeSerieVariableTimeStep(
  QObject *parent,
  const QString &providerKey,
  const QString &dataSource )
  : ReosTimeSerie( parent, providerKey, dataSource )
{
  if ( !variableTimeStepdataProvider() )
  {
    mProvider.reset( new ReosTimeSerieVariableTimeStepMemoryProvider( ) );
    connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSerie::onDataProviderChanged );
  }
  connectParameters();
}

ReosDuration ReosTimeSerieVariableTimeStep::relativeTimeAt( int i ) const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();
  if ( !dataProv )
    return ReosDuration();

  return dataProv->relativeTimeAt( i );
}

bool ReosTimeSerieVariableTimeStep::setRelativeTimeAt( int i, const ReosDuration &relativeTime )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();

  if ( !dataProv || !dataProv->isEditable() )
    return false;

  if ( i > 0 && i < dataProv->valueCount() )
  {
    if ( ( i > 1 &&  dataProv->relativeTimeAt( i - 1 ) >= relativeTime ) ||
         ( i < dataProv->valueCount() - 1 && dataProv->relativeTimeAt( i + 1 ) <= relativeTime ) )
      return false;

    dataProv->setRelativeTimeAt( i, relativeTime );
    return true;
  }

  return false;
}

QPair<QDateTime, QDateTime> ReosTimeSerieVariableTimeStep::timeExtent() const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();

  if ( !dataProv )
    return QPair<QDateTime, QDateTime>();

  if ( dataProv->valueCount() == 0 )
    return {referenceTime()->value(), referenceTime()->value() };

  QDateTime refTime = referenceTime()->value();

  return {refTime.addMSecs( dataProv->relativeTimeAt( 0 ).valueMilliSecond() ), refTime.addMSecs( dataProv->lastRelativeTime().valueMilliSecond() )};
}

void ReosTimeSerieVariableTimeStep::setValue( const ReosDuration &relativeTime, double value )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();

  if ( !dataProv || !dataProv->isEditable() )
    return;

  if ( dataProv->valueCount() == 0 || relativeTime > dataProv->lastRelativeTime() )
  {
    dataProv->appendValue( relativeTime, value );
    emit dataChanged();
    return;
  }

  if ( relativeTime < dataProv->relativeTimeAt( 0 ) )
  {
    dataProv->prependValue( relativeTime, value );
    emit dataChanged();
    return;
  }

  bool exact = false;
  int index = timeValueIndex( relativeTime, exact );

  if ( exact )
    mProvider->setValue( index, value );
  else
  {
    dataProv->insertValue( index + 1, relativeTime, value );
  }

  emit dataChanged();
}

double ReosTimeSerieVariableTimeStep::valueAtTime( const ReosDuration &relativeTime ) const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();

  if ( !dataProv )
    return 0;

  bool exact = false;
  int index = timeValueIndex( relativeTime, exact );

  if ( exact )
    return dataProv->value( index );

  if ( index < 0 || index >= dataProv->valueCount() - 1 )
    return 0;

  const ReosDuration time1 = dataProv->relativeTimeAt( index );
  const ReosDuration time2 = dataProv->relativeTimeAt( index + 1 );

  double ratio = ( relativeTime - time1 ) / ( time2 - time1 );

  return ( dataProv->value( index + 1 ) - dataProv->value( index ) ) * ratio + dataProv->value( index );
}

double ReosTimeSerieVariableTimeStep::valueAtTime( const QDateTime &time ) const
{
  return valueAtTime( ReosDuration( referenceTime()->value().msecsTo( time ) ) );
}

void ReosTimeSerieVariableTimeStep::addOther( const ReosTimeSerieVariableTimeStep *other, double factor, bool allowInterpolation )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();
  ReosTimeSerieVariableTimeStepProvider *otherDataProv = other->variableTimeStepdataProvider();

  if ( !dataProv || !otherDataProv || !dataProv->isEditable() )
    return;

  blockSignals( true );

  ReosDuration offset( referenceTime()->value().msecsTo( other->referenceTime()->value() ), ReosDuration::millisecond );

  //need to store apart and then apply, if not changed value will disturb the addiion for following
  QVector<double> newValue_1( dataProv->valueCount() );
  for ( int i = 0; i < dataProv->valueCount(); ++i )
  {
    ReosDuration thisTimeValue = dataProv->relativeTimeAt( i );
    newValue_1[i] = mProvider->value( i ) + factor * other->valueAtTime( thisTimeValue - offset );
    //setValue( thisTimeValue, mValues.at( i ) + factor * other.valueAtTime( thisTimeValue - offset ) );
  }

  // now add time steps not existing in this instance,
  QMap<ReosDuration, double> newValue_2;
  for ( int i = 0; i < otherDataProv->valueCount(); ++i )
  {
    bool exact = false;
    ReosDuration otherTimeValue = otherDataProv->relativeTimeAt( i ) + offset;
    int index = timeValueIndex( otherTimeValue, exact );
    if ( !exact )
    {
      if ( index < 0 || index >= ( dataProv->valueCount() - 1 ) || allowInterpolation )
        newValue_2[otherTimeValue] = valueAtTime( otherTimeValue ) + factor * other->valueAt( i );
    }
  }

  // Then apply the value
  for ( int i = 0; i < dataProv->valueCount(); ++i )
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
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepdataProvider();

  if ( !dataProv )
    return 0;

  if ( dataProv->valueCount() == 0 || time < dataProv->relativeTimeAt( 0 ) )
  {
    exact = false;
    return -1;
  }

  if ( time > dataProv->lastRelativeTime() )
  {
    exact = false;
    return dataProv->valueCount() - 1;
  }

  int i1 = 0;
  int i2 = dataProv->valueCount() - 1;
  while ( true )
  {
    if ( dataProv->relativeTimeAt( i1 ) == time )
    {
      exact = true;
      return i1;
    }
    if ( dataProv->relativeTimeAt( i2 ) == time )
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
    if ( time < dataProv->relativeTimeAt( inter ) )
      i2 = inter;
    else
      i1 = inter;
  }
}

QString ReosTimeSerieVariableTimeStep::unitString() const
{
  return mUnitString;
}

void ReosTimeSerieVariableTimeStep::setUnitString( const QString &unitString )
{
  mUnitString = unitString;
}

QColor ReosTimeSerieVariableTimeStep::color() const
{
  return mColor;
}

void ReosTimeSerieVariableTimeStep::setColor( const QColor &color )
{
  mColor = color;

  emit colorChanged( color );
}

void ReosTimeSerieVariableTimeStep::baseEncode( ReosEncodedElement &element ) const
{
  ReosTimeSerie::baseEncode( element );

  element.addData( QStringLiteral( "unit-string" ), mUnitString );
  element.addData( QStringLiteral( "color" ), mColor );

}

bool ReosTimeSerieVariableTimeStep::decodeBase( const ReosEncodedElement &element )
{
  ReosTimeSerie::decodeBase( element );

  if ( !element.getData( QStringLiteral( "unit-string" ), mUnitString ) )
    return false;
  element.getData( QStringLiteral( "color" ), mColor );


  if ( !mProvider )
  {
    //set default one as memory
    QVector<double> values;
    QVector<ReosDuration> timeValues;
    element.getData( QStringLiteral( "values" ), values ); //before Lekan 2.2, values were store in this element
    QList<QByteArray> encodedTimeValues;

    element.getData( QStringLiteral( "time-values" ), encodedTimeValues );
    if ( encodedTimeValues.count() != values.count() )
      values.clear();
    else
    {
      timeValues.resize( encodedTimeValues.count() );
      for ( int i = 0; i < encodedTimeValues.size(); ++i )
        timeValues[i] = ReosDuration::decode( ReosEncodedElement( encodedTimeValues.at( i ) ) );
    }

    mProvider = std::make_unique < ReosTimeSerieVariableTimeStepMemoryProvider>( values, timeValues );
  }

  return true;
}

ReosTimeSerieVariableTimeStepProvider *ReosTimeSerieVariableTimeStep::variableTimeStepdataProvider() const
{
  return static_cast<ReosTimeSerieVariableTimeStepProvider *>( mProvider.get() );
}

ReosTimeSerieVariableTimeStepModel::ReosTimeSerieVariableTimeStepModel( QObject *parent ): ReosTimeSerieModel( parent )
{
  mFixedTimeStep = ReosDuration( 5, ReosDuration::minute );
}

int ReosTimeSerieVariableTimeStepModel::rowCount( const QModelIndex & ) const
{
  if ( !mData.isNull() )
    return isEditable() ? mData->valueCount() + 1 : mData->valueCount();
  else
    return 3;
}

int ReosTimeSerieVariableTimeStepModel::columnCount( const QModelIndex & ) const
{
  return valueColumn() + 1;
}

QVariant ReosTimeSerieVariableTimeStepModel::data( const QModelIndex &index, int role ) const
{
  if ( mData.isNull() )
    return QVariant();

  int maxRowCount = isEditable() ? mData->valueCount() + 1 : mData->valueCount();

  if ( !index.isValid() || index.row() >= maxRowCount )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
    case Qt::EditRole:
      if ( index.column() == 0 )
      {
        if ( index.row() < mData->valueCount() )
          return mData->timeAt( index.row() );
      }
      else if ( index.column() == valueColumn() )
      {
        if ( index.row() < mData->valueCount() )
          return mData->valueAt( index.row() );
      }
      else if ( !mNewRowWithFixedTimeStep && index.column() == 1 )
      {
        if ( index.row() < mData->valueCount() )
          return mData->relativeTimeAt( index.row() ).toString( mVariableTimeStepUnit, 2 );
      }

      if ( isEditable() )
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

bool ReosTimeSerieVariableTimeStepModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !index.isValid() || !isEditable() || index.row() > mData->valueCount() )
    return false;

  if ( role == Qt::EditRole )
  {
    bool ok = false;
    double v = value.toString().toDouble( &ok );
    if ( ok )
    {
      if ( index.row() == mData->valueCount() ) //insert at the end
      {
        if ( !mNewRowWithFixedTimeStep && index.column() == valueColumn() - 1 )
        {

          ReosDuration relativeTime( value.toDouble(), mVariableTimeStepUnit );
          if ( mData->valueCount() == 0 || relativeTime > mData->relativeTimeAt( mData->valueCount() - 1 ) )
          {
            beginInsertRows( QModelIndex(), index.row() + 1, index.row() + 1 );
            mData->setValue( ReosDuration( relativeTime ), 0 );
            endInsertRows();
          }
        }
        else
        {
          beginInsertRows( QModelIndex(), index.row() + 1, index.row() + 1 );
          if ( mNewRowWithFixedTimeStep )
          {
            ReosDuration previousrelativeTime;
            if ( index.row() > 0 )
              previousrelativeTime = mData->relativeTimeAt( index.row() - 1 );
            mData->setValue( previousrelativeTime + mFixedTimeStep, v );
          }
          else if ( index.column() == valueColumn() )
          {
            ReosDuration relativeTime( 0, mVariableTimeStepUnit );
            if ( index.row() == 1 )
              relativeTime = mFixedTimeStep + mData->relativeTimeAt( 0 );
            else if ( index.row() > 1 )
            {
              int dataCount = mData->valueCount();
              relativeTime = mData->relativeTimeAt( dataCount - 1 ) * 2 - mData->relativeTimeAt( dataCount - 2 );
            }
            mData->setValue( relativeTime, v );
          }
          endInsertRows();
        }
      }
      else
      {
        if ( index.column() == valueColumn() )
          mData->setValueAt( index.row(), v );

        if ( !mNewRowWithFixedTimeStep )
        {
          if ( index.column() == 1 )
          {
            ReosDuration relativeTime( v, mVariableTimeStepUnit );
            mData->setRelativeTimeAt( index.row(), relativeTime );
          }
        }
      }

      return true;
    }
  }
  return false;
}

Qt::ItemFlags ReosTimeSerieVariableTimeStepModel::flags( const QModelIndex &index ) const
{
  if ( isEditable() && !mNewRowWithFixedTimeStep && index.column() > 0 )
    return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;
  else if ( isEditable() && index.column() == valueColumn() )
    return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;

  return QAbstractTableModel::flags( index );
}

QVariant ReosTimeSerieVariableTimeStepModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Horizontal )
  {
    if ( role == Qt::DisplayRole )
    {
      if ( section == valueColumn() )
        return mData->unitString();
      else if ( section == 0 )
        return tr( "Time" );
      else if ( ! mNewRowWithFixedTimeStep && section == 1 )
        return tr( "Relative time" );
    }

    return QVariant();
  }

  return QVariant();
}

void ReosTimeSerieVariableTimeStepModel::setSerie( ReosTimeSerieVariableTimeStep *serie )
{
  if ( mData )
    //disconnect( mData->referenceTime(), &ReosParameter::valueChanged, this, &ReosTimeSerieVariableTimeStepModel::updateModel );
    disconnect( mData, &ReosDataObject::dataChanged, this, &ReosTimeSerieVariableTimeStepModel::updateModel );

  beginResetModel();
  mData = serie;
  endResetModel();

  if ( mData )
    //connect( mData->referenceTime(), &ReosParameter::valueChanged, this, &ReosTimeSerieVariableTimeStepModel::updateModel );
    connect( mData, &ReosDataObject::dataChanged, this, &ReosTimeSerieVariableTimeStepModel::updateModel );
}

void ReosTimeSerieVariableTimeStepModel::setNewRowWithFixedTimeStep( bool newRowWithFixedTimeStep )
{
  beginResetModel();
  mNewRowWithFixedTimeStep = newRowWithFixedTimeStep;
  endResetModel();
}

void ReosTimeSerieVariableTimeStepModel::setFixedTimeStep( const ReosDuration &fixedTimeStep )
{
  mFixedTimeStep = fixedTimeStep;
}

void ReosTimeSerieVariableTimeStepModel::setVariableTimeStepUnit( const ReosDuration::Unit &variableTimeStepUnit )
{
  beginResetModel();
  mVariableTimeStepUnit = variableTimeStepUnit;
  endResetModel();
}

bool ReosTimeSerieVariableTimeStepModel::isEditable() const
{
  if ( mData && mData->dataProvider() )
    return mData->dataProvider()->isEditable();

  return false;
}

void ReosTimeSerieVariableTimeStepModel::updateModel()
{
  beginResetModel();
  endResetModel();
}

int ReosTimeSerieVariableTimeStepModel::valueColumn() const
{
  if ( mNewRowWithFixedTimeStep )
    return 1;
  else
    return 2;
}
