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
#include "reostimeseries.h"

#include <QLocale>
#include <QEventLoop>
#include <QTimer>

#include "qdebug.h"
#include "reostimeseriesprovider.h"

ReosTimeSeriesConstantIntervalModel::ReosTimeSeriesConstantIntervalModel( QObject *parent ): ReosTimeSerieModel( parent )
{

}

ReosTimeSerieModel::ReosTimeSerieModel( QObject *parent ): QAbstractTableModel( parent )
{}

QModelIndex ReosTimeSerieModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTimeSerieModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

QList<int> ReosTimeSerieModel::editableColumn() const
{
  QList<int> ret;
  for ( int i = 0; i < columnCount( QModelIndex() ); ++i )
  {
    if ( flags( index( 0, i, QModelIndex() ) )&Qt::ItemIsEditable )
      ret.append( i );
  }

  return ret;
}

int ReosTimeSeriesConstantIntervalModel::rowCount( const QModelIndex & ) const
{
  if ( mData )
    return isEditable() ? mData->valueCount() + 1 : mData->valueCount();
  else
    return 0;
}

int ReosTimeSeriesConstantIntervalModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosTimeSeriesConstantIntervalModel::data( const QModelIndex &index, int role ) const
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

bool ReosTimeSeriesConstantIntervalModel::setData( const QModelIndex &index, const QVariant &value, int role )
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

QVariant ReosTimeSeriesConstantIntervalModel::headerData( int section, Qt::Orientation orientation, int role ) const
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
    if ( mData->referenceTime().isValid() )
    {
      return mData->timeAt( section ).toString( QLocale().dateTimeFormat( QLocale::ShortFormat ) );;
    }
    else
      return mData->relativeTimeAt( section ).valueSecond();
  }

  return QVariant();
}

Qt::ItemFlags ReosTimeSeriesConstantIntervalModel::flags( const QModelIndex &index ) const
{
  if ( isEditable() )
    return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;
  else
    return QAbstractTableModel::flags( index );
}

void ReosTimeSeriesConstantIntervalModel::setSerieData( ReosTimeSeriesConstantInterval *data )
{
  mData = data;
  connect( data->timeStepParameter(), &ReosParameter::valueChanged, this, [this]
  {
    emit headerDataChanged( Qt::Vertical, 0, rowCount( QModelIndex() ) ) ;
  } );

  connect( data->timeStepParameter(), &ReosParameter::unitChanged, this, [this]
  {
    emit headerDataChanged( Qt::Horizontal, 0, 1 ) ;
  } );

  connect( data, &ReosDataObject::dataChanged, this, [this]
  {
    emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 0, QModelIndex() ) );
    emit headerDataChanged( Qt::Horizontal, 0, 1 ) ;
  } );
}

void ReosTimeSeriesConstantIntervalModel::setValues( const QModelIndex &fromIndex, const QList<QVariantList> &values )
{
  if ( !fromIndex.isValid() )
    return;

  setValues( fromIndex, doubleFromVariantList( values ) );
}

void ReosTimeSeriesConstantIntervalModel::insertValues( const QModelIndex &fromIndex, const QList<QVariantList> &values )
{
  const QList<double> doubleValues = doubleFromVariantList( values );
  if ( doubleValues.isEmpty() )
    return;

  insertRows( fromIndex, doubleValues.count() );
  setValues( fromIndex, doubleValues );
}

bool ReosTimeSeriesConstantIntervalModel::isEditable() const
{
  if ( mData && mData->dataProvider() )
    return mData->dataProvider()->isEditable();

  return false;
}

void ReosTimeSeriesConstantIntervalModel::setValues( const QModelIndex &fromIndex, const QList<double> &values )
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

QList<double> ReosTimeSeriesConstantIntervalModel::doubleFromVariantList( const QList<QVariantList> &values )
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

void ReosTimeSeriesConstantIntervalModel::deleteRows( const QModelIndex &fromIndex, int count )
{
  if ( !fromIndex.isValid() )
    return;
  int maxCount = std::min( count, mData->valueCount() - fromIndex.row() );
  beginRemoveRows( QModelIndex(), fromIndex.row(), fromIndex.row() + maxCount - 1 );
  mData->removeValues( fromIndex.row(), count );
  endRemoveRows();
}

void ReosTimeSeriesConstantIntervalModel::insertRows( const QModelIndex &fromIndex, int count )
{
  beginInsertRows( QModelIndex(), fromIndex.row(), fromIndex.row() + count - 1 );
  mData->insertValues( fromIndex.row(), count, mDefaultValue );
  endInsertRows();
}

ReosTimeSeriesConstantInterval::ReosTimeSeriesConstantInterval( QObject *parent, const QString &providerKey, const QString &dataSource ):
  ReosTimeSeries( parent, formatKey( providerKey ), dataSource )
  , mTimeStepParameter( new ReosParameterDuration( tr( "Time step" ), this ) )
{
  if ( constantTimeStepDataProvider() )
  {
    ReosDuration timeStep = constantTimeStepDataProvider()->timeStep();
    timeStep.setAdaptedUnit();
    mTimeStepParameter->setValue( timeStep );
  }
  else
  {
    mProvider.reset( static_cast<ReosTimeSerieProvider *>( ReosDataProviderRegistery::instance()->createProvider( formatKey( QStringLiteral( "constant-time-step-memory" ) ) ) ) );
    if ( mProvider )
    {
      connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSeriesConstantInterval::onDataProviderChanged );
      connect( mProvider.get(), &ReosTimeSerieProvider::dataReset, this, &ReosTimeSeriesConstantInterval::dataReset );
      mProvider->setReferenceTime( QDateTime( QDate( QDate::currentDate().year(), 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
    }
  }

  if ( mTimeStepParameter->value() == ReosDuration() )
  {
    constantTimeStepDataProvider()->setTimeStep( ReosDuration( 5, ReosDuration::minute ) );
  }

  connectParameters();
}

ReosParameterDuration *ReosTimeSeriesConstantInterval::timeStepParameter() const
{
  return mTimeStepParameter;
}

void ReosTimeSeriesConstantInterval::setTimeStep( const ReosDuration &timeStep )
{
  constantTimeStepDataProvider()->setTimeStep( timeStep );
}

ReosDuration ReosTimeSeriesConstantInterval::timeStep() const
{
  return constantTimeStepDataProvider()->timeStep();
}

ReosDuration ReosTimeSeriesConstantInterval::relativeTimeAt( int i ) const
{
  return timeStep() * i;
}

QPair<QDateTime, QDateTime> ReosTimeSeriesConstantInterval::timeExtent() const
{
  QPair<QDateTime, QDateTime> ret;
  if ( mProvider->valueCount() > 0 )
    ret.first = timeAt( 0 );
  else
    ret.first = referenceTime();

  if ( mProvider->valueCount() > 1 )
    ret.second = timeAt( mProvider->valueCount() - 1 ).addMSecs( timeStep().valueMilliSecond() );
  else
    ret.second = ret.first;

  return ret;
}

double ReosTimeSeriesConstantInterval::valueAt( int i ) const
{
  return valueWithMode( i, mValueMode );

}

void ReosTimeSeriesConstantInterval::setValueAt( int i, double value )
{
  switch ( mValueMode )
  {
    case ReosTimeSeriesConstantInterval::Value:
      ReosTimeSeries::setValueAt( i, value );
      break;
    case ReosTimeSeriesConstantInterval::Intensity:
      ReosTimeSeries::setValueAt( i, convertFromIntensityValue( value ) );
      break;
    case ReosTimeSeriesConstantInterval::Cumulative:
    {
      if ( i > 0 )
      {
        double cumulValueBefore = valueWithMode( i - 1 );
        ReosTimeSeries::setValueAt( i, value - cumulValueBefore );
      }
      else
      {}     // nothing to do, cumulative value at i=0 is always 0, so it is wrong to set another value
    }
    break;
  }
}

void ReosTimeSeriesConstantInterval::setValues( const QVector<double> &vals )
{
  static_cast<ReosTimeSerieConstantTimeStepProvider *>( mProvider.get() )->setValues( vals );
}

void ReosTimeSeriesConstantInterval::appendValue( double value )
{
  ReosTimeSerieConstantTimeStepProvider *dataValues = constantTimeStepDataProvider();

  if ( !dataValues || !dataValues->isEditable() )
    return;

  switch ( mValueMode )
  {
    case ReosTimeSeriesConstantInterval::Value:
      dataValues->appendValue( value );
      break;
    case ReosTimeSeriesConstantInterval::Intensity:
      dataValues->appendValue( convertFromIntensityValue( value ) );
      break;
    case ReosTimeSeriesConstantInterval::Cumulative:
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

void ReosTimeSeriesConstantInterval::insertValues( int fromPos, int count, double value )
{
  ReosTimeSerieConstantTimeStepProvider *dataValues = constantTimeStepDataProvider();

  if ( !dataValues || !dataValues->isEditable() )
    return;

  switch ( mValueMode )
  {
    case ReosTimeSeriesConstantInterval::Value:
      for ( int i = 0; i < count; ++i )
        dataValues->insertValue( fromPos, value );
      break;
    case ReosTimeSeriesConstantInterval::Intensity:
    {
      double intensity = convertFromIntensityValue( value );
      for ( int i = 0; i < count; ++i )
        dataValues->insertValue( fromPos, convertFromIntensityValue( intensity ) );
    }
    break;
    case ReosTimeSeriesConstantInterval::Cumulative:
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

double ReosTimeSeriesConstantInterval::valueWithMode( int i, ReosTimeSeriesConstantInterval::ValueMode mode ) const
{
  updateData();
  switch ( mode )
  {
    case ReosTimeSeriesConstantInterval::Value:
      return mProvider->value( i );
      break;
    case ReosTimeSeriesConstantInterval::Intensity:
      return mProvider->value( i ) / timeStep().valueUnit( mIntensityTimeUnit );
      break;
    case ReosTimeSeriesConstantInterval::Cumulative:
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

QPair<double, double> ReosTimeSeriesConstantInterval::extentValueWithMode( ReosTimeSeriesConstantInterval::ValueMode mode ) const
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

void ReosTimeSeries::setValueAt( int i, double value )
{
  if ( !mProvider || !mProvider->isEditable() )
    return;

  if ( i < mProvider->valueCount() )
  {
    mProvider->setValue( i, value );
    emit dataChanged();
  }
}

ReosTimeWindow ReosTimeSeries::timeWindow() const
{
  const QPair<QDateTime, QDateTime> te = timeExtent();
  return ReosTimeWindow( te.first, te.second );
}

ReosEncodedElement ReosTimeSeriesConstantInterval::encode( const ReosEncodeContext &context, const QString &descritpion ) const
{
  updateData();
  QString descript = descritpion;
  if ( descript.isEmpty() )
    descript = QStringLiteral( "time-serie-constant-interval" );

  ReosEncodedElement element( descript );
  ReosTimeSeries::baseEncode( element, context );

  return element;
}

ReosTimeSeriesConstantInterval *ReosTimeSeriesConstantInterval::decode( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
{
  if ( element.description() != QStringLiteral( "time-serie-constant-interval" ) )
    return nullptr;

  return new ReosTimeSeriesConstantInterval( element, context, parent );
}

ReosTimeSerieConstantTimeStepProvider *ReosTimeSeriesConstantInterval::constantTimeStepDataProvider() const
{
  return static_cast<ReosTimeSerieConstantTimeStepProvider *>( mProvider.get() );
}

void ReosTimeSeriesConstantInterval::copyFrom( ReosTimeSeriesConstantInterval *other )
{
  if ( !other || !constantTimeStepDataProvider() || !constantTimeStepDataProvider()->isEditable() )
    return;

  constantTimeStepDataProvider()->copy( other->constantTimeStepDataProvider() );
}

void ReosTimeSeriesConstantInterval::onDataProviderChanged()
{
  ReosTimeSeries::onDataProviderChanged();
  if ( constantTimeStepDataProvider() && constantTimeStepDataProvider()->timeStep() != mTimeStepParameter->value() )
    mTimeStepParameter->setValue( constantTimeStepDataProvider()->timeStep() );
}

ReosTimeSeriesConstantInterval::ReosTimeSeriesConstantInterval( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
  : ReosTimeSeries( parent )
  , mTimeStepParameter( new ReosParameterDuration( tr( "Time step" ), this ) )
{
  decodeBase( element, context );

  if ( constantTimeStepDataProvider() )
  {
    constantTimeStepDataProvider()->decode( element.getEncodedData( QStringLiteral( "data-provider" ) ), context );
    mTimeStepParameter->setValue( constantTimeStepDataProvider()->timeStep() );
  }
  else
  {
    //set default one as memory
    QVector<double> values;
    element.getData( QStringLiteral( "values" ), values ); //before Lekan 2.2, values were store in this element
    mProvider = std::make_unique < ReosTimeSerieConstantTimeStepMemoryProvider>( values );
    constantTimeStepDataProvider()->setReferenceTime( referenceTimeParameter()->value() );
    ReosParameterDuration *encodedTimeStep = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ), false, tr( "Time step" ), this );
    mTimeStepParameter->setValue( encodedTimeStep->value() );
    constantTimeStepDataProvider()->setTimeStep( mTimeStepParameter->value() );
  }

  connectParameters();
}

QString ReosTimeSeriesConstantInterval::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;

  return rawKey + QStringLiteral( "::" ) + ReosTimeSeriesConstantInterval::staticType();
}

ReosTimeSeriesConstantInterval::ValueMode ReosTimeSeriesConstantInterval::valueMode() const
{
  return mValueMode;
}

void ReosTimeSeriesConstantInterval::setValueMode( const ValueMode &valueMode )
{
  mValueMode = valueMode;
  emit dataChanged();
  emit settingsChanged();
}

void ReosTimeSeriesConstantInterval::connectParameters()
{
  ReosTimeSeries::connectParameters();
  if ( mProvider )
    mTimeStepParameter->setEditable( mProvider->isEditable() );

  connect( mTimeStepParameter, &ReosParameter::valueChanged, this, [this]
  {
    mTimeStepParameter->blockSignals( true );
    constantTimeStepDataProvider()->setTimeStep( mTimeStepParameter->value() );
    mTimeStepParameter->blockSignals( false );
  } );

  connect( mTimeStepParameter, &ReosParameter::unitChanged, this, &ReosDataObject::dataChanged );
}

double ReosTimeSeriesConstantInterval::convertFromIntensityValue( double v )
{
  return v * timeStep().valueUnit( mIntensityTimeUnit );
}

ReosDuration::Unit ReosTimeSeriesConstantInterval::intensityTimeUnit() const
{
  return mIntensityTimeUnit;
}

void ReosTimeSeriesConstantInterval::setIntensityTimeUnit( const ReosDuration::Unit &intensityTimeUnit )
{
  mIntensityTimeUnit = intensityTimeUnit;
  emit dataChanged();
  emit settingsChanged();
}

bool ReosTimeSeriesConstantInterval::addCumultive() const
{
  return mAddCumulative;
}

void ReosTimeSeriesConstantInterval::setAddCumulative( bool addCumulative )
{
  mAddCumulative = addCumulative;
}

void ReosTimeSeriesConstantInterval::syncWith( ReosTimeSeriesConstantInterval *other )
{
  connect( other, &ReosDataObject::dataChanged, this, [this, other]
  {
    copyAttribute( other );
  } );
}

void ReosTimeSeriesConstantInterval::copyAttribute( ReosTimeSeriesConstantInterval *other )
{
  if ( !other )
    return;
  timeStepParameter()->setValue( other->timeStepParameter()->value() );
  setReferenceTime( other->referenceTime() );
  setValueMode( other->valueMode() );
  setValueUnit( other->valueUnit() );
  setIntensityTimeUnit( other->intensityTimeUnit() );
  setAddCumulative( other->addCumultive() );
}

QString ReosTimeSeries::valueUnit() const
{
  return mValueUnit;
}

void ReosTimeSeries::setValueUnit( const QString &valueUnit )
{
  mValueUnit = valueUnit;
}

double *ReosTimeSeries::data()
{
  updateData();
  return mProvider->data();
}

const QVector<double> &ReosTimeSeries::constData() const
{
  updateData();
  return  mProvider->constData();
}

ReosTimeSerieProvider *ReosTimeSeries::dataProvider() const
{
  return mProvider.get();
}

void ReosTimeSeries::onDataProviderChanged()
{
  setActualized();
  if ( mProvider  && mProvider->referenceTime() != mReferenceTimeParameter->value() )
    mReferenceTimeParameter->setValue( mProvider->referenceTime() );
  updateStats();
  emit dataChanged();
}

QString ReosTimeSeriesConstantInterval::valueModeName( ReosTimeSeriesConstantInterval::ValueMode mode ) const
{
  if ( mValueModeName.contains( mode ) )
    return mValueModeName.value( mode );
  else
    return QString();
}

void ReosTimeSeriesConstantInterval::setValueModeName( ReosTimeSeriesConstantInterval::ValueMode mode, const QString &name )
{
  mValueModeName[mode] = name;
}

QColor ReosTimeSeriesConstantInterval::valueModeColor( ReosTimeSeriesConstantInterval::ValueMode mode ) const
{
  if ( mValueModeName.contains( mode ) )
    return mValueModeColor.value( mode );
  else
    return Qt::black;
}

QString ReosTimeSeriesConstantInterval::unitStringCurrentMode() const
{
  switch ( valueMode() )
  {
    case ReosTimeSeriesConstantInterval::Value:
    case ReosTimeSeriesConstantInterval::Cumulative:
      return  valueModeName( valueMode() ) + QStringLiteral( " (%1)" ).arg( valueUnit() );
      break;
    case ReosTimeSeriesConstantInterval::Intensity:
      return tr( "Intensity (%1/%2)" ).arg( valueUnit(), ReosDuration().unitToString( intensityTimeUnit() ) );
      break;
  }

  return QString();
}

QColor ReosTimeSeriesConstantInterval::currentValueModeColor() const
{
  return valueModeColor( mValueMode );
}

void ReosTimeSeriesConstantInterval::setValueModeColor( ReosTimeSeriesConstantInterval::ValueMode mode, const QColor &color )
{
  mValueModeColor[mode] = color;
}

ReosTimeSeries::ReosTimeSeries( QObject *parent, const QString &providerKey, const QString &dataSource ):
  ReosDataObject( parent )
  , mReferenceTimeParameter( new ReosParameterDateTime( tr( "Reference time" ), this ) )
{
  if ( !providerKey.isEmpty() )
    mProvider.reset( static_cast<ReosTimeSerieProvider *>( ReosDataProviderRegistery::instance()->createProvider( providerKey ) ) );

  if ( mProvider )
  {
    //mProvider->setReferenceTime( QDateTime( QDate( QDate::currentDate().year(), 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
    connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSeries::onDataProviderChanged );
    connect( mProvider.get(), &ReosTimeSerieProvider::dataReset, this, &ReosTimeSeries::dataReset );
    mProvider->setDataSource( dataSource );
  }
}

void ReosTimeSeries::reload( )
{
  if ( mProvider )
    mProvider->load();
}

void ReosTimeSeries::reloadBlocking( int timeout, bool  repeat )
{
  if ( !mProvider )
    return;

  int tryCount = 0;

  do
  {
    if ( tryCount > 0 )
    {
      qDebug() << tr( "Loading timeout %1 times" ).arg( tryCount );
    }
    QEventLoop loop;
    connect( mProvider.get(), &ReosDataProvider::loadingFinished, &loop, &QEventLoop::quit );
    QTimer timer;
    connect( &timer, &QTimer::timeout, &loop, &QEventLoop::quit );
    if ( timeout > 0 )
      timer.start( timeout );

    if ( ! mProvider->isLoading() )
      mProvider->load();

    loop.exec();
    tryCount++;
  }
  while ( repeat && mProvider->isLoading() );

}

void ReosTimeSeries::setReferenceTime( const QDateTime &dateTime )
{
  dataProvider()->setReferenceTime( dateTime );
}

QDateTime ReosTimeSeries::referenceTime() const
{
  updateData();
  return dataProvider()->referenceTime();
}

int ReosTimeSeries::valueCount() const
{
  updateData();
  return mProvider->valueCount();
}

QDateTime ReosTimeSeries::timeAt( int i ) const
{
  updateData();
  if ( referenceTime().isValid() )
    return referenceTime().addMSecs( relativeTimeAt( i ).valueMilliSecond() );
  else
    return QDateTime();
}

double ReosTimeSeries::valueAt( int i ) const
{
  updateData();
  return mProvider->value( i );
}

void ReosTimeSeries::removeValues( int fromPos, int count )
{
  mProvider->removeValues( fromPos, count );
  emit dataChanged();
}

void ReosTimeSeries::clear()
{
  mProvider->clear();
  emit dataChanged();
}

QPair<double, double> ReosTimeSeries::valueExent( bool withZero ) const
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

void ReosTimeSeries::valuesExtent( double &min, double &max, bool withZero ) const
{
  updateData();
  if ( mProvider->valueCount() == 0 )
    return;

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
}

void ReosTimeSeries::baseEncode( ReosEncodedElement &element, const ReosEncodeContext &context ) const
{
  updateData();
  ReosDataObject::encode( element );
  if ( mProvider )
  {
    element.addData( QStringLiteral( "provider-key" ), mProvider->key() );
    element.addEncodedData( QStringLiteral( "provider-data" ), mProvider->encode( context ) );
  }
}

bool ReosTimeSeries::decodeBase( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  ReosDataObject::decode( element );

  mProvider.reset();
  if ( element.hasEncodedData( QStringLiteral( "provider-key" ) ) )
  {
    QString providerKey;
    element.getData( QStringLiteral( "provider-key" ), providerKey );
    mProvider.reset( static_cast<ReosTimeSerieProvider *>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) );
    if ( mProvider && element.hasEncodedData( QStringLiteral( "provider-data" ) ) )
    {
      connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSeries::onDataProviderChanged );
      connect( mProvider.get(), &ReosTimeSerieProvider::dataReset, this, &ReosTimeSeries::dataReset );
      mProvider->decode( element.getEncodedData( QStringLiteral( "provider-data" ) ), context );
      mReferenceTimeParameter->setValue( mProvider->referenceTime() );
      mReferenceTimeParameter->setEditable( mProvider->isEditable() );
    }
  }
  else
  {
    if ( element.hasEncodedData( QStringLiteral( "reference-time" ) ) )
    {
      //no provider and we has a  reference time encoded, so old version < 2.2 ->take this one
      mReferenceTimeParameter->deleteLater();
      mReferenceTimeParameter = ReosParameterDateTime::decode(
                                  element.getEncodedData( QStringLiteral( "reference-time" ) ), false, tr( "Reference time" ), this );
    }
  }

  return true;
}

double ReosTimeSeries::minimum() const
{
  return mMinimum;
}

double ReosTimeSeries::maximum() const
{
  return mMaximum;
}

void ReosTimeSeries::updateStats()
{
  mMaximum = -std::numeric_limits<double>::max();
  mMinimum = std::numeric_limits<double>::max();
  for ( int i = 0; i < mProvider->valueCount(); ++i )
  {
    double value = mProvider->value( i );
    if ( value < mMinimum )
      mMinimum = value;
    if ( value > mMaximum )
      mMaximum = value;
  }
}

void ReosTimeSeries::connectParameters()
{
  if ( mProvider )
    mReferenceTimeParameter->setEditable( mProvider->isEditable() );

  connect( mReferenceTimeParameter, &ReosParameter::valueChanged, this, [this]
  {
    mReferenceTimeParameter->blockSignals( true );
    dataProvider()->setReferenceTime( mReferenceTimeParameter->value() );
    mReferenceTimeParameter->blockSignals( false );
  } );
}


ReosTimeSeriesVariableTimeStep::ReosTimeSeriesVariableTimeStep(
  QObject *parent,
  const QString &providerKey,
  const QString &dataSource )
  : ReosTimeSeries( parent, formatKey( providerKey ), dataSource )
{
  if ( !variableTimeStepDataProvider() )
  {
    mProvider.reset( new ReosTimeSerieVariableTimeStepMemoryProvider( ) );
    connect( mProvider.get(), &ReosTimeSerieProvider::dataChanged, this, &ReosTimeSeriesVariableTimeStep::onDataProviderChanged );
    connect( mProvider.get(), &ReosTimeSerieProvider::dataReset, this, &ReosTimeSeriesVariableTimeStep::dataReset );
    mProvider->setReferenceTime( QDateTime( QDate( QDate::currentDate().year(), 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  }
  connectParameters();
}

ReosDuration ReosTimeSeriesVariableTimeStep::relativeTimeAt( int i ) const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();
  if ( !dataProv )
    return ReosDuration();

  return dataProv->relativeTimeAt( i );
}

ReosDuration ReosTimeSeriesVariableTimeStep::totalDuration() const
{
  if ( valueCount() == 0 )
    return ReosDuration();

  ReosDuration ret = relativeTimeAt( valueCount() - 1 ) - relativeTimeAt( 0 );

  ret.setAdaptedUnit();

  return ret;
}

bool ReosTimeSeriesVariableTimeStep::setRelativeTimeAt( int i, const ReosDuration &relativeTime )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

  if ( !dataProv || !dataProv->isEditable() )
    return false;

  if ( i >= 0 && i < dataProv->valueCount() )
  {
    if ( ( i > 1 &&  dataProv->relativeTimeAt( i - 1 ) >= relativeTime ) ||
         ( ( i < dataProv->valueCount() - 1 ) && dataProv->relativeTimeAt( i + 1 ) <= relativeTime ) )
      return false;

    dataProv->setRelativeTimeAt( i, relativeTime );
    return true;
  }

  return false;
}

void ReosTimeSeriesVariableTimeStep::setAnyRelativeTimeAt( int i, const ReosDuration &relativeTime )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

  if ( !dataProv || !dataProv->isEditable() )
    return;

  if ( i >= 0 && i < dataProv->valueCount() )
    dataProv->setRelativeTimeAt( i, relativeTime );
}

QPair<QDateTime, QDateTime> ReosTimeSeriesVariableTimeStep::timeExtent() const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

  if ( !dataProv )
    return QPair<QDateTime, QDateTime>();

  if ( dataProv->valueCount() == 0 )
    return {referenceTime(), referenceTime() };

  QDateTime refTime = referenceTime();

  return {refTime.addMSecs( dataProv->relativeTimeAt( 0 ).valueMilliSecond() ), refTime.addMSecs( dataProv->lastRelativeTime().valueMilliSecond() )};
}

const QVector<ReosDuration> ReosTimeSeriesVariableTimeStep::relativeTimesData() const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

  if ( dataProv )
    return dataProv->constTimeData();

  return QVector<ReosDuration>();
}

void ReosTimeSeriesVariableTimeStep::setValue( const ReosDuration &relativeTime, double value )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

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

void ReosTimeSeriesVariableTimeStep::setValue( const QDateTime &time, double value )
{
  if ( !referenceTime().isValid() )
  {
    setReferenceTime( time );
    setValue( ReosDuration( 0.0, ReosDuration::second ), value );
  }
  else
  {
    const ReosDuration relativeTime = ReosDuration( referenceTime().msecsTo( time ) );
    setValue( relativeTime, value );
  }

}

double ReosTimeSeriesVariableTimeStep::valueAtTime( const ReosDuration &relativeTime ) const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

  if ( !dataProv )
    return 0;

  return dataProv->valueAtTime( relativeTime );
}

double ReosTimeSeriesVariableTimeStep::valueAtTime( const QDateTime &time ) const
{
  return valueAtTime( ReosDuration( referenceTime().msecsTo( time ) ) );
}

void ReosTimeSeriesVariableTimeStep::addOther( const ReosTimeSeriesVariableTimeStep *other, double factor, bool allowInterpolation )
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();
  ReosTimeSerieVariableTimeStepProvider *otherDataProv = other->variableTimeStepDataProvider();

  if ( !dataProv || !otherDataProv || !dataProv->isEditable() )
    return;

  blockSignals( true );

  ReosDuration offset( referenceTime().msecsTo( other->referenceTime() ), ReosDuration::millisecond );

  //need to store apart and then apply, if not changed value will disturb the addiion for following
  QVector<double> newValue_1( dataProv->valueCount() );
  for ( int i = 0; i < dataProv->valueCount(); ++i )
  {
    ReosDuration thisTimeValue = dataProv->relativeTimeAt( i );
    newValue_1[i] = mProvider->value( i ) + factor * other->valueAtTime( thisTimeValue - offset );
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

int ReosTimeSeriesVariableTimeStep::timeValueIndex( const ReosDuration &time, bool &exact ) const
{
  ReosTimeSerieVariableTimeStepProvider *dataProv = variableTimeStepDataProvider();

  if ( !dataProv )
    return 0;

  return dataProv->timeValueIndex( time, exact );
}

QString ReosTimeSeriesVariableTimeStep::unitString() const
{
  return mUnitString;
}

void ReosTimeSeriesVariableTimeStep::setUnitString( const QString &unitString )
{
  mUnitString = unitString;
}

QColor ReosTimeSeriesVariableTimeStep::color() const
{
  return mColor;
}

void ReosTimeSeriesVariableTimeStep::setColor( const QColor &color )
{
  mColor = color;
  emit colorChanged( color );
  emit displayColorChanged( color );
}

void ReosTimeSeriesVariableTimeStep::copyFrom( const ReosTimeSeriesVariableTimeStep *other )
{
  if ( !other || !variableTimeStepDataProvider() || !variableTimeStepDataProvider()->isEditable() )
    return;

  variableTimeStepDataProvider()->copy( other->variableTimeStepDataProvider() );
}

bool ReosTimeSeriesVariableTimeStep::operator==( const ReosTimeSeriesVariableTimeStep &other ) const
{
  if ( other.valueCount() != valueCount() )
    return false;

  if ( referenceTime() != other.referenceTime() )
    return false;

  for ( int i = 0; i < valueCount(); ++i )
  {
    if ( valueAt( i ) != other.valueAt( i ) )
      return false;
    if ( relativeTimeAt( i ) != other.relativeTimeAt( i ) )
      return false;
  }

  return true;
}

void ReosTimeSeriesVariableTimeStep::setCommonColor( const QColor &color )
{
  mColor = color;
  emit displayColorChanged( color );
}

void ReosTimeSeriesVariableTimeStep::baseEncode( ReosEncodedElement &element, const ReosEncodeContext &context ) const
{
  ReosTimeSeries::baseEncode( element, context );

  element.addData( QStringLiteral( "unit-string" ), mUnitString );
  element.addData( QStringLiteral( "color" ), mColor );

}

bool ReosTimeSeriesVariableTimeStep::decodeBase( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  ReosTimeSeries::decodeBase( element, context );

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

QString ReosTimeSeriesVariableTimeStep::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;

  return rawKey + QStringLiteral( "::" ) + ReosTimeSeriesVariableTimeStep::staticType();
}

ReosTimeSerieVariableTimeStepProvider *ReosTimeSeriesVariableTimeStep::variableTimeStepDataProvider() const
{
  return static_cast<ReosTimeSerieVariableTimeStepProvider *>( mProvider.get() );
}

QAbstractItemModel *ReosTimeSeriesVariableTimeStep::model()
{
  if ( !mModel )
  {
    mModel = new ReosTimeSeriesVariableTimeStepModel( this );
    mModel->setSerie( this );
  }

  return mModel;
}

ReosEncodedElement ReosTimeSeriesVariableTimeStep::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "time-serie-variable-time-step" ) );

  baseEncode( element, context );

  return element;
}

ReosTimeSeriesVariableTimeStep *ReosTimeSeriesVariableTimeStep::decode( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
{
  if ( element.description() != QStringLiteral( "time-serie-variable-time-step" ) )
    return nullptr;

  std::unique_ptr<ReosTimeSeriesVariableTimeStep> ret = std::make_unique<ReosTimeSeriesVariableTimeStep>( parent );
  ret->decodeBase( element, context );

  return ret.release();
}

ReosTimeSeriesVariableTimeStepModel::ReosTimeSeriesVariableTimeStepModel( QObject *parent )
  : ReosTimeSerieModel( parent )
  , mFixedTimeStep( 5, ReosDuration::minute )
{
}

int ReosTimeSeriesVariableTimeStepModel::rowCount( const QModelIndex & ) const
{
  if ( !mData.isNull() )
    return isEditable() ? mData->valueCount() + 1 : mData->valueCount();
  else
    return 3;
}

int ReosTimeSeriesVariableTimeStepModel::columnCount( const QModelIndex & ) const
{
  return valueColumn() + 1;
}

QVariant ReosTimeSeriesVariableTimeStepModel::data( const QModelIndex &index, int role ) const
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
          return mData->timeAt( index.row() ).toString( QLocale().dateTimeFormat( QLocale::ShortFormat ) );
      }
      else if ( index.column() == valueColumn() )
      {
        if ( index.row() < mData->valueCount() )
          return ReosParameter::doubleToString( mData->valueAt( index.row() ) );
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
    case Qt::BackgroundRole:
      if ( !( flags( index ) & Qt::ItemIsEditable ) )
        return QColor( 220, 220, 220 );
      break;
    case Qt::TextAlignmentRole:
      return Qt::AlignRight;
      break;
    default:
      return QVariant();
  }

  return QVariant();
}

bool ReosTimeSeriesVariableTimeStepModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !index.isValid() || !isEditable() || index.row() > mData->valueCount() || !( flags( index )& Qt::ItemIsEditable ) )
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
          // relative time column
          ReosDuration relativeTime( value.toDouble(), mVariableTimeStepUnit );

          if ( mData->valueCount() == 0 ||
               relativeTime > mData->relativeTimeAt( mData->valueCount() - 1 ) )
          {
            beginInsertRows( QModelIndex(), index.row() + 1, index.row() + 1 );
            mData->setValue( ReosDuration( relativeTime ), mDefaultValue );
            endInsertRows();
            return true;
          }
        }
        else
        {
          // value column
          beginInsertRows( QModelIndex(), index.row() + 1, index.row() + 1 );
          if ( mNewRowWithFixedTimeStep )
          {
            ReosDuration previousRelativeTime;
            if ( index.row() > 0 )
            {
              previousRelativeTime = mData->relativeTimeAt( index.row() - 1 );
              mData->setValue( previousRelativeTime + mFixedTimeStep, v );
            }
            else
              mData->setValue( ReosDuration( 0, mVariableTimeStepUnit ), v );
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
          return true;
        }
      }
      else
      {
        if ( index.column() == valueColumn() )
        {
          mData->setValueAt( index.row(), v );
          return true;
        }

        if ( !mNewRowWithFixedTimeStep )
        {
          if ( index.column() == 1 )
          {
            ReosDuration relativeTime( v, mVariableTimeStepUnit );
            mData->setRelativeTimeAt( index.row(), relativeTime );
            return true;
          }
        }
      }
    }
  }
  return false;
}

Qt::ItemFlags ReosTimeSeriesVariableTimeStepModel::flags( const QModelIndex &index ) const
{
  if ( isEditable() && !mNewRowWithFixedTimeStep && index.column() > 0 )
    return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;
  else if ( isEditable() && index.column() == valueColumn() )
    return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;

  return QAbstractTableModel::flags( index );
}

QVariant ReosTimeSeriesVariableTimeStepModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Horizontal )
  {
    if ( role == Qt::DisplayRole )
    {
      if ( section == valueColumn() )
      {
        if ( mData )
          return mData->unitString();
      }
      else if ( section == 0 )
        return tr( "Time" );
      else if ( ! mNewRowWithFixedTimeStep && section == 1 )
        return tr( "Relative time" );
    }

    return QVariant();
  }

  return QVariant();
}

void ReosTimeSeriesVariableTimeStepModel::setValues( const QModelIndex &fromIndex, const QList<QVariantList> &data )
{
  setValuesPrivate( fromIndex, data, true );
}

static bool isStringDatetime( const QString &str )
{
  // try with loval format
  QDateTime time = QLocale().toDateTime( str, QLocale::ShortFormat );
  if ( time.isValid() )
    return true;

  time = QDateTime::fromString( str, Qt::ISODate );

  return time.isValid();
}

static QDateTime stringToDateTime( const QString &stringDateTime )
{
  QDateTime time = QLocale().toDateTime( stringDateTime, QLocale::ShortFormat );
  if ( time.isValid() )
  {
    time.setTimeSpec( Qt::UTC );
    return time;
  }

  time = QDateTime::fromString( stringDateTime, Qt::ISODate );
  if ( time.isValid() )
  {
    if ( time.timeSpec() != Qt::UTC )
      time.setTimeSpec( Qt::UTC );

    return time;
  }

  return QDateTime();
}


void ReosTimeSeriesVariableTimeStepModel::setValuesPrivate( const QModelIndex &fromIndex, const QList<QVariantList> &values, bool checkValidity )
{
  if ( checkValidity && !checkListValuesValidity( fromIndex, values, false ) )
    return;

  if ( !fromIndex.isValid() || values.isEmpty() )
    return;

  if ( !fromIndex.isValid() )
    return;

  int colCount = values.at( 0 ).count();

  bool absoluteTime = false;

  if ( !values.empty() && !values.at( 0 ).empty() )
    absoluteTime =  isStringDatetime( values.at( 0 ).at( 0 ).toString() );

  int startRow = fromIndex.row();
  int endRow = startRow + values.count() - 1;
  int insertedRowCount = endRow - mData->valueCount() + 1;
  if ( insertedRowCount > 0 )
    beginInsertRows( QModelIndex(), rowCount( QModelIndex() ) - 1, rowCount( QModelIndex() ) + insertedRowCount - 2 );

  for ( int i = 0; i < values.count(); ++i )
  {
    QVariantList varRow = values.at( i );
    ReosDuration relativeTime;

    if ( i + startRow == mData->valueCount() && colCount == 1 )
      relativeTime = mData->relativeTimeAt( mData->valueCount() - 1 ) + mFixedTimeStep * ( i + 1 );
    else if ( colCount == 1 )
    {
      relativeTime = mData->relativeTimeAt( i + startRow );
    }
    else if ( colCount == 2 )
    {
      if ( absoluteTime )
      {
        QDateTime time = stringToDateTime( varRow.at( 0 ).toString() );
        relativeTime = ReosDuration( mData->referenceTime().msecsTo( time ), ReosDuration::millisecond );
      }
      else
        relativeTime = ReosDuration( varRow.at( 0 ).toDouble(), mVariableTimeStepUnit );
    }

    if ( i + startRow == mData->valueCount() )
      mData->setValue( relativeTime, varRow.last().toDouble() );
    else
    {
      mData->setAnyRelativeTimeAt( i + startRow, relativeTime );
      mData->setValueAt( i + startRow, varRow.last().toDouble() );
    }
  }

  if ( insertedRowCount > 0 )
    endInsertRows();

  emit dataChanged( index( startRow, 0, QModelIndex() ), index( endRow, 0, QModelIndex() ) );

}

void ReosTimeSeriesVariableTimeStepModel::insertValues( const QModelIndex &fromIndex, const QList<QVariantList> &values )
{
  if ( !checkListValuesValidity( fromIndex, values, true ) )
    return;

  bool withTimeValue = false;
  if ( !values.isEmpty() )
    withTimeValue = values.at( 0 ).count() > 1;

  if ( insertRowsPrivate( fromIndex, values.count(), withTimeValue ) )
    setValuesPrivate( fromIndex, values, false );
}

void ReosTimeSeriesVariableTimeStepModel::deleteRows( const QModelIndex &fromIndex, int count )
{
  if ( !fromIndex.isValid() )
    return;
  int maxCount = std::min( count, mData->valueCount() - fromIndex.row() );
  beginRemoveRows( QModelIndex(), fromIndex.row(), fromIndex.row() + maxCount - 1 );
  mData->removeValues( fromIndex.row(), count );
  endRemoveRows();
}

void ReosTimeSeriesVariableTimeStepModel::insertRows( const QModelIndex &fromIndex, int count )
{
  insertRowsPrivate( fromIndex, count, false );
}

bool ReosTimeSeriesVariableTimeStepModel::insertRowsPrivate( const QModelIndex &fromIndex, int count, bool followdBySetValueWithTime )
{
  if ( count <= 0 )
    return false;

  if ( !followdBySetValueWithTime &&
       fromIndex.row() <= 0 &&
       mData->valueCount() > 0 &&
       mData->relativeTimeAt( 0 ) == ReosDuration() )
    return false;

  ReosDuration startInverval;
  ReosDuration endInterval;
  bool interpolate = false;;
  double startValue = mDefaultValue;
  double endValue = mDefaultValue;

  int insertedIntervalCount = count;

  ReosDuration step;
  if ( mData->valueCount() == 0 )
  {
    startInverval = ReosDuration( 0, mVariableTimeStepUnit );
    endInterval = startInverval + mFixedTimeStep * count;
    step = mFixedTimeStep;
  }
  else if ( fromIndex.row() >= mData->valueCount() ) //index in the last row of the table (the void one)
  {
    startInverval = mData->relativeTimeAt( mData->valueCount() - 1 ) + mFixedTimeStep;
    endInterval = startInverval + mFixedTimeStep * count;
    step = mFixedTimeStep;
  }
  else
  {
    if ( fromIndex.row() == 0 )
      startInverval = ReosDuration( 0, mVariableTimeStepUnit );
    else
    {
      startInverval = mData->relativeTimeAt( fromIndex.row() - 1 );
      insertedIntervalCount++;
    }

    endInterval = mData->relativeTimeAt( fromIndex.row() );

    ReosDuration diff = endInterval - startInverval;
    if ( diff == ReosDuration( qint64( 0 ) ) &&  followdBySetValueWithTime )
      diff = ReosDuration( qint64( 1000 ) );  //the time will be replaced later, we just need a time step that is non zero one

    step = diff / ( insertedIntervalCount );

    if ( fromIndex.row() != 0 )
      startInverval = startInverval + step;
    else if ( followdBySetValueWithTime )
    {
      step = ReosDuration() - step;
      startInverval = startInverval + step;
    }

    endInterval = endInterval - step;
    interpolate = true;

    if ( fromIndex.row() > 0 )
      startValue = mData->valueAt( fromIndex.row() - 1 );
    else
      startValue = mDefaultValue;

    endValue = mData->valueAt( fromIndex.row() );
  }


  beginInsertRows( QModelIndex(), fromIndex.row(), fromIndex.row() + count - 1 );
  for ( int i = 0; i < count; ++i )
  {
    double value;
    if ( interpolate )
      value = startValue + i * ( endValue - startValue ) / ( insertedIntervalCount );
    else
      value = startValue;

    mData->setValue( startInverval + step * i, value );
  }
  endInsertRows();

  return true;
}

void ReosTimeSeriesVariableTimeStepModel::setSerie( ReosTimeSeriesVariableTimeStep *serie )
{
  if ( mData )
    disconnect( mData, &ReosDataObject::dataReset, this, &ReosTimeSeriesVariableTimeStepModel::updateModel );
  beginResetModel();
  mData = serie;
  endResetModel();
  if ( mData )
    connect( mData, &ReosDataObject::dataReset, this, &ReosTimeSeriesVariableTimeStepModel::updateModel );
}

void ReosTimeSeriesVariableTimeStepModel::setNewRowWithFixedTimeStep( bool newRowWithFixedTimeStep )
{
  beginResetModel();
  mNewRowWithFixedTimeStep = newRowWithFixedTimeStep;
  endResetModel();
}

void ReosTimeSeriesVariableTimeStepModel::setFixedTimeStep( const ReosDuration &fixedTimeStep )
{
  mFixedTimeStep = fixedTimeStep;
}

void ReosTimeSeriesVariableTimeStepModel::setVariableTimeStepUnit( const ReosDuration::Unit &variableTimeStepUnit )
{
  beginResetModel();
  mVariableTimeStepUnit = variableTimeStepUnit;
  endResetModel();
}

bool ReosTimeSeriesVariableTimeStepModel::isEditable() const
{
  if ( mData && mData->dataProvider() )
    return mData->dataProvider()->isEditable();

  return false;
}

void ReosTimeSeriesVariableTimeStepModel::updateModel()
{
  beginResetModel();
  endResetModel();
}

int ReosTimeSeriesVariableTimeStepModel::valueColumn() const
{
  if ( mNewRowWithFixedTimeStep )
    return 1;
  else
    return 2;
}

bool ReosTimeSeriesVariableTimeStepModel::checkListValuesValidity( const QModelIndex &index, const QList<QVariantList> &data, bool insert )
{
  if ( data.isEmpty() || !index.isValid() )
    return false;

  int colCount = data.at( 0 ).count();

  if ( colCount != 1 && colCount != 2 )
    return false;

  bool absoluteTime = isStringDatetime( data.at( 0 ).at( 0 ).toString() );

  ReosDuration prevRelativeTime;

  for ( int i = 0; i < data.count(); ++i )
  {
    const QVariantList &varList = data.at( i );

    if ( varList.count() != colCount )
      return false;

    bool ok = false;

    ReosDuration currentRelativeTime;

    if ( colCount == 2 )
    {
      if ( absoluteTime )
      {
        QDateTime time = stringToDateTime( varList.at( 0 ).toString() );
        ok = time.isValid();

        if ( insert )
        {
          if ( mData->valueCount() > 0 && index.row() > 0 )
            ok &= time > mData->timeAt( index.row() - 1 );

          if ( index.row() < mData->valueCount() - 1 )
            ok &= time < mData->timeAt( index.row() );
        }
        else
        {
          if ( index.row() > 0 )
            ok &= time > mData->timeAt( index.row() - 1 );

          if ( index.row() + data.count() < mData->valueCount() )
            ok &= time < mData->timeAt( index.row() + data.count() + 1 );
        }
        currentRelativeTime = ReosDuration( mData->referenceTime().msecsTo( time ), ReosDuration::millisecond );
      }
      else
      {
        ReosDuration time( varList.at( 0 ).toDouble( &ok ), mVariableTimeStepUnit );
        if ( insert )
        {
          if ( mData->valueCount() > 0 && index.row() > 0 )
            ok &= time > mData->relativeTimeAt( index.row() - 1 );

          if ( index.row() < mData->valueCount() - 1 )
            ok &= time < mData->relativeTimeAt( index.row() );
        }
        else
        {
          if ( index.row() > 0 )
            ok &= time > mData->relativeTimeAt( index.row() - 1 );

          if ( index.row() + data.count() < mData->valueCount() )
            ok &= time < mData->relativeTimeAt( index.row() + data.count() + 1 );
        }
        currentRelativeTime = time;
      }

      if ( i != 0 )
        if ( currentRelativeTime <= prevRelativeTime )
          return false;

      prevRelativeTime = currentRelativeTime;

      if ( !ok )
        return false;
    }

    varList.last().toDouble( &ok );

    if ( !ok )
      return false;
  }

  return true;
}
