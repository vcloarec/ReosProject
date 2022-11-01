/***************************************************************************
  reosdssprovider.cpp - ReosDssProvider

 ---------------------
 begin                : 17.10.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosdssprovider.h"

#include <QFileInfo>

#include "reostimeserie.h"
#include "reosdssutils.h"

ReosDssProviderBase::ReosDssProviderBase()
{

}

QString ReosDssProviderBase::staticKey()
{
  return QStringLiteral( "delft-fews-xml" );
}

#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
#define skipEmptyPart QString::SkipEmptyParts
#else
#define skipEmptyPart Qt::SplitBehaviorFlags::SkipEmptyParts
#endif

QString ReosDssProviderBase::fileNameFromUri( const QString &uri )
{
  if ( !uri.contains( QStringLiteral( "\"" ) ) )
    return QString();

  QStringList split = uri.split( QStringLiteral( "\"" ), skipEmptyPart );
  return split.at( 0 );
}

ReosDssPath ReosDssProviderBase::dssPathFromUri( const QString &uri )
{
  if ( !uri.contains( QStringLiteral( "::" ) ) )
    return ReosDssPath( QString() );

  const QStringList parts = uri.split( QStringLiteral( "::" ) );
  if ( parts.count() > 1 )
    return ReosDssPath( parts.at( 1 ) );

  return ReosDssPath( QString() );
}

ReosDssProviderBase::~ReosDssProviderBase() = default;

QString ReosDssProviderTimeSerieConstantTimeStep::key() const
{
  return ReosDssProviderBase::staticKey() + QStringLiteral( "::" ) + dataType();
}

void ReosDssProviderTimeSerieConstantTimeStep::load()
{
  mReferenceTime = QDateTime();
  mValues.clear();
  mDirty = false;
  const QString uri = dataSource();
  const QString fileName = fileNameFromUri( uri );
  const ReosDssPath path = dssPathFromUri( uri );
  mFile.reset( new ReosDssFile( fileName, false ) );

  if ( !mFile->isValid() )
  {
    mFile.reset();
    return;
  }

  if ( !mFile->pathExist( path ) )
    return;

  mReferenceTime = mFile->referenceTime( path );
}

QDateTime ReosDssProviderTimeSerieConstantTimeStep::referenceTime() const
{
  return mReferenceTime;
}

int ReosDssProviderTimeSerieConstantTimeStep::valueCount() const {return mValues.count();}

double ReosDssProviderTimeSerieConstantTimeStep::value( int i ) const
{
  if ( i < 0 || i > mValues.count() )
    return std::numeric_limits<double>::quiet_NaN();

  return mValues.at( i );
}

double ReosDssProviderTimeSerieConstantTimeStep::firstValue() const
{
  if ( mValues.count() > 0 )
    return mValues.first();

  return std::numeric_limits<double>::quiet_NaN();
}

double ReosDssProviderTimeSerieConstantTimeStep::lastValue() const
{
  if ( mValues.count() > 0 )
    return mValues.last();

  return std::numeric_limits<double>::quiet_NaN();
}

double *ReosDssProviderTimeSerieConstantTimeStep::data() {return mValues.data();}

const QVector<double> &ReosDssProviderTimeSerieConstantTimeStep::constData() const {return mValues;}

bool ReosDssProviderTimeSerieConstantTimeStep::createNewSerie( const ReosDssPath &path, ReosDssFile &dssFile, QString &error ) const
{
  return dssFile.createConstantIntervalSeries( path, error );
}

ReosDuration ReosDssProviderTimeSerieConstantTimeStep::timeStep() const
{
  return mTimeStep;
}

bool ReosDssProviderTimeSerieConstantTimeStep::isTimeStepCompatible( const ReosDuration &timeStep ) const
{
  return ReosDssUtils::closestValidInterval( timeStep ) == timeStep;
}

void ReosDssProviderTimeSerieConstantTimeStep::setReferenceTime( const QDateTime &referenceTime )
{
  mDirty = true;
  mReferenceTime = referenceTime;
}

void ReosDssProviderTimeSerieConstantTimeStep::setTimeStep( const ReosDuration &timeStep )
{
  if ( !isTimeStepCompatible( timeStep ) )
    return;
  mDirty = true;
  mTimeStep = timeStep;
}

void ReosDssProviderTimeSerieConstantTimeStep::resize( int size )
{
  mDirty = true;
  mValues.resize( size );
}

void ReosDssProviderTimeSerieConstantTimeStep::appendValue( double value )
{
  mDirty = true;
  mValues.append( value );
}

void ReosDssProviderTimeSerieConstantTimeStep::prependValue( double value )
{
  mDirty = true;
  mValues.prepend( value );
}

void ReosDssProviderTimeSerieConstantTimeStep::insertValue( int pos, double value )
{
  mDirty = true;
  mValues.insert( pos, value );
}

void ReosDssProviderTimeSerieConstantTimeStep::setValue( int index, double value )
{
  mDirty = true;
  mValues.replace( index, value );
}

void ReosDssProviderTimeSerieConstantTimeStep::removeValues( int from, int count )
{
  mDirty = true;
  int maxCount = std::min( count, mValues.count() - from );
  QVector<double>::iterator itStart = mValues.begin() + from;
  QVector<double>::iterator itEnd = itStart + maxCount;
  mValues.erase( itStart, itEnd );
}

void ReosDssProviderTimeSerieConstantTimeStep::clear()
{
  mDirty = true;
  mValues.clear();
}

bool ReosDssProviderTimeSerieConstantTimeStep::persistData( QString &error )
{
  const QString &uri = dataSource();
  if ( !mFile || !mFile->isValid() )
  {
    error = QObject::tr( "DSS file not opened or not valid, Unable to write on file." );
    return false;
  }

  const ReosDssPath path = ReosDssProviderBase::dssPathFromUri( uri );

  bool res = mFile->writeConstantIntervalSeries( path, mReferenceTime, mTimeStep, mValues, error );
  if ( res )
    load();

  return res;
}

QString ReosDssProviderTimeSerieConstantTimeStep::dataType()
{
  return ReosTimeSerieConstantInterval::staticType();
}

ReosTimeSerieProvider *ReosDssProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType.contains( ReosDssProviderTimeSerieConstantTimeStep::dataType() ) )
    return new ReosDssProviderTimeSerieConstantTimeStep();

  return nullptr;
}

bool ReosDssProviderFactory::createNewDataSource( const QString &uri, const QString &dataType, QString &error )
{
  const QString fileName = ReosDssProviderBase::fileNameFromUri( uri );
  const QFileInfo fileInfo( fileName );
  const ReosDssPath path = ReosDssProviderBase::dssPathFromUri( uri );

  if ( !path.isValid() )
  {
    error = QObject::tr( "The DSS path is invalid \"%1\"." );
    return false;
  }

  ReosDssFile dssFile( fileName, !fileInfo.exists() );

  if ( !dssFile.isValid() )
  {
    error = QObject::tr( "Unable to create or open file \"%1\"." ).arg( fileName );
    return false;
  }

  std::unique_ptr<ReosDssProviderBase> provider;

  if ( dataType.contains( ReosDssProviderTimeSerieConstantTimeStep::dataType() ) )
    provider.reset( new ReosDssProviderTimeSerieConstantTimeStep() );

  if ( provider )
    return provider->createNewSerie( path, dssFile, error );

  error = QObject::tr( "Data type no supported by DSS provider." );

  return false;
}

QString ReosDssProviderFactory::key() const
{
  return ReosDssProviderBase::staticKey();
}
