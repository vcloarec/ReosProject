/***************************************************************************
  reosdssfile.cpp - ReosDssFile

 ---------------------
 begin                : 11.10.2022
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
#include "reosdssfile.h"

#include <QFileInfo>
#include <QDateTime>
#include <QDebug>

extern "C" {
#include "heclib.h"
}

#include "reosduration.h"
#include "reoshydrograph.h"
#include "reosdssutils.h"


ReosDssFile::ReosDssFile( const QString &filePath, bool create )
  : mFileName( filePath )
{
  QFileInfo fileInfo( filePath );
  if ( !create && !fileInfo.exists() )
    return;
  else if ( create && fileInfo.exists() )
    if ( !QFile::remove( filePath ) )
      return;

  mIfltab.reset( new std::array<long long, 250> );
  mStatus = zopen( mIfltab->data(), filePath.toStdString().c_str() );
  mIsOpen = mStatus == STATUS_OKAY;
  mIsValid = mIsOpen;
}

ReosDssFile::ReosDssFile( ReosDssFile &&other )
{
  mIfltab = std::move( other.mIfltab );
  mStatus = other.mStatus;
  mIsValid = other.mIsValid;
  mIsOpen = other.mIsOpen;
}

ReosDssFile::~ReosDssFile()
{
  close();
}

ReosDssFile &ReosDssFile::operator=( ReosDssFile &&other )
{
  mIfltab = std::move( other.mIfltab );
  mStatus = other.mStatus;
  mIsValid = other.mIsValid;
  mIsOpen = other.mIsOpen;

  return *this;
}

bool ReosDssFile::isValid() const
{
  return mIsValid;
}

bool ReosDssFile::isOpen() const
{
  return mIsOpen;
}

void ReosDssFile::close()
{
  if ( mIfltab && mIsOpen )
  {
    zclose( mIfltab->data() );
    mIfltab.reset();
    mIsOpen = false;
    mIsValid = false;
  }
}

bool ReosDssFile::pathExist( const ReosDssPath &path, bool considerInterval ) const
{
  QList<ReosDssPath> pathes = searchRecordsPath( path, considerInterval );
  return !pathes.empty();

}

void ReosDssFile::getSeries( const ReosDssPath &path, QVector<double> &values, ReosDuration &timeStep, QDateTime &startTime )
{
  ReosDssPath allDatapath = path;
  allDatapath.setStartDate( QString( '*' ) );

  std::unique_ptr<zStructTimeSeries> timeSeries( zstructTsNew( allDatapath.c_pathString() ) );
  int status = ztsRetrieve( mIfltab->data(), timeSeries.get(), -3, 2, 0 );

  if ( status == STATUS_OKAY )
  {
    int valueCount = timeSeries->numberValues;
    values.resize( valueCount );
    memcpy( values.data(), timeSeries->doubleValues, static_cast<size_t>( valueCount )*sizeof( double ) );

    int daySince1900 = timeSeries->startJulianDate;
    int startTimeSeconds = timeSeries->startTimeSeconds;

    QDate date = QDate( 1900, 01, 01 ).addDays( daySince1900 - 1 );
    QTime time = QTime::fromMSecsSinceStartOfDay( startTimeSeconds * 1000 );
    startTime = QDateTime( date, time, Qt::UTC );

    timeStep = ReosDuration( timeSeries->timeIntervalSeconds, ReosDuration::second );
  }

  zstructFree( timeSeries.release() );
}

bool ReosDssFile::createConstantIntervalSeries( const ReosDssPath &path, QString &error )
{
  ReosDuration intervalDuration = path.timeIntervalDuration();
  if ( intervalDuration == ReosDuration() )
    intervalDuration = ReosDuration( 1.0, ReosDuration::hour );
  QVector<double> values;
  values.append( UNDEFINED_DOUBLE ); //DSS need at leastone value, we give it an undefined one because our time serie is void

  //As there is no value, time is not important, it will be override when data will be written in the file
  QDate startDate = QDate::currentDate();
  QTime startTime = QTime( 0, 0, 0 );

  return writeConstantIntervalSeries( path, QDateTime( startDate, startTime ), intervalDuration, values, error );
}

bool ReosDssFile::writeConstantIntervalSeries(
  const ReosDssPath &path,
  const QDateTime &startTime,
  const ReosDuration &timeStep,
  const QVector<double> &values,
  QString &error )
{
  if ( !mIsOpen || !mIsValid || !mIfltab )
  {
    error = QObject::tr( "DSS file \"%1\" is not valid." ).arg( mFileName );
    return false;
  }

  ReosDssPath pathToRemove = path;
  pathToRemove.setTimeInterval( QString( '*' ) ); //we need to remove all time interval
  removeDataset( pathToRemove );

  return writeConstantIntervalSeriesPrivate( path, startTime, timeStep, values, error );
}

bool ReosDssFile::writeConstantIntervalSeriesPrivate(
  const ReosDssPath &path,
  const QDateTime &startDateTime,
  const ReosDuration &timeStep,
  const QVector<double> &values,
  QString &error )
{
  ReosDssPath pathToWrite = path;
  QString ePart = getEPart( timeStep );
  if ( ePart == QString() )
  {
    error = QObject::tr( "Invalid time interval for DSS format." );
    return false;
  }
  pathToWrite.setTimeInterval( ePart );

  QDate startDate = startDateTime.date();
  QTime startTime = startDateTime.time();
  std::vector<char> strDate( 13 );
  yearMonthDayToDate( startDate.year(), startDate.month(), startDate.day(), 4, strDate.data(), strDate.size() );
  std::vector<char> strTime( 13 );
  int milliSeconds = startTime.msecsSinceStartOfDay();
  int seconds = milliSeconds / 1000;
  milliSeconds = milliSeconds - seconds * 1000;
  secondsToTimeString( seconds, milliSeconds, 3, strTime.data(), strTime.size() );

  std::unique_ptr<zStructTimeSeries> timeSerie(
    zstructTsNewRegDoubles( pathToWrite.c_pathString(),
                            const_cast<double *>( values.data() ),
                            values.count(),
                            strDate.data(),
                            strTime.data(),
                            "", // for now, we do not care about unit
                            "" ) ); // for now, we do not care about type f value

  int res = ztsStore( mIfltab->data(), timeSerie.get(), 0 );
  zstructFree( timeSerie.release() );

  if ( res != STATUS_OKAY )
  {
    error = QObject::tr( "Error when storing the data on file." );
    return false;
  }

  return true;
}

QString ReosDssFile::getEPart( const ReosDuration &interval, bool findClosest )
{
  std::vector<char> ret;
  ret.resize( 64 );

  int seconds;
  if ( findClosest )
    seconds = static_cast<int>( ReosDssUtils::closestValidInterval( interval ).valueSecond() + 0.5 );
  else
    seconds = static_cast<int>( interval.valueSecond() + 0.5 );

  int res = ztsGetEPartFromInterval( seconds, ret.data(), ret.size() );

  if ( res != STATUS_OKAY )
    return QString();

  return QString::fromStdString( std::string( ret.data() ) );
}

ReosDssPath ReosDssFile::firstFullPath( const ReosDssPath &path, bool considerInterval ) const
{
  QList<ReosDssPath> pathes = searchRecordsPath( path, considerInterval );
  if ( pathes.isEmpty() )
    return ReosDssPath();
  else
    return pathes.first();
}

QList<ReosDssPath> ReosDssFile::searchRecordsPath( const ReosDssPath &path, bool considerInterval ) const
{
  QList<ReosDssPath> ret;
  std::unique_ptr<zStructCatalog> catStruct( zstructCatalogNew() );
  int status;
  ReosDssPath searchPath = path;

  searchPath.setStartDate( QString( '*' ) );

  if ( !considerInterval )
    searchPath.setTimeInterval( QString( '*' ) );

  status = zcatalog( mIfltab->data(), searchPath.c_pathString(), catStruct.get(), 0 );
  if ( status < 0 )
    return ret;

  int count = catStruct->numberPathnames;
  ret.reserve( count );
  for ( int i = 0; i < count; ++i )
    ret.append( ReosDssPath( QString( catStruct->pathnameList[i] ) ) );

  zstructFree( catStruct.release() );

  return ret;
}

void ReosDssFile::removeDataset( const ReosDssPath &path )
{
  const QList<ReosDssPath> recordPathes = searchRecordsPath( path, false );

  for ( const ReosDssPath &rp : recordPathes )
  {
    zdelete( mIfltab->data(), rp.c_pathString() );
  }
}


ReosDssPath::ReosDssPath()
{
  mData = QVector<QVector<char>>( 6 );
  mIsValid = true;
}

ReosDssPath::ReosDssPath( const QString &path )
{
  QStringList members = path.split( '/', Qt::SplitBehaviorFlags::KeepEmptyParts );
  if ( members.count() != 8 )
  {
    mIsValid = false;
    return;
  }
  members.removeFirst();
  members.removeLast();
  mData = QVector<QVector<char>>( 6 );
  for ( int i = 0; i < 6; ++i )
    stringToData( members.at( i ), static_cast<Part>( i ) );

  mIsValid = true;
}

char *ReosDssPath::c_string( Part part )
{
  return mData[part].data();
}

const char *ReosDssPath::const_c_string( Part part ) const
{
  return mData[part].data();
}

size_t ReosDssPath::size( Part part ) const
{
  return static_cast<size_t>( mData.at( part ).count() );
}

const QString ReosDssPath::string() const
{
  return '/' + group() + '/' + location() + '/' + parameter() + '/' + startDate() + '/' + timeInterval() + '/' + version() + '/';
}

const char *ReosDssPath::c_pathString() const
{
  mTempPathString = string().toStdString();
  return mTempPathString.c_str();
}

bool ReosDssPath::isValid() const
{
  return mIsValid;
}

const QString ReosDssPath::group() const
{
  return toQString( Group );
}

void ReosDssPath::setGroup( const QString &newGroup )
{
  mTempPathString.clear();
  stringToData( newGroup, Group );
}

const QString ReosDssPath::location() const
{
  return toQString( Location );
}

void ReosDssPath::setLocation( const QString &newLocation )
{
  mTempPathString.clear();
  stringToData( newLocation, Location );
}

const QString ReosDssPath::parameter() const
{
  return toQString( Parameter );
}

void ReosDssPath::setParameter( const QString &newParameter )
{
  mTempPathString.clear();
  stringToData( newParameter, Parameter );
}

const QString ReosDssPath::startDate() const
{
  return toQString( StartDate );
}

void ReosDssPath::setStartDate( const QString &newStartDate )
{
  mTempPathString.clear();
  stringToData( newStartDate, StartDate );
}

const QString ReosDssPath::timeInterval() const
{
  return toQString( TimeInterval );
}

void ReosDssPath::setTimeInterval( const ReosDuration &interval )
{
  mTempPathString.clear();
  stringToData( ReosDssUtils::durationToDssInterval( interval ), TimeInterval );
}

void ReosDssPath::setTimeInterval( const QString &newTimeInterval )
{
  mTempPathString.clear();
  stringToData( newTimeInterval, TimeInterval );
}

const QString ReosDssPath::version() const
{
  return toQString( Version );
}

void ReosDssPath::setVersion( const QString &newVersion )
{
  mTempPathString.clear();
  stringToData( newVersion, Version );
}

ReosDuration ReosDssPath::timeIntervalDuration() const
{
  return ReosDssUtils::dssIntervalToDuration( timeInterval() );
}

QString ReosDssPath::toQString( Part part ) const
{
  std::string str( mData.at( part ).data() );
  QString ret;
  ret = QString::fromStdString( str );
  return ret;
}

void ReosDssPath::stringToData( const QString &str, Part part )
{
  QString effStr = str;
  if ( effStr.size() < 64 )
    effStr.resize( 64 );
  QVector<char> strChar( str.count() + 1 );
  std::string source = str.toStdString();
  memcpy( strChar.data(), source.data(), source.size() );
  strChar[ str.count()] = '\0';

  mData[part] = strChar;
}
