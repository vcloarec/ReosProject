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
  if ( mIfltab && mIsOpen )
    zclose( mIfltab->data() );
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

bool ReosDssFile::pathExist( const ReosDssPath &path ) const
{
  if ( hasData( path ) )
    return true;
  // maybe the path is presents but dataset contains no valid value, so no record.
  // we need to search in the catalog

  QList<ReosDssPath> pathes = searchRecordsPath( path );
  return !pathes.empty();
}

bool ReosDssFile::hasData( const ReosDssPath &path ) const
{
  return zcheck( mIfltab->data(), path.c_pathString() ) == STATUS_OKAY;
}

bool ReosDssFile::createConstantIntervalSeries( const ReosDssPath &path, QString &error )
{
  if ( !mIsOpen || !mIsValid || !mIfltab )
  {
    error = QObject::tr( "DSS file \"%1\" is not valid." ).arg( mFileName );
    return false;
  }

  ReosDuration intervalDuration = path.timeIntervalDuration();
  if ( intervalDuration == ReosDuration() )
  {
    error = QObject::tr( "The time interval in the path is not valid." );
    return false;
  }

  QVector<double> values;
  values.append( UNDEFINED_DOUBLE ); //DSS need at leastone value, we give it an undefined one because our time serie is void

  //As there is no value, time is not important, it will be override when data will be written in the file
  QDate startDate = QDate::currentDate();
  QTime startTime = QTime( 0, 0, 0 );

  return writeConstantIntervalSeriesPrivate( path, QDateTime( startDate, startTime ), intervalDuration, values, error );
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

  ReosDuration intervalDuration = path.timeIntervalDuration();
  if ( intervalDuration == ReosDuration() )
  {
    error = QObject::tr( "The time interval in the path is not valid." );
    return false;
  }

  ReosDssPath pathToRemove = path;
  pathToRemove.setTimeInterval( QString( '*' ) ); //we need to remove all time interval
  removeDataset( pathToRemove );

  return writeConstantIntervalSeriesPrivate( path, startTime, timeStep, values, error );
}

QDateTime ReosDssFile::referenceTime( const ReosDssPath &path ) const
{
  if ( !mIsValid || !mIfltab )
    return QDateTime();

  int startJulian = 0;
  int startSeconds = 0;
  int endJulian = 0;
  int endSeconds = 0;
  std::string str = path.string().toStdString();
  const char *pth = str.c_str();
  int res = ztsGetDateTimeRange( mIfltab->data(), pth, 0,
                                 &startJulian, &startSeconds, &endJulian, &endSeconds );

  if ( res == STATUS_OKAY )
  {
    return QDateTime();
  }

  return QDateTime();
}

ReosDuration ReosDssFile::timeInterval( const ReosDssPath &path ) const
{
  return ReosDuration();
}

QVector<double> ReosDssFile::values( const ReosDssPath &path ) const
{
  return QVector<double>();
}
  return false;
}

ReosDuration ReosDssFile::closestValidInterval( const ReosDuration &interval )
{
  QList<ReosDuration> validInterval;
  validInterval << ReosDuration( 1, ReosDuration::second )
                << ReosDuration( 2, ReosDuration::second )
                << ReosDuration( 3, ReosDuration::second )
                << ReosDuration( 4, ReosDuration::second )
                << ReosDuration( 5, ReosDuration::second )
                << ReosDuration( 6, ReosDuration::second )
                << ReosDuration( 10, ReosDuration::second )
                << ReosDuration( 15, ReosDuration::second )
                << ReosDuration( 20, ReosDuration::second )
                << ReosDuration( 30, ReosDuration::second )
                << ReosDuration( 1, ReosDuration::minute )
                << ReosDuration( 2, ReosDuration::minute )
                << ReosDuration( 3, ReosDuration::minute )
                << ReosDuration( 4, ReosDuration::minute )
                << ReosDuration( 5, ReosDuration::minute )
                << ReosDuration( 6, ReosDuration::minute )
                << ReosDuration( 10, ReosDuration::minute )
                << ReosDuration( 12, ReosDuration::minute )
                << ReosDuration( 15, ReosDuration::minute )
                << ReosDuration( 20, ReosDuration::second )
                << ReosDuration( 30, ReosDuration::second )
                << ReosDuration( 1, ReosDuration::hour )
                << ReosDuration( 2, ReosDuration::hour )
                << ReosDuration( 3, ReosDuration::hour )
                << ReosDuration( 4, ReosDuration::hour )
                << ReosDuration( 6, ReosDuration::hour )
                << ReosDuration( 8, ReosDuration::hour )
                << ReosDuration( 12, ReosDuration::hour )
                << ReosDuration( 1, ReosDuration::day )
                << ReosDuration( 1, ReosDuration::week )
                << ReosDuration( 10, ReosDuration::day )
                << ReosDuration( 15, ReosDuration::day )
                << ReosDuration( 1, ReosDuration::month )
                << ReosDuration( 1, ReosDuration::year );

  for ( int i = 0; i < validInterval.count() - 1; ++i )
  {
    const ReosDuration &vi1 =  validInterval.at( i );
    const ReosDuration &vi2 =  validInterval.at( i + 1 );

    if ( interval <= vi1 )
      return vi1;

    if ( interval == vi2 )
      return vi2;

    if ( interval > vi1 && interval < vi2 )
    {
      if ( interval - vi1 < vi2 - interval )
        return vi1;
      else
        return vi2;
    }
  }

  return validInterval.last();
}

QString ReosDssFile::getEPart( const ReosDuration &interval, bool findClosest )
{
  std::vector<char> ret;
  ret.resize( 64 );

  int seconds;
  if ( findClosest )
    seconds = static_cast<int>( closestValidInterval( interval ).valueSecond() + 0.5 );
  else
    seconds = static_cast<int>( interval.valueSecond() + 0.5 );

  int res = ztsGetEPartFromInterval( seconds, ret.data(), ret.size() );

  if ( res != STATUS_OKAY )
    return QString();

  return QString::fromStdString( std::string( ret.data() ) );
}

QList<ReosDssPath> ReosDssFile::searchRecordsPath( const ReosDssPath &path ) const
{
  QList<ReosDssPath> ret;
  std::unique_ptr<zStructCatalog> catStruct( zstructCatalogNew() );
  int status;
  ReosDssPath searchPath = path;
  searchPath.setStartDate( QString( '*' ) );
  status = zcatalog( mIfltab->data(), searchPath.c_pathString(), catStruct.get(), 0 );
  if ( status < 0 )
    return ret;

  int count = catStruct->numberPathnames;
  ret.reserve( count );
  for ( int i = 0; i < count; ++i )
    ret.append( ReosDssPath( QString( catStruct->pathnameList[i] ) ) );

  return ret;
}

void ReosDssFile::removeDataset( const ReosDssPath &path )
{
  const QList<ReosDssPath> recordPathes = searchRecordsPath( path );

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
  stringToData( newGroup, Group );
}

const QString ReosDssPath::location() const
{
  return toQString( Location );
}

void ReosDssPath::setLocation( const QString &newLocation )
{
  stringToData( newLocation, Location );
}

const QString ReosDssPath::parameter() const
{
  return toQString( Parameter );
}

void ReosDssPath::setParameter( const QString &newParameter )
{
  stringToData( newParameter, Parameter );
}

const QString ReosDssPath::startDate() const
{
  return toQString( StartDate );
}

void ReosDssPath::setStartDate( const QString &newStartDate )
{
  stringToData( newStartDate, StartDate );
}

const QString ReosDssPath::timeInterval() const
{
  return toQString( TimeInterval );
}

void ReosDssPath::setTimeInterval( const QString &newTimeInterval )
{
  stringToData( newTimeInterval, TimeInterval );
}

const QString ReosDssPath::version() const
{
  return toQString( Version );
}

void ReosDssPath::setVersion( const QString &newVersion )
{
  stringToData( newVersion, Version );
}

ReosDuration ReosDssPath::timeIntervalDuration() const
{
  int interValSeconds = 0;
  int flagDirection = 1;
  const QString timeItervalString = timeInterval();
  size_t strSize = size( ReosDssPath::TimeInterval );
  std::vector<char> str( strSize );
  memcpy( str.data(), const_c_string( ReosDssPath::TimeInterval ), size( ReosDssPath::TimeInterval )*sizeof( char ) );

  if ( !timeItervalString.isEmpty() &&
       ( ztsGetStandardInterval( 7,
                                 &interValSeconds,
                                 str.data(),
                                 static_cast<size_t>( timeInterval().size() ),
                                 &flagDirection ) == STATUS_OKAY ) )
  {
    return ReosDuration( interValSeconds, ReosDuration::second );
  }

  return ReosDuration();
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
  QVector<char> strChar( str.count() + 1 );
  std::string source = str.toStdString();
  memcpy( strChar.data(), source.data(), source.size() );
  strChar[ str.count()] = '\0';

  mData[part] = strChar;
}
