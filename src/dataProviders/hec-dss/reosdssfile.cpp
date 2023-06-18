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
#include <cmath>

extern "C" {
#include "heclib.h"
}

#include "reosduration.h"
#include "reoshydrograph.h"
#include "reosgriddedrainitem.h"
#include "reosdssutils.h"
#include "reosmemoryraster.h"
#include "reosgisengine.h"


ReosDssFile::ReosDssFile( const QString &filePath, bool create )
  : mFileName( filePath )
{
  QFileInfo fileInfo( mFileName );
  if ( fileInfo.suffix() != QStringLiteral( "dss" ) )
  {
    mFileName = mFileName + ( QStringLiteral( ".dss" ) );
    fileInfo = QFileInfo( mFileName );
  }

  if ( !create && !fileInfo.exists() )
    return;
  else if ( create && fileInfo.exists() )
    if ( !QFile::remove( mFileName ) )
      return;

  open();
}

ReosDssFile::ReosDssFile( ReosDssFile &&other )
  : mIfltab( std::move( other.mIfltab ) )
  , mStatus( other.mStatus )
  , mIsValid( other.mIsValid )
  , mIsOpen( other.mIsOpen )
{
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

void ReosDssFile::open()
{
  mIfltab.reset( new std::array<long long, 250> );
  mStatus = zopen( mIfltab->data(), mFileName.toUtf8().data() );
  mIsOpen = mStatus == STATUS_OKAY;
  mIsValid = mIsOpen;
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

bool ReosDssFile::getSeries( const ReosDssPath &path, QVector<double> &values, ReosDuration &timeStep, QDateTime &startTime ) const
{
  if ( !path.isValid() )
    return false;
  ReosDssPath allDatapath = path;
  allDatapath.setStartDate( QString( '*' ) );

  const char *pthStr = allDatapath.c_pathString();
  zStructTimeSeries *timeSeries( zstructTsNew( pthStr ) );
  int status = ztsRetrieve( mIfltab->data(), timeSeries, -3, 2, 0 );

  if ( status == STATUS_OKAY )
  {
    int valueCount = timeSeries->numberValues;
    values.resize( valueCount );
    memcpy( values.data(), timeSeries->doubleValues, static_cast<size_t>( valueCount )*sizeof( double ) );

    int daySince1900 = timeSeries->startJulianDate;
    int startTimeSeconds = timeSeries->startTimeSeconds;

    const QDate date = QDate( 1900, 01, 01 ).addDays( daySince1900 - 1 );
    const QTime time = QTime::fromMSecsSinceStartOfDay( startTimeSeconds * 1000 );
    startTime = QDateTime( date, time, Qt::UTC );

    timeStep = ReosDuration( timeSeries->timeIntervalSeconds, ReosDuration::second );
  }

  zstructFree( timeSeries );

  return status == STATUS_OKAY;
}

bool ReosDssFile::getSeries( const ReosDssPath &path, const ReosTimeWindow &timeWindow, QVector<double> &values, ReosDuration &timeStep, QDateTime &startTime ) const
{
  if ( !path.isValid() )
    return false;

  if ( !timeWindow.isValid() )
    return getSeries( path, values, timeStep, startTime );

  ReosDssPath allDatapath = path;
  allDatapath.setStartDate( QString( '*' ) );
  const QString dssStartDate = ReosDssUtils::dateToDssDate( timeWindow.start().date() );
  const QString dssStartTime = ReosDssUtils::timeToDssTime( timeWindow.start().time() );
  const QString dssEndDate = ReosDssUtils::dateToDssDate( timeWindow.end().date() );
  const QString dssEndTime = ReosDssUtils::timeToDssTime( timeWindow.end().time() );
  zStructTimeSeries *timeSeries( zstructTsNewTimes( allDatapath.c_pathString(),
                                 dssStartDate.toUtf8().data(),
                                 dssStartTime.toUtf8().data(),
                                 dssEndDate.toUtf8().data(),
                                 dssEndTime.toUtf8().data() ) );

  int status = ztsRetrieve( mIfltab->data(), timeSeries, -3, 2, 0 );

  if ( status == STATUS_OKAY )
  {
    int valueCount = timeSeries->numberValues;
    values.resize( valueCount );
    memcpy( values.data(), timeSeries->doubleValues, static_cast<size_t>( valueCount )*sizeof( double ) );

    int daySince1900 = timeSeries->startJulianDate;
    int startTimeSeconds = timeSeries->startTimeSeconds;

    const QDate date = QDate( 1900, 01, 01 ).addDays( daySince1900 - 1 );
    const QTime time = QTime::fromMSecsSinceStartOfDay( startTimeSeconds * 1000 );
    startTime = QDateTime( date, time, Qt::UTC );

    timeStep = ReosDuration( timeSeries->timeIntervalSeconds, ReosDuration::second );
  }

  zstructFree( timeSeries );

  return status == STATUS_OKAY;
}

bool ReosDssFile::gridMinMax( const ReosDssPath &path, double &min, double &max ) const
{
  zStructSpatialGrid *grid = zstructSpatialGridNew( path.c_pathString() );
  int status = zspatialGridRetrieve( mIfltab->data(), grid, false );
  if ( status != STATUS_OKAY )
  {
    zstructFree( grid );
    return false;
  }

  min = *reinterpret_cast<float *>( grid->_minDataValue );
  max = *reinterpret_cast<float *>( grid->_maxDataValue );
  zstructFree( grid );
  return true;
}

ReosRasterExtent ReosDssFile::gridExtent( const ReosDssPath &path ) const
{
  zStructSpatialGrid *grid = zstructSpatialGridNew( path.c_pathString() );
  int status = zspatialGridRetrieve( mIfltab->data(), grid, false );

  if ( status != STATUS_OKAY )
  {
    zstructFree( grid );
    return ReosRasterExtent();
  }

  int xCellCount = grid->_numberOfCellsX;
  int yCellCount = grid->_numberOfCellsY;
  double cellSize = grid->_cellSize;

  double yBottom = cellSize * grid->_lowerLeftCellY;
  double xLeft = cellSize * grid->_lowerLeftCellX;


  ReosMapExtent extent( xLeft, yBottom, xLeft + xCellCount * cellSize, yBottom + yCellCount * cellSize );
  QString crs( grid->_srsDefinition );
  zstructFree( grid );

  extent.setCrs( crs );

  return ReosRasterExtent( extent, xCellCount, yCellCount );
}

QVector<double> ReosDssFile::gridValues( const ReosDssPath &path, int &xCount, int &yCount ) const
{
  zStructSpatialGrid *grid = zstructSpatialGridNew( path.c_pathString() );
  int status = zspatialGridRetrieve( mIfltab->data(), grid, true );
  if ( status != STATUS_OKAY )
  {
    xCount = 0;
    yCount = 0;
    zstructFree( grid );
    return QVector<double>();
  }

  xCount = grid->_numberOfCellsX;
  yCount = grid->_numberOfCellsY;

  QVector<double> ret( xCount * yCount );

  for ( int i = 0; i < xCount; ++i )
    for ( int j = 0; j < yCount; ++j )
    {
      float value = reinterpret_cast<float *>( grid->_data )[static_cast<size_t>( i + ( yCount - j - 1 ) * xCount )];
      if ( value == grid->_nullValue )
        ret[i + j * xCount] = std::numeric_limits<double>::quiet_NaN();
      else
        ret[i + j * xCount] = static_cast<double>( value );
    }

  zstructFree( grid );

  return ret;
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

template<typename T >
static void *allocValue( T value )
{
  void *ptr = malloc( sizeof( T ) * 1 );
  *( static_cast<T *>( ptr ) ) = value;

  return ptr;
}

static char *allocCopyString( const QString &string )
{
  size_t len = static_cast<size_t>( string.count() );
  char *str = ( char * )malloc( string.count() + 1 );
  std::string stdString = string.toStdString();
  strncpy( str, stdString.c_str(), len + 1 );
  return str;
}

//static char *utmGridDef( int zone, const char *utmHemi )
//{
//  char *falseNorthing = "0";
//  char _centralMeridian[sizeof( int )];
//  char tens[sizeof( int )];
//  char ones[sizeof( int )];
//  int len = sizeof( UTM_SRC_DEFINITION );
//  char *utm = ( char * )malloc( sizeof( char ) * len );

//  if ( strcmp( utmHemi, "S" ) == 0 )
//    falseNorthing = "10000000";
//  int centralMeridian = -183 + zone * 6;

//  snprintf( _centralMeridian, sizeof( int ), "%d", centralMeridian );
//  snprintf( tens, sizeof( int ), "%.f", fmodf( zone, 10 ) );
//  snprintf( ones, sizeof( int ), "%d", zone / 10 );

//  snprintf( utm, len, UTM_SRC_DEFINITION, ones, tens, _centralMeridian, falseNorthing );

//  return utm;
//}

bool ReosDssFile::writeGriddedData( ReosGriddedRainfall *griddedrainFall, const ReosDssPath &path )
{
  return writeGriddedDataPrivate( griddedrainFall, path, griddedrainFall->extent(), -1, ReosTimeWindow(), true );
}

bool ReosDssFile::writeGriddedData(
  ReosGriddedRainfall *griddedRainFall,
  const ReosDssPath &path,
  const ReosMapExtent &destination,
  double resolution,
  const ReosTimeWindow &timeWindow )
{
  return writeGriddedDataPrivate( griddedRainFall, path, destination, resolution, timeWindow, false );
}

bool ReosDssFile::writeGriddedDataPrivate(
  ReosGriddedRainfall *griddedRainFall,
  const ReosDssPath &path,
  const ReosMapExtent &destination,
  double resolution,
  const ReosTimeWindow &timeWindow,
  bool reduce )
{
  bool res = true;
  const ReosRasterExtent extent = griddedRainFall->rasterExtent();

  ReosMapExtent destinationFullExtent = ReosGisEngine::transformExtent( extent, destination.crs() );
  double destResolution = resolution;
  if ( destResolution < 0 )
  {
    double dx = destinationFullExtent.width() / extent.xCellCount();
    double dy = destinationFullExtent.height() / extent.yCellCount();
    destResolution = std::min( dx, dy );
  }

  int xCellBottomLeft;
  int ycellBottomLeft;
  int xCellTopRight;
  int yCellTopRight;

  if ( reduce )
  {
    xCellBottomLeft = static_cast<int>( std::ceil( destination.xMapMin() / destResolution ) );
    ycellBottomLeft = static_cast<int>( std::ceil( destination.yMapMin() / destResolution ) );
    xCellTopRight = static_cast<int>( std::floor( destination.xMapMax() / destResolution ) ) ;
    yCellTopRight = static_cast<int>( std::floor( destination.yMapMax() / destResolution ) ) ;
  }
  else
  {
    xCellBottomLeft = static_cast<int>( std::floor( destination.xMapMin() / destResolution ) );
    ycellBottomLeft = static_cast<int>( std::floor( destination.yMapMin() / destResolution ) );
    xCellTopRight =  static_cast<int>( std::ceil( destination.xMapMax() / destResolution ) ) ;
    yCellTopRight = static_cast<int>( std::ceil( destination.yMapMax() / destResolution ) ) ;
  }

  double xMinDestin = xCellBottomLeft * destResolution;
  double yMinDestin = ycellBottomLeft * destResolution;
  double xMaxDestin = ( xCellTopRight + 1 ) * destResolution;
  double yMaxDestin = ( yCellTopRight + 1 ) * destResolution;

  ReosMapExtent destExtent( xMinDestin, yMinDestin, xMaxDestin, yMaxDestin );
  destExtent.setCrs( destination.crs() );

  std::unique_ptr<ReosGriddedRainfall> transformedGriddedRainfall(
    griddedRainFall->transform( destExtent, destResolution, destResolution, timeWindow ) );

  ReosRasterExtent transformedExtent = transformedGriddedRainfall->rasterExtent();

  for ( int i = 0; i < transformedGriddedRainfall->gridCount(); ++i )
  {
    ReosDssPath effPath = path;
    const QDateTime startDateTime = transformedGriddedRainfall->startTime( i );
    const QDateTime endDateTime = transformedGriddedRainfall->endTime( i );

    effPath.setParameter( QStringLiteral( "PRECIP" ) );
    effPath.setStartDate( ReosDssUtils::dateToHecRasDate( startDateTime.date() ) + ':' + startDateTime.time().toString( "HHmm" ) );
    effPath.setTimeInterval( ReosDssUtils::dateToHecRasDate( endDateTime.date() ) + ':' + endDateTime.time().toString( "HHmm" ) );
    zStructSpatialGrid *grid = zstructSpatialGridNew( effPath.c_pathString() );

    grid->_type = 430;     //*************************
    grid->_version = 1;
    QString units = "MM";  //*************************
    grid->_dataUnits = allocCopyString( units );
    grid->_dataType = PER_CUM;
    QString source = "INTERNAL";
    grid->_dataSource = allocCopyString( source );

    grid->_compressionMethod = ZLIB_COMPRESSION;
    grid->_cellSize = static_cast<float>( std::fabs( transformedExtent.xCellSize() ) );
    grid->_isInterval = 1;
    grid->_isTimeStamped = 1;

    grid->_numberOfCellsX = transformedExtent.xCellCount();
    grid->_numberOfCellsY = transformedExtent.yCellCount();
    grid->_lowerLeftCellX = xCellBottomLeft;
    grid->_lowerLeftCellY = ycellBottomLeft;

    grid->_xCoordOfGridCellZero = 0.0;
    grid->_yCoordOfGridCellZero = 0.0;
    grid->_srsDefinitionType = 0;
    QString crsName = ( "---" );
    grid->_srsName = allocCopyString( crsName );
    QString crs = ReosGisEngine::crsEsriWkt( transformedExtent.crs() );;
    grid->_srsDefinition = allocCopyString( crs );

    const QVector<double> values = transformedGriddedRainfall->intensityValues( i );
    float min = std::numeric_limits<float>::max();
    float max = -std::numeric_limits<float>::max();
    double mean = 0;
    double noData = -99999.0;
    grid->_data = malloc( sizeof( float ) * values.count() );
    for ( int di = 0; di < values.count(); ++di )
    {
      float val = static_cast<float>( values.at( di ) );
      if ( std::isnan( val ) )
      {
        static_cast<float *>( grid->_data )[static_cast<size_t>( di )] = noData;
        continue;
      }
      static_cast<float *>( grid->_data )[static_cast<size_t>( di )] = val;
      if ( val  < min )
        min = val;
      if ( val > max )
        max = val;
      mean += val;
    }
    grid->_minDataValue = allocValue( min );
    grid->_maxDataValue = allocValue( max );
    grid->_meanDataValue = allocValue( static_cast<float>( mean / values.count() ) );
    grid->_compressionParameters = malloc( sizeof( float ) * 2 );
    grid->_nullValue = noData;
    static_cast<float *>( grid->_compressionParameters )[0] = 0.0;
    static_cast<float *>( grid->_compressionParameters )[1] = 0.0;

    res = zspatialGridStore( mIfltab->data(), grid ) == STATUS_OKAY;
    zstructFree( grid );
    if ( !res )
      break;
  }
  return res;
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

  zStructTimeSeries *timeSerie(
    zstructTsNewRegDoubles( pathToWrite.c_pathString(),
                            const_cast<double *>( values.data() ),
                            values.count(),
                            strDate.data(),
                            strTime.data(),
                            "", // for now, we do not care about unit
                            "" ) ); // for now, we do not care about type f value

  int res = ztsStore( mIfltab->data(), timeSerie, 0 );
  zstructFree( timeSerie );

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
  zStructCatalog *catStruct( zstructCatalogNew() );
  int status;
  ReosDssPath searchPath = path;

  searchPath.setStartDate( QString( '*' ) );

  if ( !considerInterval )
    searchPath.setTimeInterval( QString( '*' ) );

  status = zcatalog( mIfltab->data(), searchPath.c_pathString(), catStruct, 0 );
  if ( status < 0 )
  {
    zstructFree( catStruct );
    return ret;
  }

  int count = catStruct->numberPathnames;
  ret.reserve( count );
  for ( int i = 0; i < count; ++i )
    ret.append( ReosDssPath( QString( catStruct->pathnameList[i] ) ) );

  zstructFree( catStruct );

  return ret;
}

QList<ReosDssPath> ReosDssFile::allPathes() const
{
  ReosDssPath path;
  path.setGroup( QString( '*' ) );
  path.setLocation( QString( '*' ) );
  path.setParameter( QString( '*' ) );
  path.setVersion( QString( '*' ) );

  return searchRecordsPath( path, false );
}

ReosDssFile::RecordInfo ReosDssFile::recordInformation( const ReosDssPath &path ) const
{
  RecordInfo ret;

  zStructRecordBasics *recordBasics = zstructRecordBasicsNew( path.c_pathString() );
  int status = zgetRecordBasics( mIfltab->data(), recordBasics );

  if ( status < 0 )
  {
    zstructFree( recordBasics );
    return ret;
  }

  ret.version = recordBasics->version;
  ret.recordType = recordBasics->recordType;

  zstructFree( recordBasics );
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
  if ( !mIsValid )
    return QString();
  return toQString( Group );
}

void ReosDssPath::setGroup( const QString &newGroup )
{
  mTempPathString.clear();
  stringToData( newGroup, Group );
}

const QString ReosDssPath::location() const
{
  if ( !mIsValid )
    return QString();
  return toQString( Location );
}

void ReosDssPath::setLocation( const QString &newLocation )
{
  mTempPathString.clear();
  stringToData( newLocation, Location );
}

const QString ReosDssPath::parameter() const
{
  if ( !mIsValid )
    return QString();
  return toQString( Parameter );
}

void ReosDssPath::setParameter( const QString &newParameter )
{
  mTempPathString.clear();
  stringToData( newParameter, Parameter );
}

const QString ReosDssPath::startDate() const
{
  if ( !mIsValid )
    return QString();
  return toQString( StartDate );
}

void ReosDssPath::setStartDate( const QString &newStartDate )
{
  mTempPathString.clear();
  stringToData( newStartDate, StartDate );
}

const QString ReosDssPath::timeInterval() const
{
  if ( !mIsValid )
    return QString();
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
  if ( !mIsValid )
    return QString();
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

bool ReosDssPath::isEquivalent( const ReosDssPath &other ) const
{
  bool cond1 = ( group().isEmpty() || other.group().isEmpty() || group().toLower() == other.group().toLower() );
  bool cond2 = ( location().isEmpty() || other.location().isEmpty() || location().toLower() == other.location().toLower() ) ;
  bool cond3 = ( parameter().isEmpty() || other.parameter().isEmpty() || parameter().toLower() == other.parameter().toLower() ) ;
  bool cond4 = ( version().isEmpty() || other.version().isEmpty() || version().toLower() == other.version().toLower() );

  return cond1 && cond2 && cond3 && cond4;

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
