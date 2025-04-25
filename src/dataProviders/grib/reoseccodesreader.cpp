#include "reoseccodesreader.h"
#include "iostream"
#include <QDebug>
#include <QDateTime>
#include <QFile>

#include "gdal.h"
#include "ogr_srs_api.h"


static bool handleError( int error )
{
  return error == 0;
}

static void printError( int error_code )
{
  std::string error_message( codes_get_error_message( error_code ) );
  std::cout << "ECCODES Error: " << error_message << std::endl;
}


static QString getString( codes_handle *handle, const char *key )
{
  size_t length = 0;
  handleError( codes_get_length( handle, key, &length ) );
  char str[length];
  handleError( codes_get_string( handle, key, str, &length ) );

  return QString::fromUtf8( str );
}

static QVariant getKeyValue( codes_handle *handle, const char *key )
{
  int type_code = 0;
  handleError( codes_get_native_type( handle, key, &type_code ) );

  switch ( type_code )
  {
    case CODES_TYPE_STRING:
      return QVariant::fromValue( getString( handle, key ) );
      break;
    case CODES_TYPE_LONG:
    {
      long ret = 0;
      handleError( codes_get_long( handle, key, &ret ) );
      return QVariant::fromValue( ret );
    }
    case CODES_TYPE_DOUBLE:
    {
      double ret = 0;
      handleError( codes_get_double( handle, key, &ret ) );
      return QVariant::fromValue( ret );
    }
    default:
      return QVariant();
      break;
  }
}

static QVariantMap keys( codes_handle *handle )
{
  QVariantMap keysMap;
  if ( !handle )
    return keysMap;
  codes_keys_iterator *kit = codes_keys_iterator_new( handle, 0, nullptr );
  while ( codes_keys_iterator_next( kit ) )
  {
    const char *key = codes_keys_iterator_get_name( kit );
    QString keyName = QString::fromUtf8( key );
    keysMap.insert( key, getKeyValue( handle, key ) );
  }
  codes_keys_iterator_delete( kit );

  return keysMap;
}

static QString projStringToWkt( const QString &projString )
{
  // Create an OGRSpatialReferenceH object( Handle - based API )
  OGRSpatialReferenceH hSRS = OSRNewSpatialReference( NULL );
  if ( OSRImportFromProj4( hSRS, projString.toUtf8() ) != OGRERR_NONE )
  {
    OSRDestroySpatialReference( hSRS );
    return QString();
  }

  // Convert to WKT format
  char *wkt = NULL;
  if ( OSRExportToWkt( hSRS, &wkt ) != OGRERR_NONE )
  {
    fprintf( stderr, "Failed to convert to WKT\n" );
    OSRDestroySpatialReference( hSRS );
    return QString();
  }

  return QString::fromUtf8( wkt );
}

static ReosEcCodesGridDescritpion gridDescription( const ReosEcCodesReaderKeys &keys )
{
  ReosEcCodesGridDescritpion ret;

  const QString gridType = keys.stringValue( QStringLiteral( "gridType" ) );
  if ( gridType == QStringLiteral( "regular_ll" ) )
  {
    if ( keys.hasKey( QStringLiteral( "shapeOfTheEarth" ) ) )
    {
      int earthShapeCode = keys.longValue( QStringLiteral( "shapeOfTheEarth" ) );
      switch ( earthShapeCode )
      {
        case 6:
          ret.wktCrs = projStringToWkt( QString( "+proj=longlat +a=%1 +b=%1 +no_defs" ).arg( 6371229 ) );
          break;
        default:
          break;
      }
    }
    else if ( keys.hasKey( QStringLiteral( "earthIsOblate" ) ) )
    {
      //https://codes.ecmwf.int/grib/format/edition-independent/1/19/
      long  earthIsOblate = keys.longValue( QStringLiteral( "earthIsOblate" ) );
      if ( earthIsOblate == 0 )
        //https://confluence.ecmwf.int/pages/viewpage.action?pageId=44245972
      {
        long radius = 0;
        if ( keys.hasKey( QStringLiteral( "radius" ) ) )
          radius = keys.longValue( QStringLiteral( "radius" ) );
        else
          radius = 6367470;

        ret.wktCrs = projStringToWkt( QString( "+proj=longlat +a=%1 +b=%1 +no_defs" ).arg( radius ) );
      }

    }

    ret.width = keys.longValue( QStringLiteral( "Ni" ) );
    ret.height = keys.longValue( QStringLiteral( "Nj" ) );

    double firstLat = keys.doubleValue( QStringLiteral( "latitudeOfFirstGridPointInDegrees" ) );
    double lastLat = keys.doubleValue( QStringLiteral( "latitudeOfLastGridPointInDegrees" ) );
    double firstLon = keys.doubleValue( QStringLiteral( "longitudeOfFirstGridPointInDegrees" ) );
    double lastLon = keys.doubleValue( QStringLiteral( "longitudeOfLastGridPointInDegrees" ) );

    if ( firstLon >= 180.0 )
      firstLon = -360 + firstLon;
    if ( lastLon >= 180.0 )
      lastLon = -360 + firstLon;

    double pixelWidth = std::abs( firstLon - lastLon ) / ( ret.width - 1 );
    double pixelHeight = std::abs( firstLat - lastLat ) / ( ret.height - 1 );


    if ( keys.longValue( QStringLiteral( "iScansNegatively" ) ) == 1 )
    {
      ret.east = firstLon;
      ret.west = lastLon;
    }
    else
    {
      ret.east = lastLon;
      ret.west = firstLon;
    }

    ret.west = ret.west - pixelWidth / 2;
    ret.east = ret.east + pixelWidth / 2;

    if ( keys.longValue( QStringLiteral( "jScansNegatively" ) ) == 1 )
    {
      ret.north = firstLat;
      ret.south = lastLat;
      ret.yAscendant = false;
    }
    else
    {
      ret.north = lastLat;
      ret.south = firstLat;
      ret.yAscendant = true;
    }

    ret.north = ret.north + pixelHeight / 2;
    ret.south = ret.south - pixelHeight / 2;

    return ret;
  }

  return ReosEcCodesGridDescritpion();
}


ReosEcCodesReader::ReosEcCodesReader( const QString &gribFileName, const QVariantMap &variableKeys )
  : mFileName( gribFileName )
  , mVariableKeys( variableKeys )
  , mIndex( gribFileName, variableKeys )
{
  int error = 0;

  mIsValid = mIndex.isValid();
}

ReosEcCodesReader::~ReosEcCodesReader()
{
}

static ReosRasterExtent extentFromKeys( const ReosEcCodesReaderKeys &keys )
{
  const ReosEcCodesGridDescritpion grid = gridDescription( keys );
  ReosMapExtent mapExtent( grid.west, grid.south, grid.east, grid.north );
  mapExtent.setCrs( grid.wktCrs );
  return ReosRasterExtent( mapExtent, grid.width, grid.height, true, grid.yAscendant );
}

static QDateTime intToTime( int dateInt, int timeInt )
{
  int y = int( std::round( dateInt / 10000.0 ) );
  int m = int( std::round( ( dateInt - y * 10000.0 ) / 100.0 ) );
  int d = dateInt - y * 10000.0 - m * 100;

  int h = int( std::round( timeInt / 100.0 ) );
  int mi = timeInt - h * 100;

  return QDateTime( QDate( y, m, d ), QTime( h, mi ), Qt::UTC );
}

static QDateTime dataTimeFromKeys( const ReosEcCodesReaderKeys &keys )
{
  int dateInt = keys.longValue( QStringLiteral( "dataDate" ) );
  int timeInt = keys.longValue( QStringLiteral( "dataTime" ) );

  return intToTime( dateInt, timeInt );
}

static QDateTime validityTimeFromKeys( const ReosEcCodesReaderKeys &keys )
{
  int dateInt = keys.longValue( QStringLiteral( "validityDate" ) );
  int timeInt = keys.longValue( QStringLiteral( "validityTime" ) );

  return intToTime( dateInt, timeInt );
}

static QPair<int, int> stepRangeFromKeys( const ReosEcCodesReaderKeys &keys )
{
  int start = keys.longValue( QStringLiteral( "startStep" ) );
  int end = keys.longValue( QStringLiteral( "endStep" ) );

  if ( start == -1 || end == -1 )
  {
    QString startStr = keys.stringValue( QStringLiteral( "startStep" ) );
    QString endStr = keys.stringValue( QStringLiteral( "endStep" ) );

    if ( startStr.contains( 'm' ) )
      startStr = startStr.remove( 'm' );

    if ( endStr.contains( 'm' ) )
      endStr = endStr.remove( 'm' );

    start = startStr.toInt();
    end = endStr.toInt();
  }

  return QPair<int, int>( start, end );
}

static ReosDuration stepDurationFromKeys( const ReosEcCodesReaderKeys &keys )
{
  int stepUnit = keys.longValue( QStringLiteral( "stepUnits" ) );
  ReosDuration::Unit unit;

  switch ( stepUnit )
  {
    case 0:
      unit = ReosDuration::minute;
      break;
    case 1:
      unit = ReosDuration::hour;
      break;
    default:
      unit = ReosDuration::hour;
      break;
  }

  QPair<int, int> range = stepRangeFromKeys( keys );

  return ReosDuration( range.second - range.first, unit );
}

static ReosEcCodesReader::StepType stepTypeFromKeys( const ReosEcCodesReaderKeys &keys )
{
  QString strType = keys.stringValue( QStringLiteral( "stepType" ) );

  if ( strType == QStringLiteral( "accum" ) )
    return ReosEcCodesReader::StepType::Accum;

  if ( strType == QStringLiteral( "instant" ) )
    return ReosEcCodesReader::StepType::Instant;

  return ReosEcCodesReader::StepType::Unknown;
}

bool ReosEcCodesReader::nextFrameMetadata( ReosEcCodesReader::FrameMetadata &meta ) const
{
  bool ok = false;
  codes_handle *handle = mIndex.nextHandle( ok );
  if ( !ok )
  {
    codes_handle_delete( handle );
    return false;
  }

  if ( handle )
  {
    ReosEcCodesReaderKeys keys( handle );
    codes_handle_delete( handle );

    meta.extent = extentFromKeys( keys );
    meta.dataTime = dataTimeFromKeys( keys );
    meta.validityTime = validityTimeFromKeys( keys );
    meta.stepRange = stepRangeFromKeys( keys );
    meta.stepDuration = stepDurationFromKeys( keys );
    meta.stepType = stepTypeFromKeys( keys );
    return true;
  }

  return false;
}

bool ReosEcCodesReader::isValid() const
{
  return mIsValid;
}

int ReosEcCodesReader::frameCount() const
{
  if ( mFrameCount < 0 )
  {
    bool ok = false;
    while ( codes_handle *handle = mIndex.nextHandle( ok ) )
    {
      if ( !ok )
      {
        codes_handle_delete( handle );
        break;
      }
      mFrameCount++;
      codes_handle_delete( handle );
    }
  }
  return mFrameCount;
}

ReosEcCodesReaderKeys ReosEcCodesReader::keys( int frameIndex ) const
{
  return getKeys( frameIndex );

}


ReosRasterExtent ReosEcCodesReader::extent( int frameIndex ) const
{
  const ReosEcCodesReaderKeys &keys = getKeys( frameIndex );
  return extentFromKeys( keys );
}

ReosRasterMemory<double> ReosEcCodesReader::values( int index ) const
{
  if ( mCacheValuesIndex == index )
    return mCacheValues;

  codes_handle *handle = findHandle( index );

  ReosRasterMemory<double> ret;
  if ( handle )
  {
    const ReosEcCodesReaderKeys &keys( handle );

    bool jIncrementFirst = keys.longValue( QStringLiteral( "jScansPositively" ) ) == 1;

    size_t arraySize = 0;
    if ( ! handleError( codes_get_size( handle, "values", &arraySize ) ) )
      return ret;

    QVector<double> data;
    data.resize( int( arraySize ) );

    if ( !handleError( codes_get_double_array( handle, "values", data.data(), &arraySize ) ) )
      return ret;

    codes_handle_delete( handle );

    double missingValue = keys.doubleValue( QStringLiteral( "missingValue" ) );
    if ( std::isnan( missingValue ) )
      missingValue = keys.longValue( QStringLiteral( "missingValue" ) );

    for ( int i = 0; i < data.count(); ++i )
      if ( data.at( i ) == missingValue )
        data[i] = std::numeric_limits<double>::quiet_NaN();

    ReosEcCodesGridDescritpion gridDesc = gridDescription( keys );
    ret.reserveMemory( gridDesc.height, gridDesc.width );

    ret.setValues( data );
  }
  mCacheValues = ret;
  mCacheValuesIndex = index;
  return ret;
}

QDateTime ReosEcCodesReader::dataTime( int index ) const
{
  const ReosEcCodesReaderKeys keys = getKeys( index );
  return dataTimeFromKeys( keys );
}

QDateTime ReosEcCodesReader::validityTime( int index ) const
{
  const ReosEcCodesReaderKeys keys = getKeys( index );
  return validityTimeFromKeys( keys );
}

QPair<int, int> ReosEcCodesReader::stepRange( int index ) const
{
  const ReosEcCodesReaderKeys &keys = getKeys( index );
  return stepRangeFromKeys( keys );
}

ReosDuration ReosEcCodesReader::stepDuration( int index ) const
{
  const ReosEcCodesReaderKeys &keys = getKeys( index );
  return stepDurationFromKeys( keys );
}

ReosEcCodesReader::StepType ReosEcCodesReader::stepType( int index ) const
{
  const ReosEcCodesReaderKeys &keys = getKeys( index );
  return stepTypeFromKeys( keys );
}

codes_handle *ReosEcCodesReader::findHandle( int index ) const
{
  return mIndex.handle( index );
}

const ReosEcCodesReaderKeys &ReosEcCodesReader::getKeys( int index ) const
{
  if ( index != mCacheKeysIndex )
  {
    codes_handle *handle = findHandle( index );
    mCacheKeys = ReosEcCodesReaderKeys( handle );
    mCacheKeysIndex = index;
    codes_handle_delete( handle );
  }

  return mCacheKeys;
}

QList<ReosEcCodesReader::Variable> ReosEcCodesReader::variables( const QString &fileName )
{
  QMap < QString, ReosEcCodesReader::Variable >  ret;
  int error = 0;

  FILE *file = fopen( fileName.toUtf8(), "rb" );

  if ( !file )
    return QList<ReosEcCodesReader::Variable>();

  int count = 0;

  codes_handle *handle = nullptr;
  while ( true )
  {
    handle = codes_handle_new_from_file( nullptr, file, PRODUCT_GRIB, &error );
    if ( handleError( error ) )
    {
      if ( !handle )
        break;
    }
    else
      break;

    ReosEcCodesReaderKeys keysMap( handle );
    ReosEcCodesGridDescritpion grid = gridDescription( keysMap );

    codes_handle_delete( handle );
    const QString &name = keysMap.stringValue( "name" );
    const QString shortName = keysMap.stringValue( "shortName" );

    auto it = ret.constFind( name );
    if ( it == ret.constEnd() )
      ret.insert( name, Variable( {name, shortName} ) );

    count++;
  }

  fclose( file );

  return ret.values();
}


ReosEcCodesReaderKeys::ReosEcCodesReaderKeys( codes_handle *handle )
  : mMap( keys( handle ) )
{
}

bool ReosEcCodesReaderKeys::hasKey( const QString &key ) const
{
  return mMap.contains( key );
}

QString ReosEcCodesReaderKeys::stringValue( const QString &key ) const
{
  auto it = mMap.find( key );
  if ( it != mMap.constEnd() )
  {
    if ( it.value().isValid() && it.value().type() == QVariant::String )
    {
      return it.value().toString();
    }
  }

  return QString();
}

long ReosEcCodesReaderKeys::longValue( const QString &key ) const
{
  auto it = mMap.find( key );
  if ( it != mMap.constEnd() )
  {
    if ( it.value().isValid() && it.value().type() == QMetaType::Long )
    {
      bool ok = false;
      long val = it.value().toLongLong( &ok );
      if ( ok )
        return val;
    }
  }

  return -1;
}

double ReosEcCodesReaderKeys::doubleValue( const QString &key ) const
{
  auto it = mMap.find( key );
  if ( it != mMap.constEnd() )
  {
    if ( it.value().isValid() && it.value().type() == QMetaType::Double )
    {
      bool ok = false;
      double val = it.value().toDouble( &ok );
      if ( ok )
        return val;
    }
  }

  return std::numeric_limits<double>::quiet_NaN();
}

QVariantMap ReosEcCodesReaderKeys::map() const
{
  return mMap;
}

void ReosEcCodesDataset::addMessage( const QDateTime &startTime, FILE *file, long posInFile )
{
  mMessages.push_back( std::make_unique<ReosEcCodesMessage>( file, posInFile ) );
}

int ReosEcCodesDataset::frameCount() const
{
  return static_cast<int>( mMessages.size() );
}

ReosEcCodesMessage::ReosEcCodesMessage( FILE *file, long posInFile )
  : mFile( file )
  , mPosInFIle( posInFile )
{

}

ReosEcCodesMessage::~ReosEcCodesMessage()
{
}

ReosEcCodesIndex::ReosEcCodesIndex( const QString &fileName, const QVariantMap &variableKeys )
  : mFileName( fileName )
  , mKeys( variableKeys )
{
  reset();
}

void ReosEcCodesIndex::reset()
{
  int error = 0;

  //  "The type of the key can be explicitly declared appending :l for long,(or alternatively :i), :d for double, :s for string to the key name"

  QStringList keys;
  for ( auto it = mKeys.constBegin(); it != mKeys.constEnd(); ++it )
  {
    const QString &key = it.key();
    const QVariant &value = it.value();

    switch ( value.type() )
    {
      case QMetaType::QString:
        keys.append( QStringLiteral( "%1:%2" ).arg( key, QString( 's' ) ) );
        break;
      case QMetaType::Long:
        keys.append( QStringLiteral( "%1:%2" ).arg( key, QString( 'l' ) ) );
        break;
      case QMetaType::Int:
        keys.append( QStringLiteral( "%1:%2" ).arg( key, QString( 'i' ) ) );
        break;
      case QMetaType::Double:
        keys.append( QStringLiteral( "%1:%2" ).arg( key, QString( 'd' ) ) );
        break;
      default:
        continue;
        break;
    }
  }

  const QString &keysString = keys.join( ',' );
  mIndex = std::shared_ptr<codes_index>(
             codes_index_new_from_file( nullptr, mFileName.toUtf8(), keysString.toUtf8(), &error ),
  []( codes_index * index ) { if ( index ) codes_index_delete( index );} );
  mNextIndex = 0;

  if ( error != 0 )
  {
    mIndex.reset();
    return;
  }

  for ( auto it = mKeys.constBegin(); it != mKeys.constEnd(); ++it )
  {
    const QString &key = it.key();
    const QVariant &value = it.value();

    switch ( value.type() )
    {
      case QMetaType::QString:
        codes_index_select_string( mIndex.get(), key.toUtf8(), value.toString().toUtf8() );
        break;
      case QMetaType::Long:
      case QMetaType::Int:
        codes_index_select_long( mIndex.get(), key.toUtf8(), value.toInt() );
        break;
      case QMetaType::Double:
        codes_index_select_long( mIndex.get(), key.toUtf8(), value.toDouble() );
        break;
      default:
        continue;
        break;
    }
  }
}


codes_handle *ReosEcCodesIndex::nextHandle( bool &ok )
{
  int error = 0;
  if ( !mIndex )
    return nullptr;
  codes_handle *handle = codes_handle_new_from_index( mIndex.get(), &error );
  if ( error != 0 )
  {
    ok = error == CODES_END_OF_INDEX;
    codes_handle_delete( handle );
    return nullptr;
  }
  ok = true;
  mNextIndex++;
  return handle;
}

codes_handle *ReosEcCodesIndex::handle( int handleIndex )
{
  if ( handleIndex == mNextIndex )
  {
    bool ok = false;
    codes_handle *ret = nextHandle( ok );
    if ( ok )
    {
      if ( !ret )
      {
        reset();
        return nullptr;
      }
      return ret;
    }
    else
    {
      codes_handle_delete( ret );
      return nullptr;
    }
  }

  bool ok = false;

  int i = 0;
  if ( mNextIndex < handleIndex )
    i = mNextIndex;
  else
    reset();

  while ( codes_handle *handle = nextHandle( ok ) )
  {
    if ( !ok )
    {
      codes_handle_delete( handle );
      return nullptr;
    }

    if ( handleIndex == i )
      return handle;
    codes_handle_delete( handle );

    i++;
    if ( i > handleIndex )
    {
      reset();
      break;
    }
  }

  return nullptr;

}

bool ReosEcCodesIndex::isValid() const
{
  return mIndex != nullptr;
}
