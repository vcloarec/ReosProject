/***************************************************************************
  reosgribprovider.cpp - ReosGribProvider

 ---------------------
 begin                : 11.11.2022
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

#include <QFileInfo>
#include <QDir>
#include <QLocale>

#include "reosgribprovider.h"
#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"
#include "reoseccodesreader.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosGribProviderFactory();
}

ReosGribGriddedDataProvider::ReosGribGriddedDataProvider()
{
  mCache.setMaxCost( 20000000 );
}

ReosGriddedDataProvider *ReosGribGriddedDataProvider::clone() const
{
  std::unique_ptr<ReosGribGriddedDataProvider> other = std::make_unique<ReosGribGriddedDataProvider>();
  other->setDataSource( dataSource() );

  return other.release();
}

void ReosGribGriddedDataProvider::load()
{
  QString fileSource = sourcePathFromUri( dataSource() );
  QString varName = variableFromUri( dataSource() );
  mSourceValueType = valueTypeFromUri( dataSource() );
  mGribKeys = keysFromUri( dataSource() );
  mIsValid = false;
  GribReader reader = mGribKeys.isEmpty() ? GDAL : EcCodes;

  QMap<qint64, GribFrame> pathes;

  if ( !sourceIsValid( fileSource, mLastMessage ) )
    return;
  QDir dir;
  QFileInfo fileInfo( fileSource );
  if ( fileInfo.isDir() )
  {
    QStringList filters;
    dir = QDir( fileSource );
    filters << QStringLiteral( "*.grib2" );
    filters << QStringLiteral( "*.grb2" );
    const QStringList files = dir.entryList( filters, QDir::Files );
    for ( const QString &file : files )
      switch ( reader )
      {
        case GDAL:
          parseFileWithGDAL( dir.filePath( file ), varName, mReferenceTime, pathes, mExtent );
          break;
        case EcCodes:
          parseFileWithEcCodes( dir.filePath( file ), pathes, mExtent );
      }
  }
  else if ( fileInfo.isFile() )
  {
    switch ( reader )
    {
      case GDAL:
        parseFileWithGDAL( fileSource, varName, mReferenceTime, pathes, mExtent );
        break;
      case EcCodes:
        parseFileWithEcCodes( fileSource, pathes, mExtent );
    }
  }

  mFrames = pathes.values();
  mIsValid = true;

  emit dataReset();
  emit loadingFinished();
}

QStringList ReosGribGriddedDataProvider::fileSuffixes() const
{
  QStringList ret;
  ret << QStringLiteral( "grib2" )
      << QStringLiteral( "grb2" );

  return ret;
}

ReosGriddedRainfallProvider::FileDetails ReosGribGriddedDataProvider::details( const QString &source, ReosModule::Message &message ) const
{
  FileDetails ret;
  QDir dir;

  if ( !sourceIsValid( source, message ) )
    return ret;

  QStringList files = getFiles( source, dir );

  if ( files.isEmpty() )
  {
    message.type = ReosModule::Error;
    message.text = tr( "No Grib2 source found in \"%1\"." ).arg( source );
  }

  bool hasExtent = false;

  for ( const QString &file : std::as_const( files ) )
  {
    ReosGdalDataset dataset( dir.filePath( file ) );

    int bandCount = dataset.bandCount();

    if ( bandCount == 0 )
    {
      message.type = ReosModule::Error;
      message.text = tr( "No data found in \"%1\"." ).arg( source );
      return ret;
    }

    for ( int i = 1; i <= bandCount; ++i )
    {
      QMap<QString, QString> metadata = dataset.bandMetadata( i );
      auto it = metadata.find( QStringLiteral( "GRIB_COMMENT" ) );
      if ( it == metadata.end() )
        continue;

      if ( ret.availableVariables.contains( it.value() ) )
        continue;

      ret.availableVariables.append( it.value() );
      if ( !hasExtent )
        ret.extent = dataset.extent();
    }
  }

  if ( ret.availableVariables.isEmpty() )
  {
    message.type = ReosModule::Error;
    message.text = tr( "No data found in \"%1\"." ).arg( source );
  }

  ret.files = files;

  giveName( ret );
  return ret;
}

bool ReosGribGriddedDataProvider::isValid() const
{
  return mIsValid;
}

int ReosGribGriddedDataProvider::count() const
{
  switch ( mSourceValueType )
  {
    case ValueType::Cumulative:
      return mFrames.count() - 1;
      break;
    case ValueType::Instantaneous:
      return mFrames.count();
      break;
    case ValueType::CumulativeOnTimeStep:
      return mFrames.count();
      break;
    default:
      break;
  }
}

bool ReosGribGriddedDataProvider::canReadUri( const QString &path ) const
{
  QDir dir;
  ReosModule::Message message;
  if ( !sourceIsValid( path, message ) )
    return false;

  QStringList files = getFiles( path, dir );

  return !files.empty() && dir.exists();
}

QDateTime ReosGribGriddedDataProvider::startTime( int index ) const
{
  switch ( mSourceValueType )
  {
    case ValueType::Cumulative:
      return QDateTime::fromSecsSinceEpoch( mFrames.at( index ).validTime, Qt::UTC );
      break;
    case ValueType::CumulativeOnTimeStep:
      return QDateTime::fromSecsSinceEpoch(
               mFrames.at( index ).validTime, Qt::UTC ).addSecs( -mFrames.at( index ).timeRange.valueSecond() );
      break;
    case ValueType::Instantaneous:
      return QDateTime::fromSecsSinceEpoch( mFrames.at( index ).validTime, Qt::UTC );
      break;
  }

  return QDateTime();
}

QDateTime ReosGribGriddedDataProvider::endTime( int index ) const
{
  switch ( mSourceValueType )
  {
    case ValueType::Cumulative:
      return QDateTime::fromSecsSinceEpoch( mFrames.at( index + 1 ).validTime, Qt::UTC );
      break;
    case ValueType::CumulativeOnTimeStep:
    case ValueType::Instantaneous:
      return QDateTime::fromSecsSinceEpoch( mFrames.at( index ).validTime, Qt::UTC );
      break;
  }

  return QDateTime();;
}

const QVector<double> ReosGribGriddedDataProvider::data( int index ) const
{
  if ( index < 0 )
    return QVector<double>();

  CacheValues *cache = mCache.object( index );

  if ( cache && cache->typeCalculatedFrom == mSourceValueType )
    return cache->values;

  if ( cache )
    mCache.remove( index );


  switch ( mSourceValueType )
  {
    case ValueType::Cumulative:
    {
      ReosRasterMemory<double> raster = frame( index + 1 );
      ReosRasterMemory<double> prevRaster = frame( index );
      QVector<double> ret( raster.values().count(),  std::numeric_limits<double>::quiet_NaN() ) ;

      for ( int i = 0; i < ret.count(); ++i )
        ret[i] = raster.values().at( i ) - prevRaster.values().at( i );
      return ret;

    }
    break;
    case ValueType::CumulativeOnTimeStep:
    case ValueType::Instantaneous:
    {
      ReosRasterMemory<double> raster = frame( index );
      return raster.values();
    }

    break;
  }

  return QVector<double>();
}

bool ReosGribGriddedDataProvider::getDirectMinMax( double &min, double &max ) const
{
  if ( mHasMinMaxCalculated )
  {
    min = mMin;
    max = mMax;
  }
  return mHasMinMaxCalculated;
}

void ReosGribGriddedDataProvider::calculateMinMax( double &min, double &max ) const
{
  mMin = std::numeric_limits<double>::max();
  mMax = -std::numeric_limits<double>::max();

  int gridCount = count();

  for ( int i = 0; i < gridCount; ++i )
  {
    const QVector<double> vals = data( i );
    for ( const double &v : vals )
    {
      if ( v < mMin )
        mMin = v;
      if ( v > mMax )
        mMax = v;
    }
  }
  min = mMin;
  max = mMax;
  mHasMinMaxCalculated = true;
}

QString ReosGribGriddedDataProvider::htmlDescription() const
{
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );

  htmlText += QStringLiteral( "<h2>" ) + tr( "Gridded Precipitation" ) + QStringLiteral( "</h2>\n<hr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Format" ) ) + QStringLiteral( "</td><td>" )
              + QStringLiteral( "GRIB 2" ) + QStringLiteral( "</td></tr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Source" ) ) + QStringLiteral( "</td><td>" )
              + sourcePathFromUri( dataSource() ) + QStringLiteral( "</td></tr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Variable" ) ) + QStringLiteral( "</td><td>" )
              + variableFromUri( dataSource() ) + QStringLiteral( "</td></tr>\n" );

  if ( count() > 0 )
  {
    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                +  QStringLiteral( "<b>%1</b>" ).arg( tr( "Start date" ) ) + QStringLiteral( "</td><td>" )
                + startTime( 0 ).toString( QLocale().dateTimeFormat() )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                +  QStringLiteral( "<b>%1</b>" ).arg( tr( "End date" ) ) + QStringLiteral( "</td><td>" )
                + endTime( count() - 1 ).toString( QLocale().dateTimeFormat() )
                + QStringLiteral( "</td></tr>\n" );
  }

  return htmlText;
}

void ReosGribGriddedDataProvider::exportToTiff( int index, const QString &fileName ) const
{
  ReosRasterMemory<double> rast( mExtent.yCellCount(), mExtent.xCellCount() );

  if ( rast.reserveMemory() )
  {
    rast.setValues( data( index ) );
    ReosGdalDataset::writeDoubleRasterToFile( fileName, rast, mExtent );
  }

}

ReosRasterExtent ReosGribGriddedDataProvider::extent() const
{
  return mExtent;
}

QString ReosGribGriddedDataProvider::dataType() {return ReosGriddedRainfall::staticType();}

QString ReosGribGriddedDataProvider::staticKey()
{
  return GRIB_KEY + QString( "::" ) + dataType();
}

QString ReosGribGriddedDataProvider::uri( const QString &sourcePath, const QString &variable, ValueType valueType )
{
  QString stringValueType;

  switch ( valueType )
  {
    case ValueType::Cumulative:
      stringValueType = QStringLiteral( "cumulative" );
      break;
    case ValueType::CumulativeOnTimeStep:
      stringValueType = QStringLiteral( "height" );
    case ValueType::Instantaneous:
      stringValueType = QStringLiteral( "intensity" );
      break;
  }
  return QStringLiteral( "\"%1\"::%2::%3" ).arg( sourcePath, variable, stringValueType );
}

QString ReosGribGriddedDataProvider::uri( const QString &sourcePath, const QVariantMap &gribKeys )
{
  QStringList stringKey;

  for ( auto it = gribKeys.constBegin(); it != gribKeys.constEnd(); ++it )
    stringKey.append( QStringLiteral( "%1:%2" ).arg( it.key(), it.value().toString() ) );

  return QStringLiteral( "\"%1\"::keys=%2" ).arg( sourcePath, stringKey.join( '&' ) );
}

QString ReosGribGriddedDataProvider::sourcePathFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() == 0 )
    return QString();

  QString source = part.at( 0 );
  source.remove( QStringLiteral( "\"" ) );
  return source;
}

QString ReosGribGriddedDataProvider::variableFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 2 || part.at( 1 ).startsWith( QStringLiteral( "keys=" ) ) )
    return QString();

  return part.at( 1 );
}

QVariantMap ReosGribGriddedDataProvider::keysFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 2 || !part.at( 1 ).startsWith( QStringLiteral( "keys=" ) ) )
    return QVariantMap();

  const QString stringKeys = part.at( 1 ).split( '=' ).at( 1 );

  const QStringList keysList = stringKeys.split( '&' );

  QVariantMap ret;

  for ( const QString &key : keysList )
  {
    QStringList split = key.split( ':' );
    if ( split.count() != 2 )
      continue;

    ret.insert( split.at( 0 ), split.at( 1 ) );
  }

  return ret;
}

ReosGriddedRainfallProvider::ValueType ReosGribGriddedDataProvider::valueTypeFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 3 )
    return ValueType::CumulativeOnTimeStep;

  if ( part.at( 2 ) == QStringLiteral( "cumulative" ) )
    return ValueType::Cumulative;

  if ( part.at( 2 ) == QStringLiteral( "intensity" ) )
    return ValueType::Instantaneous;

  if ( part.at( 2 ) == QStringLiteral( "height" ) )
    return ValueType::CumulativeOnTimeStep;

  return ValueType::CumulativeOnTimeStep;
}

bool ReosGribGriddedDataProvider::sourceIsValid( const QString &source, ReosModule::Message &message ) const
{
  QFileInfo fileInfo( source );
  if ( fileInfo.isDir() )
  {
    QDir dir( source );
    if ( !dir.exists() )
    {
      message.type = ReosModule::Error;
      message.text = tr( "Folder \"%1\" does not exist." ).arg( source );
      return false;
    }
  }
  else if ( fileInfo.isFile() )
  {
    if ( !fileInfo.exists() )
    {
      message.type = ReosModule::Error;
      message.text = tr( "File \"%1\" does not exist." ).arg( source );
      return false;
    }
  }
  else
  {
    message.type = ReosModule::Error;
    message.text = tr( "Unknow source \"%1\"." ).arg( source );
    return false;
  }

  return true;

}

ReosEncodedElement ReosGribGriddedDataProvider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "grib-gridded-precipitation" ) );

  QString uriToEncode = dataSource();

  QString sourcePath = sourcePathFromUri( uriToEncode );
  sourcePath = context.pathToEncode( sourcePath );
  uriToEncode = uri( sourcePath, variableFromUri( uriToEncode ), valueTypeFromUri( uriToEncode ) );
  element.addData( QStringLiteral( "data-source" ), uriToEncode );

  return element;
}

void ReosGribGriddedDataProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != QStringLiteral( "grib-gridded-precipitation" ) )
    return;
  QString source;
  if ( element.getData( QStringLiteral( "data-source" ), source ) )
  {
    QString sourcePath = sourcePathFromUri( source );
    sourcePath = context.resolvePath( sourcePath );
    source = uri( sourcePath, variableFromUri( source ), valueTypeFromUri( source ) );
    setDataSource( source );
  }

}

void ReosGribGriddedDataProvider::parseFileWithGDAL(
  const QString &fileName,
  const QString &varName,
  qint64 &referenceTime,
  QMap<qint64, GribFrame> &pathes,
  ReosRasterExtent &extent )
{
  ReosGdalDataset dataset( fileName );
  if ( !dataset.isValid() )
    return;

  int bandCount = dataset.bandCount();
  for ( int bi = 1; bi <= bandCount; ++bi )
  {
    const QMap<QString, QString> metadata = dataset.bandMetadata( bi );

    if ( metadata.value( QStringLiteral( "GRIB_COMMENT" ) ) == varName )
    {
      if ( pathes.isEmpty() )
        extent = dataset.extent();
      else
      {
        if ( extent != dataset.extent() )
          continue;
      }

      GribFrame path;
      path.file = fileName;
      path.frameNo = bi - 1;
      path.reader = GDAL;

      QString strRefTime = metadata.value( QStringLiteral( "GRIB_REF_TIME" ) );
      if ( strRefTime.isEmpty() )
        continue;
      strRefTime = strRefTime.split( ' ', Qt::SplitBehaviorFlags::SkipEmptyParts ).at( 0 );
      bool ok = false;
      qint64 refTime = strRefTime.toInt( &ok );
      if ( !ok )
        continue;
      if ( mReferenceTime == -1 )
        referenceTime = refTime;
      // else if ( refTime != mReferenceTime )
      //   continue;

      QString strValidTime = metadata.value( QStringLiteral( "GRIB_VALID_TIME" ) );
      if ( strValidTime.isEmpty() )
        continue;
      strValidTime = strValidTime.split( ' ', Qt::SplitBehaviorFlags::SkipEmptyParts ).at( 0 );
      ok = false;
      path.validTime = strValidTime.toInt( &ok );
      if ( !ok )
        continue;
      pathes.insert( path.validTime, path );
    }
  }
}

void ReosGribGriddedDataProvider::parseFileWithEcCodes(
  const QString &fileName,
  QMap<qint64, GribFrame> &pathes,
  ReosRasterExtent &extent )
{
  std::unique_ptr<ReosEcCodesReader> reader = std::make_unique<ReosEcCodesReader>( fileName, mGribKeys );
  if ( !reader->isValid() )
    return;

  int frameCount = reader->frameCount();
  for ( int fi = 0; fi < frameCount; ++fi )
  {
    if ( pathes.isEmpty() )
      extent = reader->extent( fi );
    else
    {
      if ( extent != reader->extent( fi ) )
        continue;
    }

    GribFrame path;
    path.file = fileName;
    path.frameNo = fi;
    path.reader = EcCodes;
    path.timeRange = reader->stepDuration( fi );

    ReosEcCodesReader::StepType stepType = reader->stepType( fi );
    QPair<int, int> range = reader->stepRange( fi );

    switch ( stepType )
    {
      case ReosEcCodesReader::Accum:
      {
        if ( range.first == 0 )
          mSourceValueType = ValueType::Cumulative;
        else
          mSourceValueType = ValueType::CumulativeOnTimeStep;
      }
      break;
      case ReosEcCodesReader::Instant:
        mSourceValueType = ValueType::Instantaneous;
      default:
        break;
    }

    if ( mReferenceTime == -1 )
      mReferenceTime = reader->referenceTime( fi ).toSecsSinceEpoch();


    path.validTime = reader->validityTime( fi ).toSecsSinceEpoch();

    pathes.insert( path.validTime, path );
  }

  mCurrentReader.reset( reader.release() );
  mCurrentReaderType = EcCodes;
  mCurrentFile = fileName;
}

QStringList ReosGribGriddedDataProvider::getFiles( const QString &path, QDir &dir ) const
{
  QStringList files;
  QFileInfo fileInfo( path );
  if ( fileInfo.isDir() )
  {
    QStringList filters;
    dir = QDir( path );
    filters << QStringLiteral( "*.grib2" );
    filters << QStringLiteral( "*.grb2" );
    files = dir.entryList( filters, QDir::Files );
  }
  else if ( fileInfo.isFile() &&
            ( fileInfo.suffix() == QStringLiteral( "grib2" ) ||
              fileInfo.suffix() == QStringLiteral( "grb2" ) ) )
  {
    files << path;
    dir = fileInfo.dir();
  }

  return files;
}

void ReosGribGriddedDataProvider::giveName( FileDetails &details )
{
  QString name;
  auto baseName = []( const QString & string )->QString
  {
    QFileInfo fileInfo( string );
    return fileInfo.baseName();
  };
  const QStringList &files = details.files;

  if ( files.count() == 1 )
    name = baseName( files.first() );
  else
  {
    QString fileName1 = baseName( files.at( 0 ) );
    QString commonPart;
    for ( int i = 0; i < files.count() - 1; ++i )
    {
      commonPart.clear();
      const QString fileName2 = baseName( files.at( i + 1 ) );
      int cp2 = 0;
      QChar c2;
      bool common = false;
      for ( int cp1 = 0; cp1 < fileName1.count(); cp1++ )
      {
        const QChar c1 = fileName1.at( cp1 );
        c2 = fileName2.at( cp2 );

        if ( common && c1 != c2 )
          break;

        while ( c1 != c2 && cp2 < fileName2.count() - 1 )
        {
          cp2++;
          c2 = fileName2.at( cp2 );
        }

        common = c1 == c2;
        if ( !common )
        {
          if ( c2 == fileName2.count() )
          {
            if ( c1 == fileName1.count() )
              break;
            else
              cp2 = 0;
          }
        }
        else
        {
          commonPart.append( c1 );
          cp2++;
        }
        if ( cp2 == fileName2.count() )
          break;
      }
      if ( !commonPart.isEmpty() )
        fileName1 = commonPart;
    }

    if ( ! fileName1.isEmpty() )
      name = commonPart;
  }

  details.deducedName = name;
}

ReosRasterMemory<double> ReosGribGriddedDataProvider::frame( int index ) const
{
  GribFrame gf = mFrames.at( index );
  if ( mCurrentReader &&
       mCurrentReaderType == gf.reader &&
       mCurrentFile == gf.file )
    return mCurrentReader->values( gf.frameNo );

  switch ( gf.reader )
  {
    case GDAL:
      mCurrentReader.reset( new ReosGdalDataset( gf.file ) );
    default:
      mCurrentReader.reset( new ReosEcCodesReader( gf.file, mGribKeys ) );
      break;
  }
  mCurrentFile = gf.file;
  mCurrentReaderType = gf.reader;
  return mCurrentReader->values( gf.frameNo );
}

ReosGriddedDataProvider *ReosGribProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosGribGriddedDataProvider::dataType() )
    return new ReosGribGriddedDataProvider;

  return nullptr;
}

QString ReosGribProviderFactory::key() const
{
  return GRIB_KEY;
}

bool ReosGribProviderFactory::supportType( const QString &dataType ) const
{
  return dataType.contains( ReosGriddedRainfall::staticType() )
         || dataType.contains( ReosGriddedData::staticType() );
}

QVariantMap ReosGribProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
  {
    ret.insert( QStringLiteral( "file-or-dir-path" ), QObject::tr( "File or directory where are stored the data" ) );
    ret.insert( QStringLiteral( "variable" ), QObject::tr( "variable that store the pricipitation values" ) );
    ret.insert( QStringLiteral( "value-type" ), QObject::tr( "Type of the values: Intensity(0), Height for the time step (1) or Cummulative heigth (2) " ) );
    ret.insert( QStringLiteral( "keys" ), QObject::tr( "GRIB keys used for filtering the dataset, can be used instead of \"variables\" and \"value-type\"." ) );
  }

  return ret;
}

QString ReosGribProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType ) &&
       parameters.contains( QStringLiteral( "file-or-dir-path" ) ) &&
       parameters.contains( QStringLiteral( "variable" ) ) && parameters.contains( QStringLiteral( "value-type" ) ) )
  {
    const QString path = parameters.value( QStringLiteral( "file-or-dir-path" ) ).toString();
    const QString variable = parameters.value( QStringLiteral( "variable" ) ).toString();
    int typeInt = parameters.value( QStringLiteral( "value-type" ) ).toInt();

    if ( typeInt >= 0 && typeInt < 3 )
    {
      ReosGriddedRainfallProvider::ValueType type = static_cast<ReosGriddedRainfallProvider::ValueType>( typeInt );
      ok = true;
      return ReosGribGriddedDataProvider::uri( path, variable, type );
    }
  }

  if ( supportType( dataType ) &&
       parameters.contains( QStringLiteral( "file-or-dir-path" ) ) &&
       parameters.contains( QStringLiteral( "grib-keys" ) ) )
  {
    const QString path = parameters.value( QStringLiteral( "file-or-dir-path" ) ).toString();
    const QVariantMap keys = parameters.value( QStringLiteral( "grib-keys" ) ).toMap();
    ok = true;
    return ReosGribGriddedDataProvider::uri( path, keys );
  }

  ok = false;
  return QString();
}
