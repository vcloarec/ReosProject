/***************************************************************************
  reoscomephoresprovider.cpp - ReosComephoresProvider

 ---------------------
 begin                : 22.12.2022
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
#include "reoscomephoreprovider.h"

#include <QLocale>

#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"
#include "reosgisengine.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosComephoresProviderFactory();
}

ReosComephoreProvider::ReosComephoreProvider()
{
  mCache.setMaxCost( 20000000 );
}

ReosGriddedRainfallProvider *ReosComephoreProvider::clone() const
{
  std::unique_ptr<ReosComephoreProvider> other = std::make_unique<ReosComephoreProvider>();

  other->mIsValid = mIsValid;
  other->mFileReader.reset( mFileReader->clone() );
  other->mExtent = mExtent;

  return other.release();
}

ReosComephoreProvider::~ReosComephoreProvider() = default;

void ReosComephoreProvider::setDataSource( const QString &dataSource )
{
  mIsValid = false;
  ReosGriddedRainfallProvider::setDataSource( dataSource );
  QFileInfo sourceInfo( dataSource );

  if ( sourceInfo.isDir() )
  {
    if ( ReosComephoreTiffFilesReader::canReadFile( dataSource ) )
      mFileReader.reset( new ReosComephoreTiffFilesReader( dataSource ) );
    else
      mFileReader.reset( new ReosComephoreNetCdfFolderReader( dataSource ) );
  }
  else if ( sourceInfo.isFile() && sourceInfo.suffix() == QStringLiteral( "nc" ) )
  {
    mFileReader.reset( new ReosComephoreNetCdfFilesReader( dataSource ) );
  }

  if ( mFileReader )
  {
    mIsValid = mFileReader->frameCount() > 0;
    mExtent = mFileReader->extent();
  }

  emit dataReset();
  emit loadingFinished();
}

QStringList ReosComephoreProvider::fileSuffixes() const
{
  QStringList ret;
  ret << QStringLiteral( "tif" )
      << QStringLiteral( "tiff" );

  return ret;
}

QString ReosComephoreProvider::htmlDescription() const
{
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );


  htmlText += QStringLiteral( "<h2>" ) + tr( "Gridded Precipitation" ) + QStringLiteral( "</h2>\n<hr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Format" ) ) + QStringLiteral( "</td><td>" )
              + QStringLiteral( "COMEPHORE" ) + QStringLiteral( "</td></tr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Source" ) ) + QStringLiteral( "</td><td>" )
              + dataSource() + QStringLiteral( "</td></tr>\n" );

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

bool ReosComephoreProvider::isValid() const
{
  return mIsValid;
}

int ReosComephoreProvider::count() const
{
  if ( mFileReader )
    return mFileReader->frameCount();
  return 0;
}

QDateTime ReosComephoreProvider::startTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index );

  return QDateTime();
}

QDateTime ReosComephoreProvider::endTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index ).addSecs( 3600 );

  return QDateTime();
}

const QVector<double> ReosComephoreProvider::data( int index ) const
{
  if ( index < 0 || index >= mFileReader->frameCount() )
    return QVector<double>();

  if ( mCache.contains( index ) )
    return *mCache.object( index );

  if ( mFileReader )
  {
    bool readLine = true;
    const QVector<int> rawValues = mFileReader->data( index, readLine );
    std::unique_ptr<QVector<double>> values = std::make_unique<QVector<double>>();
    values->resize( rawValues.count() );

    int xCount = mExtent.xCellCount();
    int yCount = mExtent.yCellCount();

    Q_ASSERT( xCount * yCount == rawValues.count() );

    for ( int xi = 0; xi < xCount; ++xi )
      for ( int yi = 0; yi < yCount; ++yi )
      {
        int retIndex = xi + yi * xCount;
        int rawIndex = readLine ? retIndex : yi + xi * yCount;

        int rawValue = rawValues.at( rawIndex );
        if ( rawValue == 65535 || rawValues.at( rawIndex ) == 0 )
          ( *values )[retIndex] = std::numeric_limits<double>::quiet_NaN();
        else
          ( *values )[retIndex] = rawValue / 10.0;
      }

    QVector<double> ret = *values;
    mCache.insert( index, values.release(), rawValues.size() );

    return ret;
  }

  return QVector<double>();
}

ReosRasterExtent ReosComephoreProvider::extent() const
{
  return mExtent;
}

bool ReosComephoreProvider::canReadUri( const QString &uri ) const
{
  if ( ReosComephoreTiffFilesReader::canReadFile( uri ) )
    return true;

  if ( ReosComephoreNetCdfFilesReader::canReadFile( uri ) )
    return true;

  if ( ReosComephoreNetCdfFolderReader::canReadFile( uri ) )
    return true;

  return false;
}

static QFileInfoList tiffFiles( const QString &folderPath )
{
  QFileInfoList ret;
  QFileInfo sourceInfo( folderPath );
  if ( !sourceInfo.isDir() )
    return ret;

  QDir sourceDir( folderPath );

  if ( !sourceDir.cd( QStringLiteral( "RR" ) ) )
    return ret;

  QStringList filters;
  filters << QStringLiteral( "*_RR.gtif" );
  filters << QStringLiteral( "*_RR.tif" );
  filters << QStringLiteral( "*_RR.tiff" );
  const QFileInfoList fileInfoList = sourceDir.entryInfoList( filters, QDir::Files );

  return fileInfoList;
}

ReosGriddedRainfallProvider::FileDetails ReosComephoreProvider::details( const QString &source, ReosModule::Message &message ) const
{
  FileDetails ret;

  bool ok = false;
  ret = ReosComephoreTiffFilesReader::details( source, &ok );
  if ( ok )
    return ret;

  return FileDetails();
}

ReosEncodedElement ReosComephoreProvider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "comephores-gridded-precipitation" ) );

  QString sourcePath = dataSource();
  sourcePath = context.pathToEncode( sourcePath );
  element.addData( QStringLiteral( "data-source" ), sourcePath );

  return element;
}

void ReosComephoreProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != QStringLiteral( "comephores-gridded-precipitation" ) )
    return;
  QString source;
  if ( element.getData( QStringLiteral( "data-source" ), source ) )
  {
    const QString sourcePath =  context.resolvePath( source );
    setDataSource( sourcePath );
  }
}

bool ReosComephoreProvider::getDirectMinMax( double &min, double &max ) const
{
  if ( mFileReader )
    return mFileReader->getDirectMinMax( min, max );
  return false;
}

void ReosComephoreProvider::calculateMinMax( double &min, double &max ) const
{
  min = std::numeric_limits<double>::max();
  max = -std::numeric_limits<double>::max();

  int gridCount = count();

  for ( int i = 0; i < gridCount; ++i )
  {
    const QVector<double> vals = data( i );
    for ( const double &v : vals )
    {
      if ( v < min )
        min = v;
      if ( v > max )
        max = v;
    }
  }
}

QString ReosComephoreProvider::dataType() {return ReosGriddedRainfall::staticType();}

QString ReosComephoreProvider::staticKey()
{
  return COMEPHORES_KEY + QString( "::" ) + dataType();
}

ReosComephoreTiffFilesReader::ReosComephoreTiffFilesReader( const QString &folderPath )
{
  const QFileInfoList fileInfoList = tiffFiles( folderPath );

  for ( const QFileInfo &fi : fileInfoList )
  {
    const QString timeString = fi.baseName().remove( QStringLiteral( "_RR" ) );
    QDateTime time = QDateTime::fromString( timeString, QStringLiteral( "yyyyMMddHH" ) );
    time.setTimeSpec( Qt::UTC );
    mFilesNames.insert( time, fi.filePath() );
    mTimes.append( time );
  }
}

ReosComephoreFilesReader *ReosComephoreTiffFilesReader::clone() const
{
  std::unique_ptr<ReosComephoreTiffFilesReader> other( new ReosComephoreTiffFilesReader );
  other->mFilesNames = mFilesNames;
  other->mTimes = mTimes;
  return other.release();
}

int ReosComephoreTiffFilesReader::frameCount() const
{
  return mFilesNames.count();
}

QDateTime ReosComephoreTiffFilesReader::time( int i ) const
{
  return mTimes.at( i );
}

QVector<int> ReosComephoreTiffFilesReader::data( int index, bool &readLine ) const
{
  const QDateTime &time = mTimes.at( index );
  const QString fileName = mFilesNames.value( time );
  ReosGdalDataset dataset( fileName );

  const ReosRasterMemory<int> values = dataset.valuesInt( 1 );
  readLine = true;

  return values.values();
}

ReosRasterExtent ReosComephoreTiffFilesReader::extent() const
{
  if ( mFilesNames.isEmpty() )
    return ReosRasterExtent();

  ReosGdalDataset dataset( mFilesNames.begin().value() );

  return dataset.extent();
}

bool ReosComephoreTiffFilesReader::getDirectMinMax( double &, double & ) const
{
  return false;
}

bool ReosComephoreTiffFilesReader::canReadFile( const QString &uri )
{
  return !tiffFiles( uri ).isEmpty();
}

ReosGriddedRainfallProvider::FileDetails ReosComephoreTiffFilesReader::details( const QString &source, bool *ok )
{
  ReosGriddedRainfallProvider::FileDetails ret;

  ReosComephoreTiffFilesReader fileReader( source );
  if ( fileReader.frameCount() == 0 )
  {
    *ok = false;
    return ret;
  }

  ret.extent = fileReader.extent();
  ret.files = fileReader.mFilesNames.values();

  QDir dir( source );
  ret.deducedName = dir.dirName();

  *ok = true;

  return ret;
}

ReosComephoreTiffFilesReader::~ReosComephoreTiffFilesReader() = default;

ReosGriddedRainfallProvider *ReosComephoresProviderFactory::createProvider( const QString &dataType ) const
{
  if ( ReosComephoreProvider::dataType() == dataType )
    return new ReosComephoreProvider;

  return nullptr;
}

QString ReosComephoresProviderFactory::key() const
{
  return COMEPHORES_KEY;
}

bool ReosComephoresProviderFactory::supportType( const QString &dataType ) const
{
  return dataType.contains( ReosGriddedRainfall::staticType() );
}

QVariantMap ReosComephoresProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
    ret.insert( QStringLiteral( "file-or-dir-path" ), QObject::tr( "File or directory where are stored the data" ) );

  return ret;
}

QString ReosComephoresProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType ) && parameters.contains( QStringLiteral( "file-or-dir-path" ) ) )
  {
    ok = true;
    return parameters.value( QStringLiteral( "file-or-dir-path" ) ).toString();
  }
  else
  {
    ok = false;
    return QString();
  }
}

ReosComephoreNetCdfFilesReader::ReosComephoreNetCdfFilesReader( const QString &filePath )
  :  mFileName( filePath )
{
  mFile.reset( new ReosNetCdfFile( mFileName ) );
  if ( mFile->isValid() )
  {
    const QString proj4Crs = mFile->globalStringAttributeValue( QStringLiteral( "crs_proj4_string" ) );
    const QString crs = ReosGisEngine::crsFromProj( proj4Crs );
    int xCount = mFile->dimensionLength( QStringLiteral( "X" ) );
    int yCount = mFile->dimensionLength( QStringLiteral( "Y" ) );

    double nw_latitude = mFile->globalDoubleAttributeValue( QStringLiteral( "nw_corner_latitude" ) );
    double nw_longitude = mFile->globalDoubleAttributeValue( QStringLiteral( "nw_corner_longitude" ) );
    const QString crsWGS84 = ReosGisEngine::crsFromEPSG( 4326 );
    const ReosSpatialPosition nw_position( nw_longitude, nw_latitude, crsWGS84 );
    const QPointF projectedOrigin = ReosGisEngine::transformToCoordinates( nw_position, crs );
    double xResolution = mFile->globalDoubleAttributeValue( QStringLiteral( "x_resolution_in_m" ) );
    double yResolution = mFile->globalDoubleAttributeValue( QStringLiteral( "y_resolution_in_m" ) );

    mExtent = ReosRasterExtent( projectedOrigin.x(), projectedOrigin.y(), xCount, yCount, xResolution, -yResolution );
    mExtent.setCrs( crs );

    int frameCount = mFile->dimensionLength( QStringLiteral( "time" ) );
    const QVector<qint64> intTime = mFile->getInt64Array( QStringLiteral( "time" ), frameCount );
    const QDateTime oriTime( QDate( 1949, 12, 1 ), QTime( 0, 0, 0 ), Qt::UTC );
    QMap<QDateTime, int> timeToFileIndex;
    for ( int i = 0; i < frameCount; ++i )
    {
      const QDateTime time = oriTime.addSecs( 3600 * intTime.at( i ) );
      if ( !timeToFileIndex.contains( time ) )
        timeToFileIndex.insert( time, i );
    }
    mTimes = timeToFileIndex.keys();
    for ( int i = 0; i < mTimes.count(); ++i )
      mRainIndexToFileIndex.insert( i, timeToFileIndex.value( mTimes.at( i ) ) );
  }

  mFile.reset();
}

ReosComephoreFilesReader *ReosComephoreNetCdfFilesReader::clone() const
{
  std::unique_ptr<ReosComephoreNetCdfFilesReader> other = std::unique_ptr<ReosComephoreNetCdfFilesReader>();
  other->mFile.reset( new ReosNetCdfFile( mFileName ) );

  other->mFileName = mFileName;
  other->mExtent = mExtent;
  other->mRainIndexToFileIndex = mRainIndexToFileIndex;
  other->mTimes = mTimes;
  return other.release();
}

int ReosComephoreNetCdfFilesReader::frameCount() const
{
  return mTimes.count();
}

QDateTime ReosComephoreNetCdfFilesReader::time( int i ) const
{
  int index = mRainIndexToFileIndex.value( i, -1 );
  if ( index < 0 || index >= mTimes.count() )
    return QDateTime();
  return mTimes.at( index );
}

QVector<int> ReosComephoreNetCdfFilesReader::data( int index, bool &readLine ) const
{
  if ( !mFile )
    mFile.reset( new ReosNetCdfFile( mFileName ) );

  int fileIndex = mRainIndexToFileIndex.value( index, -1 );

  const QVector<int> starts( {fileIndex, 0, 0} );
  int xCount = mExtent.xCellCount();
  int yCount = mExtent.yCellCount();
  const QVector<int> counts( {1, xCount, yCount} );

  if ( mFile->isValid() )
  {
    readLine = false;
    return mFile->getIntArray( QStringLiteral( "RR" ), starts, counts );
  }

  return QVector<int>();
}

ReosRasterExtent ReosComephoreNetCdfFilesReader::extent() const
{
  return mExtent;
}

bool ReosComephoreNetCdfFilesReader::getDirectMinMax( double &min, double &max ) const
{
  return false;
}

bool ReosComephoreNetCdfFilesReader::canReadFile( const QString &uri )
{
  ReosNetCdfFile file( uri );
  if ( !file.isValid() )
    return false;

  if ( !( file.hasVariable( QStringLiteral( "RR" ) ) &&
          file.hasVariable( QStringLiteral( "QUALIF" ) ) &&
          file.hasVariable( QStringLiteral( "ERR" ) ) ) )
    return false;

  const QStringList dimensionNames = file.variableDimensionNames( QStringLiteral( "RR" ) );

  return dimensionNames.contains( QStringLiteral( "X" ) ) && dimensionNames.contains( QStringLiteral( "Y" ) );
}

ReosComephoreNetCdfFolderReader::ReosComephoreNetCdfFolderReader( const QString &folderPath )
  : mFolderPath( folderPath )
{
  QDir dir( folderPath );

  QStringList filters;
  filters << "*.nc";
  const QStringList entries = dir.entryList( filters, QDir::Files );

  QMap<QDateTime, size_t> firstTimeToIndex;
  for ( const QString &file : entries )
  {
    const QString filePath = dir.filePath( file );
    if ( ReosComephoreNetCdfFilesReader::canReadFile( filePath ) )
    {
      std::unique_ptr<ReosComephoreNetCdfFilesReader> fileReader = std::make_unique<ReosComephoreNetCdfFilesReader>( filePath );
      if ( fileReader->frameCount() == 0 )
        continue;
      if ( !mExtent.isValid() )
        mExtent = fileReader->extent();
      else if ( mExtent != fileReader->extent() )
        continue;

      firstTimeToIndex.insert( fileReader->time( 0 ),  mFileReaders.size() );
      mFileReaders.emplace_back( fileReader.release() );
    }
  }

  int count = 0;
  const QList<QDateTime> times = firstTimeToIndex.keys();
  for ( const QDateTime &dt : times )
  {
    size_t fileIndex = firstTimeToIndex.value( dt );
    ReosComephoreNetCdfFilesReader *reader = mFileReaders.at( fileIndex ).get();
    int frameCount = reader->frameCount();
    for ( int i = 0; i < frameCount; ++i )
    {
      mGlobalIndexToReaderIndex.insert( count, InternalIndex{fileIndex, i} );
      count++;
    }
  }
}

ReosComephoreFilesReader *ReosComephoreNetCdfFolderReader::clone() const
{
  return new ReosComephoreNetCdfFolderReader( mFolderPath );
}

int ReosComephoreNetCdfFolderReader::frameCount() const
{
  return mGlobalIndexToReaderIndex.count();
}

QDateTime ReosComephoreNetCdfFolderReader::time( int i ) const
{
  auto it = mGlobalIndexToReaderIndex.find( i );
  if ( it != mGlobalIndexToReaderIndex.constEnd() )
    return mFileReaders.at( it.value().fileIndex )->time( it.value().internIndex );

  return QDateTime();
}

QVector<int> ReosComephoreNetCdfFolderReader::data( int index, bool &readLine ) const
{
  auto it = mGlobalIndexToReaderIndex.find( index );
  if ( it != mGlobalIndexToReaderIndex.constEnd() )
    return mFileReaders.at( it.value().fileIndex )->data( it.value().internIndex, readLine );

  return QVector<int>();
}

ReosRasterExtent ReosComephoreNetCdfFolderReader::extent() const
{
  return mExtent;
}

bool ReosComephoreNetCdfFolderReader::getDirectMinMax( double &min, double &max ) const
{
  return false;
}

bool ReosComephoreNetCdfFolderReader::canReadFile( const QString &uri )
{
  QDir dir( uri );
  if ( !dir.exists() )
    return false;
  QStringList filters;
  filters << "*.nc";
  const QStringList entries = dir.entryList( filters, QDir::Files );

  for ( const QString &file : entries )
  {
    const QString filePath = dir.filePath( file );
    if ( ReosComephoreNetCdfFilesReader::canReadFile( filePath ) )
      return true;
  }

  return false;
}
