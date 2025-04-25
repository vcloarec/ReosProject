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
#include "reosera5provider.h"

#include <QLocale>
#include <QDebug>
#include <QElapsedTimer>

#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"
#include "reosgisengine.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosEra5ProviderFactory();
}

ReosEra5Provider::ReosEra5Provider()
{
  mCache.setMaxCost( 20000000 );
}

ReosGriddedDataProvider *ReosEra5Provider::clone() const
{
  std::unique_ptr<ReosEra5Provider> other = std::make_unique<ReosEra5Provider>();

  other->mIsValid = mIsValid;
  other->mFileReader.reset( mFileReader->clone() );
  other->mExtent = mExtent;

  return other.release();
}

void ReosEra5Provider::load()
{
  mIsValid = false;

  const QString &uri = dataSource();
  bool ok = false;
  const QString &path = pathFromUri( uri, ok );
  if ( !ok )
    return;
  QFileInfo sourceInfo( path );
  ok = false;
  const QString &varName = varNameFromUri( uri, ok );
  if ( !ok )
    return;

  QDateTime start = startFromUri( uri, ok );
  if ( !ok )
    start = QDateTime();

  QDateTime end = endFromUri( uri, ok );
  if ( !ok )
    end = QDateTime();

  if ( sourceInfo.isDir() )
  {
    if ( ReosEra5NetCdfFolderReader::canReadFile( dataSource() ) )
      mFileReader.reset( new ReosEra5NetCdfFolderReader( path, varName, start, end ) );
  }
  else if ( sourceInfo.isFile() && sourceInfo.suffix() == QStringLiteral( "nc" ) )
  {
    mFileReader.reset( new ReosEra5NetCdfFilesReader( path, varName, start, end ) );
  }

  if ( mFileReader )
  {
    mIsValid = mFileReader->frameCount() > 0;
    mExtent = mFileReader->extent();
  }

  emit dataReset();
  emit loadingFinished();
}

ReosEra5Provider::~ReosEra5Provider() = default;

QStringList ReosEra5Provider::fileSuffixes() const
{
  QStringList ret;
  ret << QStringLiteral( "tif" )
      << QStringLiteral( "tiff" );

  return ret;
}

QString ReosEra5Provider::htmlDescription() const
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

bool ReosEra5Provider::isValid() const
{
  return mIsValid;
}

int ReosEra5Provider::count() const
{
  if ( mFileReader )
    return mFileReader->frameCount();
  return 0;
}

QDateTime ReosEra5Provider::startTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index ).addSecs( -3600 );

  return QDateTime();
}

QDateTime ReosEra5Provider::endTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index );

  return QDateTime();
}

const QVector<double> ReosEra5Provider::data( int index ) const
{
  if ( index < 0 || index >= mFileReader->frameCount() )
    return QVector<double>();

  if ( mCache.contains( index ) )
    return *mCache.object( index );

  if ( mFileReader )
  {
    bool readLine = true;
    return mFileReader->data( index, readLine );
  }

  return QVector<double>();
}

const QVector<double> ReosEra5Provider::dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const
{
  if ( index < 0 || index >= mFileReader->frameCount() )
    return QVector<double>();

  if ( rowMax < rowMin || colMax < colMin )
    return QVector<double>();

  if ( mFileReader )
  {
    bool readLine = true;
    return mFileReader->dataInGridExtent( index, rowMin, rowMax, colMin, colMax, readLine );
  }

  return QVector<double>();
}

ReosRasterExtent ReosEra5Provider::extent() const
{
  return mExtent;
}

bool ReosEra5Provider::canReadUri( const QString &uri ) const
{
  if ( ReosEra5NetCdfFilesReader::canReadFile( uri ) )
    return true;

  if ( ReosEra5NetCdfFolderReader::canReadFile( uri ) )
    return true;

  return false;
}

ReosGriddedRainfallProvider::FileDetails ReosEra5Provider::details( const QString &source, ReosModule::Message &message ) const
{
  return FileDetails();
}

bool ReosEra5Provider::getDirectMinMax( double &min, double &max ) const
{
  if ( mFileReader )
    return mFileReader->getDirectMinMax( min, max );
  return false;
}

void ReosEra5Provider::calculateMinMax( double &min, double &max ) const
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

QString ReosEra5Provider::dataType() {return ReosGriddedData::staticType();}

QVariantMap ReosEra5Provider::decodeUri( const QString &uri, bool &ok )
{
  const QStringList params = uri.split( QStringLiteral( "::" ) );
  if ( params.size() < 2 )
  {
    ok = false;
    return QVariantMap();
  }

  QVariantMap ret;
  ret[QStringLiteral( "file-or-dir-path" )] = params.at( 0 );
  ret[QStringLiteral( "var-short-name" )] = params.at( 1 );

  if ( params.count() == 4 )
  {
    ret[QStringLiteral( "start-date-time" )] = params.at( 2 );
    ret[QStringLiteral( "end-date-time" )] = params.at( 3 );
  }

  ok = true;

  return ret;
}

QString ReosEra5Provider::buildUri( const QVariantMap &parameters, bool &ok )
{
  if ( parameters.contains( QStringLiteral( "file-or-dir-path" ) ) &&
       parameters.contains( QStringLiteral( "var-short-name" ) ) )
  {
    ok = true;
    QDateTime starTime = parameters.value( QStringLiteral( "start-date-time" ) ).toDateTime();
    QDateTime endTime =  parameters.value( QStringLiteral( "end-date-time" ) ).toDateTime();
    starTime.setTimeSpec( Qt::UTC );
    endTime.setTimeSpec( Qt::UTC );
    return buildUri( parameters.value( QStringLiteral( "file-or-dir-path" ) ).toString(),
                     parameters.value( QStringLiteral( "var-short-name" ) ).toString(),
                     starTime,
                     endTime );
  }
  else
  {
    ok = false;
    return QString();
  }
}

QString ReosEra5Provider::buildUri( const QString &filePath, const QString &varName, const QDateTime &startTime, const QDateTime &endTime )
{
  QString uri = QStringLiteral( "%1::%2" ).arg( filePath, varName );
  if ( startTime.isValid() && endTime.isValid() )
  {
    uri = uri + QStringLiteral( "::%1::%2" ).arg( startTime.toString( Qt::ISODate ), endTime.toString( Qt::ISODate ) );
  }

  return uri;
}

QString ReosEra5Provider::pathFromUri( const QString &uri, bool &ok )
{
  const QStringList params = uri.split( QStringLiteral( "::" ) );
  if ( params.size() < 1 )
  {
    ok = false;
    return QString();
  }

  ok = true;

  return params.at( 0 );
}

QString ReosEra5Provider::varNameFromUri( const QString &uri, bool &ok )
{
  const QStringList params = uri.split( QStringLiteral( "::" ) );
  if ( params.size() < 2 )
  {
    ok = false;
    return QString();
  }

  ok = true;

  return params.at( 1 );
}

QDateTime ReosEra5Provider::startFromUri( const QString &uri, bool &ok )
{
  const QStringList params = uri.split( QStringLiteral( "::" ) );
  if ( params.size() < 4 )
  {
    ok = false;
    return QDateTime();
  }

  const QDateTime ret = QDateTime::fromString( params.at( 2 ), Qt::ISODate );
  ok = ret.isValid();

  return ret;
}

QDateTime ReosEra5Provider::endFromUri( const QString &uri, bool &ok )
{
  const QStringList params = uri.split( QStringLiteral( "::" ) );
  if ( params.size() < 4 )
  {
    ok = false;
    return QDateTime();
  }

  const QDateTime ret = QDateTime::fromString( params.at( 3 ), Qt::ISODate );
  ok = ret.isValid();

  return ret;
}

QString ReosEra5Provider::staticKey()
{
  return ERA5_KEY + QString( "::" ) + dataType();
}

ReosGriddedDataProvider *ReosEra5ProviderFactory::createProvider( const QString &dataType ) const
{
  if ( ReosEra5Provider::dataType() == dataType )
    return new ReosEra5Provider();

  return nullptr;
}

QString ReosEra5ProviderFactory::key() const
{
  return ERA5_KEY;
}

bool ReosEra5ProviderFactory::supportType( const QString &dataType ) const
{
  return dataType.contains( ReosGriddedData::staticType() );
}

QVariantMap ReosEra5ProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
  {
    ret.insert( QStringLiteral( "file-or-dir-path" ), QObject::tr( "File or directory where are stored the data" ) );
    ret.insert( QStringLiteral( "var-short-name" ), QObject::tr( "Variable short name" ) );
    ret.insert( QStringLiteral( "start_date_time" ), QObject::tr( "Start date time (iso format) of requested data, optional." ) );
    ret.insert( QStringLiteral( "end_date_time" ), QObject::tr( "End date time (iso format) of requested data, optional." ) );
  }

  return ret;
}

QString ReosEra5ProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType ) )
  {
    ok = true;
    return ReosEra5Provider::buildUri( parameters, ok );
  }
  else
  {
    ok = false;
    return QString();
  }
}

ReosEra5NetCdfFilesReader::ReosEra5NetCdfFilesReader( const QString &fileName, const QString &varName, const QDateTime &start, const QDateTime &end )
  :  mFileName( fileName ), mVarName( varName ), mStart( start ), mEnd( end )
{
  mFile.reset( new ReosNetCdfFile( mFileName ) );
  if ( mFile->isValid() )
  {
    const QString crs = ReosGisEngine::crsFromEPSG( 4326 );
    int longitudeCount = mFile->dimensionLength( QStringLiteral( "longitude" ) );
    int latitudeCount = mFile->dimensionLength( QStringLiteral( "latitude" ) );
    double lonResol = 0.25;
    double latResol = 0.25;

    QVector<double> longs = mFile->getDoubleArray( QStringLiteral( "longitude" ), longitudeCount );
    QVector<double> lats = mFile->getDoubleArray( QStringLiteral( "latitude" ), longitudeCount );

    mExtent = ReosRasterExtent( longs.at( 0 ) - lonResol / 2, lats.at( 0 ) + latResol / 2, longitudeCount, latitudeCount, lonResol, -latResol );
    mExtent.setCrs( crs );

    mScalefactor = mFile->doubleAttributeValue( varName, QStringLiteral( "scale_factor" ) );
    mAddOffset = mFile->doubleAttributeValue( varName, QStringLiteral( "add_offset" ) );
    mFillingValue = mFile->shortAttributeValue( varName, QStringLiteral( "_FillValue" ) );
    mMissingValue = mFile->shortAttributeValue( varName, QStringLiteral( "missing_value" ) );

    int frameCount = mFile->dimensionLength( QStringLiteral( "time" ) );
    const QVector<int> intTime = mFile->getIntArray( QStringLiteral( "time" ), frameCount );
    const QDateTime oriTime( QDate( 1900, 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC );
    QMap<QDateTime, int> timeToFileIndex;
    for ( int i = 0; i < frameCount; ++i )
    {
      const QDateTime &time = oriTime.addSecs( 3600 * static_cast<qint64>( intTime.at( i ) ) );
      if ( !mStart.isValid() || !mEnd.isValid()  || ( time >= mStart && time.addSecs( 3600 ) <= mEnd ) )
      {
        if ( !timeToFileIndex.contains( time ) )
          timeToFileIndex.insert( time, i );
      }
    }
    mTimes = timeToFileIndex.keys();
    for ( int i = 0; i < mTimes.count(); ++i )
      mDataIndexToFileIndex.insert( i, timeToFileIndex.value( mTimes.at( i ) ) );
  }

  mFile.reset();
}

QVector<double> ReosEra5NetCdfFilesReader::treatRawData( const QVector<qint16> &rawData ) const
{
  QVector<double> ret( rawData.count(), std::numeric_limits<double>::quiet_NaN() );

  for ( int i = 0; i < rawData.count(); ++i )
  {
    qint16 rawValue = rawData.at( i );
    if ( rawValue != mFillingValue && rawValue != mMissingValue )
    {
      ret[i] = std::max( 0.0,   rawValue * mScalefactor + mAddOffset );
    }
  }
  return ret;
}

ReosEra5NetCdfFilesReader *ReosEra5NetCdfFilesReader::clone() const
{
  std::unique_ptr<ReosEra5NetCdfFilesReader> other = std::make_unique<ReosEra5NetCdfFilesReader>( mFileName, mVarName, mStart, mEnd );
  return other.release();
}

int ReosEra5NetCdfFilesReader::frameCount() const
{
  return mTimes.count();
}

QDateTime ReosEra5NetCdfFilesReader::time( int i ) const
{
  if ( i < 0 || i >= mTimes.count() )
    return QDateTime();
  return mTimes.at( i );
}

QVector<double> ReosEra5NetCdfFilesReader::data( int index, bool &readLine ) const
{
  if ( !mFile )
    mFile.reset( new ReosNetCdfFile( mFileName ) );

  int fileIndex = mDataIndexToFileIndex.value( index, -1 );

  const QVector<int> starts( {fileIndex, 0, 0} );
  int xCount = mExtent.xCellCount();
  int yCount = mExtent.yCellCount();
  const QVector<int> counts( {1, yCount, xCount} );

  if ( mFile->isValid() )
  {
    readLine = false;
    return treatRawData( mFile->getShortArray( mVarName, starts, counts ) );
  }

  return QVector<double>();
}

QVector<double> ReosEra5NetCdfFilesReader::dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const
{
  if ( !mFile )
    mFile.reset( new ReosNetCdfFile( mFileName ) );

  int fileIndex = mDataIndexToFileIndex.value( index, -1 );

  const QVector<int> starts( {fileIndex, colMin, rowMin} );
  int xCount = colMax - colMin + 1;
  int yCount = rowMax - rowMin + 1;
  const QVector<int> counts( {1, xCount, yCount} );

  if ( mFile->isValid() )
  {
    readLine = false;
    return treatRawData( mFile->getShortArray( mVarName, starts, counts ) );
  }

  return QVector<double>();
}

ReosRasterExtent ReosEra5NetCdfFilesReader::extent() const
{
  return mExtent;
}

bool ReosEra5NetCdfFilesReader::getDirectMinMax( double &min, double &max ) const
{
  return false;
}

QVector<int> ReosEra5NetCdfFilesReader::qualifData( int index, bool &readLine ) const
{
  if ( !mFile )
    mFile.reset( new ReosNetCdfFile( mFileName ) );

  int fileIndex = mDataIndexToFileIndex.value( index, -1 );

  const QVector<int> starts( {fileIndex, 0, 0} );
  int xCount = mExtent.xCellCount();
  int yCount = mExtent.yCellCount();
  const QVector<int> counts( {1, xCount, yCount} );

  if ( mFile->isValid() )
  {
    readLine = false;
    return mFile->getIntArray( QStringLiteral( "QUALIF" ), starts, counts );
  }

  return QVector<int>();
}

bool ReosEra5NetCdfFilesReader::canReadFile( const QString &uri )
{
  bool ok = false;
  const QVariantMap &decodedUri = ReosEra5Provider::decodeUri( uri, ok );
  if ( !ok )
    return false;

  const QString &fileName = decodedUri.value( QStringLiteral( "file-or-dir-path" ) ).toString();
  const QString &varName = decodedUri.value( QStringLiteral( "var-short-name" ) ).toString();

  ReosNetCdfFile file( fileName );
  if ( !file.isValid() )
    return false;

  if ( ! file.hasVariable( varName ) )
    return false;

  const QStringList dimensionNames = file.variableDimensionNames( varName );

  return dimensionNames.contains( QStringLiteral( "latitude" ) ) &&
         dimensionNames.contains( QStringLiteral( "longitude" ) ) &&
         dimensionNames.contains( QStringLiteral( "time" ) );
}

void ReosEra5NetCdfFilesReader::reset()
{
  mFile.reset();
}

ReosEra5NetCdfFolderReader::ReosEra5NetCdfFolderReader( const QString &folderPath, const QString &varName, const QDateTime &start, const QDateTime &end )
  : mFolderPath( folderPath ), mVarName( varName ), mStart( start ), mEnd( end )
{
  std::cout << "Parse folder " << folderPath.toStdString() << "." << std::endl;
  QDir dir( folderPath );

  QStringList filters;
  filters << "*.nc";
  const QStringList entries = dir.entryList( filters, QDir::Files );

  QMap<QDateTime, size_t> firstTimeToIndex;
  for ( const QString &file : entries )
  {
    const QString filePath = dir.filePath( file );
    if ( ReosEra5NetCdfFolderReader::canReadFile( ReosEra5Provider::buildUri( folderPath, varName, mStart, mEnd ) ) )
    {
      std::unique_ptr<ReosEra5NetCdfFilesReader> fileReader = std::make_unique<ReosEra5NetCdfFilesReader>( filePath, varName, mStart, mEnd );
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
    ReosEra5NetCdfFilesReader *reader = mFileReaders.at( fileIndex ).get();
    int frameCount = reader->frameCount();
    for ( int i = 0; i < frameCount; ++i )
    {
      mGlobalIndexToReaderIndex.insert( count, InternalIndex{fileIndex, i} );
      count++;
    }
  }

  mLastFileIndex = mFileReaders.size();
  std::cout << "Files count: " << mFileReaders.size() << "." << std::endl;
}

ReosEra5FilesReader *ReosEra5NetCdfFolderReader::clone() const
{
  return new ReosEra5NetCdfFolderReader( mFolderPath, mVarName, mStart, mEnd );
}

int ReosEra5NetCdfFolderReader::frameCount() const
{
  return mGlobalIndexToReaderIndex.count();
}

QDateTime ReosEra5NetCdfFolderReader::time( int i ) const
{
  auto it = mGlobalIndexToReaderIndex.find( i );
  if ( it != mGlobalIndexToReaderIndex.constEnd() )
    return mFileReaders.at( it.value().fileIndex )->time( it.value().internIndex );

  return QDateTime();
}

ReosEra5NetCdfFilesReader *ReosEra5NetCdfFolderReader::fileReader( int index, int &interIndex ) const
{
  auto it = mGlobalIndexToReaderIndex.find( index );
  if ( it != mGlobalIndexToReaderIndex.constEnd() )
  {
    size_t fileIndex = it.value().internIndex;
    ReosEra5NetCdfFilesReader *ret = mFileReaders.at( it.value().fileIndex ).get();
    interIndex = it.value().internIndex;
    if ( mLastFileIndex != mFileReaders.size() && mLastFileIndex != fileIndex )
    {
      mFileReaders.at( mLastFileIndex )->reset();
      mLastFileIndex = fileIndex;
    }

    return ret;
  }

  return nullptr;
}


QVector<double> ReosEra5NetCdfFolderReader::data( int index, bool &readLine ) const
{
  int interIndex = -1;
  ReosEra5NetCdfFilesReader *fr = fileReader( index, interIndex );
  if ( fr )
    return fr->data( interIndex, readLine );

  return QVector<double>();
}

QVector<double> ReosEra5NetCdfFolderReader::dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const
{
  int interIndex = -1;
  ReosEra5NetCdfFilesReader *fr = fileReader( index, interIndex );
  if ( fr )
    return fr->dataInGridExtent( interIndex, rowMin, rowMax, colMin, colMax, readLine );

  return QVector<double>();
}

ReosRasterExtent ReosEra5NetCdfFolderReader::extent() const
{
  return mExtent;
}

bool ReosEra5NetCdfFolderReader::getDirectMinMax( double &min, double &max ) const
{
  return false;
}

QVector<int> ReosEra5NetCdfFolderReader::qualifData( int index, bool &readLine ) const
{
  auto it = mGlobalIndexToReaderIndex.find( index );
  if ( it != mGlobalIndexToReaderIndex.constEnd() )
    return mFileReaders.at( it.value().fileIndex )->qualifData( it.value().internIndex, readLine );

  return QVector<int>();
}

bool ReosEra5NetCdfFolderReader::canReadFile( const QString &uri )
{
  bool ok = false;
  QVariantMap decodedUri = ReosEra5Provider::decodeUri( uri, ok );
  if ( !ok )
    return false;

  const QString &dirPath = decodedUri.value( QStringLiteral( "file-or-dir-path" ) ).toString();

  QDir dir( dirPath );
  if ( !dir.exists() )
    return false;
  QStringList filters;
  filters << "*.nc";
  const QStringList entries = dir.entryList( filters, QDir::Files );

  for ( const QString &file : entries )
  {
    const QString filePath = dir.filePath( file );
    decodedUri[QStringLiteral( "file-or-dir-path" )] = filePath;
    const QString &fileUri = ReosEra5Provider::buildUri( decodedUri, ok );
    if ( ok && ReosEra5NetCdfFilesReader::canReadFile( fileUri ) )
      return true;
  }

  return false;
}

