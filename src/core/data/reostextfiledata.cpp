/***************************************************************************
  reostextfiledata.cpp - ReosTextFileData

 ---------------------
 begin                : 9.2.2021
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
#include "reostextfiledata.h"

ReosTextFileData::ReosTextFileData( QObject *parent )
{}

bool ReosTextFileData::setFileName( const QString &fileName )
{
  beginResetModel();
  mFileName = fileName;
  bool result = parsePreview();
  endResetModel();

  return result;
}

void ReosTextFileData::setLines( int headerLine, int firstDataline )
{
  beginResetModel();
  mHeaderLine = headerLine;
  mFirstDataLine = firstDataline;
  mPreviewLineMaxCount = std::max( mHeaderLine, mPreviewLineMaxCount );
  parsePreview();
  endResetModel();
}

int ReosTextFileData::headerLine() const
{
  return mHeaderLine;
}

int ReosTextFileData::firstDataLine() const
{
  return mFirstDataLine;
}

int ReosTextFileData::previewLineMaxCount() const
{
  return mPreviewLineMaxCount;
}

void ReosTextFileData::setPreviewLineMaxCount( int previewLineCount )
{
  mPreviewLineMaxCount = previewLineCount;
}

void ReosTextFileData::setDelimiters( const QStringList &delimiters )
{
  mDelimiters = delimiters;
  QStringList special;
  special << QString( '[' )
          << QString( ']' )
          << QString( '(' )
          << QString( ')' )
          << QString( '#' )
          << QString( '&' )
          << QString( '$' )
          << QString( '%' )
          << QString( '^' )
          << QString( '|' ) ;

  QStringList list;
  for ( const QString &str : delimiters )
  {
    if ( str == tr( "space" ) )
      list.append( QStringLiteral( " +" ) );
    else  if ( str.size() > 1 )
      list.append( str + QString( '+' ) );
    else if ( special.contains( str ) )
      list.append( QStringLiteral( "\\" ) + str );
    else if ( !str.isEmpty() )
      list.append( str );
  }

  mRegExpDelimiters = QRegularExpression( list.join( '|' ) );

  beginResetModel();
  parsePreview();
  endResetModel();
}

QModelIndex ReosTextFileData::index( int row, int column, const QModelIndex &parent ) const
{
  Q_UNUSED( parent );
  return createIndex( row, column );
}

QModelIndex ReosTextFileData::parent( const QModelIndex &child ) const
{
  Q_UNUSED( child );
  return QModelIndex();
}

int ReosTextFileData::rowCount( const QModelIndex & ) const
{
  return mPreviewData.count() - mFirstDataLine + 1;
}

int ReosTextFileData::columnCount( const QModelIndex & ) const
{
  return mHeaders.count();
}

QVariant ReosTextFileData::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    int r = index.row();
    int c = index.column();
    int lineIndex = mFirstDataLine - 1 +  r;

    if ( lineIndex < mPreviewData.count() )
    {
      QString line = mPreviewData.at( mFirstDataLine - 1 +  r );
      QStringList data = splitLine( line );
      if ( c < data.count() )
        return data.at( c );
    }
  }

  return QVariant();
}

QVariant ReosTextFileData::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical && role == Qt::DisplayRole )
  {
    return section + mFirstDataLine;
  }

  if ( orientation == Qt::Horizontal && role == Qt::DisplayRole )
  {
    if ( section < mHeaders.count() )
      return mHeaders.at( section );
  }

  return QVariant();

}

QVector<QString> ReosTextFileData::columnValues( int columnIndex )
{
  QVector<QString> ret;

  if ( columnIndex < 0 )
    return ret;

  QFile file( mFileName );

  if ( !file.open( QIODevice::ReadOnly ) )
    return ret;

  QTextStream stream( &file );

  int lineCounter = 1;
  while ( !stream.atEnd() )
  {
    QString line = readLine( stream );
    if ( lineCounter >= mFirstDataLine )
    {
      QStringList row = splitLine( line );
      if ( columnIndex >= row.size() )
        ret.append( QString() );
      else
        ret.append( row.at( columnIndex ) );
    }

    ++lineCounter;
  }

  return ret;
}

QStringList ReosTextFileData::delimiters() const
{
  return mDelimiters;
}

QStringList ReosTextFileData::headers() const {return mHeaders;}

bool ReosTextFileData::parsePreview()
{
  QFile file( mFileName );
  mHeaders.clear();
  mPreviewData.clear();

  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QTextStream stream( &file );

  findEOL( stream );

  for ( int i = 0; i < mPreviewLineMaxCount; ++i )
  {
    if ( stream.atEnd() )
      break;
    else
      mPreviewData.append( readLine( stream ) );
  }

  if ( mPreviewData.empty() )
    return false;
  if ( mHeaderLine == 0 )
  {
    int count = splitLine( mPreviewData.at( 0 ) ).count();
    for ( int i = 0; i < count; ++i )
    {
      mHeaders.append( tr( "Field %1" ).arg( i + 1 ) );
    }
  }
  else
  {
    mHeaders = splitLine( mPreviewData.at( mHeaderLine - 1 ) );
  }

  emit headersChanged( mHeaders );

  return true;
}

QStringList ReosTextFileData::splitLine( const QString &line ) const
{

  if ( mRegExpDelimiters.isValid() && !mRegExpDelimiters.pattern().isEmpty() )
    return line.split( mRegExpDelimiters );
  else
    return QStringList( line );

}

void ReosTextFileData::findEOL( QTextStream &stream )
{
  mFirstEOLChar = 0;
  while ( !stream.atEnd() && mFirstEOLChar == 0 )
  {
    const QString str = stream.read( 1 ); //TODO : mabe change the lengh of reading with another value
    const QChar *strChar = str.constData();
    for ( int i = 0; i < str.size(); ++i )
      if ( strChar[i] == '\r' || strChar[i] == '\n' )
      {
        mFirstEOLChar = strChar[i];
        break;
      }
  }

  stream.seek( 0 );
}

QString ReosTextFileData::readLine( QTextStream &stream )
{
  // We should always use mStream->readLine(), but it fails to detect \r
  // line endings.
  if ( mFirstEOLChar == '\r' )
  {
    QString line;
    bool lineFinished = false;
    while ( !stream.atEnd() && !lineFinished )
    {
      const QString str = stream.read( 1 );
      const QChar *strChar = str.constData();
      if ( strChar == mFirstEOLChar )
        lineFinished = true;
      else
        line.append( str );
    }
    return line;
  }
  else
  {
    return stream.readLine();
  }
}
