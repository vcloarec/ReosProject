/***************************************************************************
  reostelemacsteeringfile.h - ReosTelemacSteeringFile

 ---------------------
 begin                : 2.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reostelemacsteeringfile.h"
#include <QFile>
#include <QTextStream>


ReosTelemacSteeringFile::ReosTelemacSteeringFile( const QString &path )
  : mPath( path )
{}

void ReosTelemacSteeringFile::open()
{
  QFile file( mPath );
  if ( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
    return;

  QTextStream in( &file );
  while ( !in.atEnd() )
  {
    const QString line = in.readLine().trimmed();
    if ( line.isEmpty() )
      continue;

    std::unique_ptr<SteeringLine> steeringLine = std::make_unique<SteeringLine>( line );
    if ( steeringLine->key() != QStringLiteral( "COMMENT" ) )
      mLinesMap.insert( steeringLine->key(), steeringLine.get() );
    mLines.push_back( std::move( steeringLine ) );
  }
}

void ReosTelemacSteeringFile::setKey( const QString &key, const QString &value )
{
  SteeringLine *line = mLinesMap.value( key, nullptr );
  if ( line )
  {
    line->setValue( value );
  }
  else
  {
    std::unique_ptr<SteeringLine> newLine = std::make_unique<SteeringLine>( key, value );
    mLinesMap.insert( key, newLine.get() );
    mLines.push_back( std::move( newLine ) );
  }
}

int ReosTelemacSteeringFile::keyCount() const
{
  return mLinesMap.count();
}

int ReosTelemacSteeringFile::lineCount() const
{
  return mLines.size();
}

ReosTelemacSteeringFile::SteeringLine::SteeringLine( const QString &line )
{
  //example : TREATMENT OF FLUXES AT THE BOUNDARIES = 2;2
  // We want to have key='TREATMENT OF FLUXES AT THE BOUNDARIES' and value='2;2'
  if ( line.isEmpty() )
    return;

  if ( line.startsWith( '/' ) ) // comment line
  {
    mKey = "COMMENT";
    mValue = line.mid( 1 ).trimmed();
  }
  else
  {
    mKey = line.section( '=', 0, 0 ).trimmed();
    mValue = line.section( '=', 1 ).trimmed();
  }
}

void ReosTelemacSteeringFile::SteeringLine::setValue( const QString &value )
{
  mValue = value;
}

ReosTelemacSteeringFile::SteeringLine::SteeringLine( const QString &key, const QString &value )
  : mKey( key )
  , mValue( value )
{}

void ReosTelemacSteeringFile::save( const QString &path )
{
  if ( !path.isEmpty() )
    mPath = path;

  QFile file( mPath );
  if ( !file.open( QIODevice::WriteOnly | QIODevice::Text ) )
    return;
  for ( const std::unique_ptr<SteeringLine> &line : mLines )
  {
    if ( line->key() == QStringLiteral( "COMMENT" ) )
      file.write( QString( "/ %1\n" ).arg( line->value() ).toUtf8() );
    else
      file.write( QString( "%1 = %2\n" ).arg( line->key(), line->value() ).toUtf8() );
  }
}

QString ReosTelemacSteeringFile::value( const QString &key ) const
{
  return mLinesMap.value( key, nullptr ) ? mLinesMap.value( key )->value() : QString();
}

void ReosTelemacSteeringFile::addComment( const QString &comment )
{
  mLines.push_back( std::make_unique<SteeringLine>( "COMMENT", comment ) );
}

QString ReosTelemacSteeringFile::SteeringLine::key() const
{
  return mKey;
}

QString ReosTelemacSteeringFile::SteeringLine::value() const
{
  return mValue;
}
