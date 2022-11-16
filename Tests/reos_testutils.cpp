/***************************************************************************
                      reos_testutils.cpp
                     --------------------------------------
Date                 : 04-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reos_testutils.h"
#include "reosprocess.h"
#include "QDir"
#include <filesystem>

const char *data_path()
{
  return TESTDATA;
}

std::string test_file( std::string basename )
{
  std::string path( data_path() );
  path += '/' + basename;
  return path;
}

std::string tmp_file( std::string basename )
{
  std::string path( data_path() + std::string( "/tmp" ) );
  std::filesystem::path tmpPath( path );
  if ( !std::filesystem::exists( path ) )
    std::filesystem::create_directory( tmpPath );
  path += '/' + basename;
  return path;
}

QString testFile( const QString &baseName )
{
  QString path( data_path() );
  path.append( QString( '/' ) + baseName );
  return path;
}

QString tempFile( const QString &baseName )
{
  QString path( QString( data_path() ) + QStringLiteral( "/tmp" ) );
  QDir dir( path );
  if ( !dir.exists() )
    dir.mkpath( path );
  path.append( QString( '/' ) + baseName );
  return path;
}


ModuleProcessControler::ModuleProcessControler( ReosProcess *process ): mProcess( process )
{
  QObject::connect( process, &ReosProcess::finished, this, &ModuleProcessControler::processFinished );
  process->startOnOtherThread();
}

void ModuleProcessControler::waitForFinished()
{
  if ( mProcess && !mProcess->isFinished() )
    mEventLoop.exec();
}

void ModuleProcessControler::processFinished()
{
  mEventLoop.exit();
}

bool equal( double a, double b, double precision )
{
  return std::fabs( a - b ) < precision;
}

bool equal( const QPolygonF &poly1, const QPolygonF &poly2 )
{
  if ( poly1.count() != poly2.count() )
    return false;

  if ( poly1.empty() )
    return false;

  int pos2 = 0;
  int count = poly2.count();
  while ( pos2 < poly2.count() )
  {
    if ( poly1.first() == poly2.at( pos2 ) )
      break;
    else
      pos2++;
  }

  for ( int i = 0; i < count; ++i )
  {
    if ( poly1.at( i ) != poly2.at( ( pos2 + i ) % count ) )
      return false;
  }

  return true;
}
