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


ReosDssFile::ReosDssFile( const QString &filePath, bool create )
{
  QFileInfo fileInfo( filePath );
  if ( !create && !fileInfo.exists() )
    return;

  mIfltab.reset( new std::array<long long, 250> );
  mStatus = zopen( mIfltab->data(), filePath.toStdString().c_str() );
  qDebug() << mStatus;
  mIsOpen = mStatus == STATUS_OKAY;
  mIsValid = mIsOpen;
}

ReosDssFile::~ReosDssFile()
{
  if ( mIfltab )
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

bool ReosDssFile::addHydrograph( ReosHydrograph *hydrograph, const QDateTime &startTime, const ReosDuration &interval )
{
  if ( !mIsOpen || !mIsValid || mIfltab )
    return false;

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
