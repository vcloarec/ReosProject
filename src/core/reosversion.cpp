/***************************************************************************
                      reosversion.cpp
                     --------------------------------------
Date                 : 07-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosversion.h"

ReosVersion::ReosVersion( const QString &name, int major, int minor, int sub ):
  mSoftName( name ),
  mMajor( major ),
  mMinor( minor ),
  mSub( sub )
{

}

bool ReosVersion::operator==( const ReosVersion &other )
{
  return ( mSoftName == other.mSoftName && mMajor == other.mMajor && mMinor == other.mMinor && mSub == other.mSub );
}

bool ReosVersion::operator>( const ReosVersion &other )
{
  if ( mSoftName != other.mSoftName )
    return false;

  if ( mMajor > other.mMajor )
    return true;

  if ( mMajor == other.mMajor && mMinor > other.mMinor )
    return true;

  if ( mMajor == other.mMajor && mMinor == other.mMinor && mSub > other.mSub )
    return true;

  return false;
}

bool ReosVersion::operator<( const ReosVersion &other )
{
  if ( mSoftName != other.mSoftName )
    return false;

  return ( !operator>( other ) && !operator==( other ) );
}

QString ReosVersion::getSoftName() const
{
  return mSoftName;
}

void ReosVersion::setSoftName( const QString &value )
{
  mSoftName = value;
}

QString ReosVersion::softwareNameWithVersion() const
{
  QString returnValue = mSoftName;
  returnValue.append( ' ' );
  returnValue.append( stringVersion() );

  return returnValue;
}

QString ReosVersion::stringVersion() const
{
  QString version = QObject::tr( "version" );
  version.append( QString( " %1.%2.%3" ).arg( mMajor ).arg( mMinor ).arg( mSub ) );
  return version;
}
