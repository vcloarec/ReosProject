/***************************************************************************
  reoshdf5.cpp - ReosHdf5

 ---------------------
 begin                : 16.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshdf5.h"
#include "cstring"

ReosHdf5File::ReosHdf5File( const QString &fileName )
  : id( H5Fopen( fileName.toUtf8().constData(), H5F_ACC_RDONLY, H5P_DEFAULT ) )
{
}

ReosHdf5File::~ReosHdf5File()
{
  H5Fclose( id );
}

bool ReosHdf5File::isValid() const
{
  return id != H5I_INVALID_HID;
}

bool ReosHdf5File::pathExists( const QString &path ) const
{
  return H5Lexists( id, path.toUtf8().constData(), H5P_DEFAULT ) > 0;
}

ReosHdf5Group ReosHdf5File::createGroup( const QString &path ) const
{
  return ReosHdf5Group( path, id );
}

ReosHdf5Group::~ReosHdf5Group()
{
  mData->ref--;
  if ( mData->ref == 0 )
  {
    H5Gclose( mData->id );
  }
}

ReosHdf5Group::ReosHdf5Group( const ReosHdf5Group &other )
{
  mData = other.mData;
  mData->ref++;
}

ReosHdf5Group &ReosHdf5Group::operator=( const ReosHdf5Group &other )
{
  mData->ref--;
  if ( mData->ref == 0 )
    H5Aclose( mData->id );

  mData = other.mData;
  mData->ref++;
  return *this;
}

bool ReosHdf5Group::isValid() const
{
  return mData->id != H5I_INVALID_HID;
}

ReosHdf5Attribute ReosHdf5Group::attribute( const QString &name ) const
{
  return ReosHdf5Attribute( name, mData->id );
}

ReosHdf5Group::ReosHdf5Group( const QString &path, hid_t file )
{
  mData = std::make_shared<Data>();

  mData->id = H5Gopen( file, path.toUtf8().constData(), H5P_DEFAULT );
  mData->ref = 1;
}

ReosHdf5Attribute::~ReosHdf5Attribute()
{
  mData->ref--;
  if ( mData->ref == 0 )
  {
    H5Aclose( mData->id );
  }
}

ReosHdf5Attribute::ReosHdf5Attribute( const ReosHdf5Attribute &other )
{
  mData = other.mData;
  mData->ref++;
}

ReosHdf5Attribute &ReosHdf5Attribute::operator=( const ReosHdf5Attribute &other )
{
  mData->ref--;
  if ( mData->ref == 0 )
    H5Aclose( mData->id );

  mData = other.mData;
  mData->ref++;
  return *this;
}

bool ReosHdf5Attribute::isValid() const
{
  return mData->id != H5I_INVALID_HID;
}

QString ReosHdf5Attribute::readString( size_t preSize ) const
{
  hid_t dtId = H5Aget_type( mData->id );
  std::string str;
  str.resize( preSize );
  herr_t status = H5Aread( mData->id, dtId, str.data() );
  H5Tclose( dtId );
  if ( status == -1 )
    return QString();
  return QString( str.c_str() );
}

int ReosHdf5Attribute::readInt( bool &ok ) const
{
  hid_t dtId = H5Aget_type( mData->id );
  int ret;
  herr_t status = H5Aread( mData->id, dtId, &ret );
  H5Tclose( dtId );
  if ( status == -1 )
  {
    ok = false;
    return 0;
  }
  ok = true;
  return ret;
}


ReosHdf5Attribute::ReosHdf5Attribute( const QString &name, hid_t group )
{
  mData = std::make_shared<Data>();
  mData->id = H5Aopen( group, name.toUtf8().constData(), H5P_DEFAULT );
  mData->ref = 1;
}
