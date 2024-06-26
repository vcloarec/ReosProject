/***************************************************************************
  reosnetcdfutils.cpp - ReosNetCdfUtils

 ---------------------
 begin                : 25.12.2022
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
#include "reosnetcdfutils.h"

#include <limits>
#include <netcdf.h>
#include <QVector>

#ifdef _MSC_VER
#include <windows.h>
#define UNICODE
#include <locale>
#include <codecvt>
#include <stringapiset.h>
#endif


#ifdef _MSC_VER
static std::string utf8ToWin32Recode( const std::string &utf8String )
{
  //from GDAL: ./port/cpl_recode_stub.cpp, CPLWin32Recode()

  // Compute length in wide characters
  int wlen = MultiByteToWideChar( CP_UTF8, 0, utf8String.c_str(), -1, nullptr, 0 );

  // do the conversion to wide char
  std::wstring wstr;
  wstr.resize( static_cast<size_t>( wlen ) + 1 );
  wstr.data()[wlen] = 0;
  MultiByteToWideChar( CP_UTF8, 0, utf8String.c_str(), -1, wstr.data(), wstr.size() );

  int len = WideCharToMultiByte( CP_ACP, 0, wstr.c_str(), -1, nullptr, 0, nullptr, nullptr );

  std::string ret;
  ret.resize( static_cast<size_t>( len ) + 1 );

  WideCharToMultiByte( CP_ACP, 0, wstr.c_str(), -1, ret.data(), ret.size(), nullptr, nullptr );

  return ret;
}
#endif

static std::string systemFileName( const QString &utf8FileName )
{
  std::string ret;
#ifdef _MSC_VER
  ret = utf8ToWin32Recode( utf8FileName.toStdString() );
#else
  ret = utf8FileName.toStdString();
#endif
  return ret;
}

ReosNetCdfFile::ReosNetCdfFile( const QString &fileName, bool write )
{
  int res = nc_open( systemFileName( fileName ).c_str(), write ? NC_WRITE : NC_NOWRITE, &mNcId );
  mIsValid = res == NC_NOERR;
  if ( !mIsValid )
    return;

  res = nc_inq( mNcId, &mDimCount, &mVarCount, &mGlobalAttCOunt, &mUnlimitDimId );
  mIsValid = res == NC_NOERR;

  QVector<int> varIds;
  varIds.resize( mVarCount );
  res = nc_inq_varids( mNcId, &mVarCount, varIds.data() );

  mIsValid = res == NC_NOERR;

  for ( int varId : std::as_const( varIds ) )
  {
    char name[NC_MAX_NAME];
    res = nc_inq_varname( mNcId, varId, name );
    if ( res == NC_NOERR )
      mVarNameToVarId.insert( QString( name ), varId );
  }

  for ( int varId : std::as_const( varIds ) )
  {
    size_t attlen = 0;
    if ( nc_inq_attlen( mNcId, varId, "long_name", &attlen ) )
      continue;
    std::string lg;
    lg.resize( attlen );
    res = nc_get_att_text( mNcId, varId, "long_name", lg.data() );
    if ( res == NC_NOERR )
      mVarLongNameToVarId.insert( QString::fromStdString( lg ), varId );
  }
}

ReosNetCdfFile::~ReosNetCdfFile()
{
  nc_close( mNcId );
}

bool ReosNetCdfFile::isValid() const
{
  return mIsValid;
}

bool ReosNetCdfFile::hasVariable( const QString &variableName )
{
  return mVarNameToVarId.contains( variableName );
}

bool ReosNetCdfFile::hasVariableByLongName( const QString &variableName )
{
  return mVarLongNameToVarId.contains( variableName );
}

int ReosNetCdfFile::variableDimensionCount( const QString &variableName ) const
{
  int dimCount = 0;
  int res = nc_inq_varndims( mNcId, mVarNameToVarId.value( variableName ), &dimCount );

  if ( res == NC_NOERR )
    return dimCount;

  return -1;
}

int ReosNetCdfFile::dimensionLength( const QString &dimensionName ) const
{
  int dimId = -1;
  int res = nc_inq_dimid( mNcId, dimensionName.toUtf8().constData(), &dimId );
  if ( res != NC_NOERR )
    return -1;

  size_t length = 0;
  res = nc_inq_dimlen( mNcId, dimId, &length );
  if ( res != NC_NOERR )
    return -1;

  return static_cast<int>( length );
}

QStringList ReosNetCdfFile::variableDimensionNames( const QString &variableName ) const
{
  QStringList ret;
  int dimCount = 0;
  int varId = mVarNameToVarId.value( variableName );
  int res = nc_inq_varndims( mNcId, varId, &dimCount );

  if ( res != NC_NOERR )
    return ret;

  QVector<int> dimId;
  dimId.resize( dimCount );
  res = nc_inq_vardimid( mNcId, varId, dimId.data() );
  if ( res != NC_NOERR )
    return ret;

  ret.reserve( dimCount );
  for ( int i = 0; i < dimCount; ++i )
  {
    char name[NC_MAX_NAME];
    res = nc_inq_dimname( mNcId, dimId.at( i ), name );
    if ( res != NC_NOERR )
      return QStringList();

    ret.append( QString( name ) );
  }

  return ret;
}

double ReosNetCdfFile::globalDoubleAttributeValue( const QString &attribureName ) const
{
  double ret = std::numeric_limits<double>::quiet_NaN();
  int res = nc_get_att_double( mNcId, NC_GLOBAL, attribureName.toUtf8().constData(), &ret );

  if ( res != NC_NOERR )
    return std::numeric_limits<double>::quiet_NaN();

  return ret;
}

QString ReosNetCdfFile::globalStringAttributeValue( const QString &attribureName ) const
{
  size_t len = 0;
  int res  = nc_inq_attlen( mNcId, NC_GLOBAL, attribureName.toUtf8().constData(), &len );
  if ( res != NC_NOERR )
    return QString();
  std::vector<char> retStr( len + 1 );
  res = nc_get_att_text( mNcId, NC_GLOBAL, attribureName.toUtf8().constData(), retStr.data() );
  retStr[len] = '\0';
  if ( res != NC_NOERR )
    return QString();

  return QString( reinterpret_cast<char *>( retStr.data() ) );
}

double ReosNetCdfFile::doubleAttributeValue( const QString &variableName, const QString &attributeName ) const
{
  int varId = mVarNameToVarId.value( variableName );
  double ret = 0;
  int res = nc_get_att_double( mNcId, varId, attributeName.toUtf8().constData(), &ret );
  if ( res != NC_NOERR )
    return std::numeric_limits<double>::quiet_NaN();
  return ret;
}

qint16 ReosNetCdfFile::shortAttributeValue( const QString &variableName, const QString &attributeName ) const
{
  int varId = mVarNameToVarId.value( variableName );
  qint16 ret = 0;
  int res = nc_get_att_short( mNcId, varId, attributeName.toUtf8().constData(), &ret );
  if ( res != NC_NOERR )
    return 0;
  return ret;
}

QVector<qint64> ReosNetCdfFile::getInt64Array( const QString &variableName, int size )
{
  int varId = mVarNameToVarId.value( variableName );
  QVector<qint64> ret( size );
  int res = nc_get_var_longlong( mNcId, varId, ret.data() );
  if ( res != NC_NOERR )
    return QVector<qint64> ();

  return ret;
}

QVector<int> ReosNetCdfFile::getIntArray( const QString &variableName, int size ) const
{
  int varId = mVarNameToVarId.value( variableName );
  QVector<int> ret( size );
  int res = nc_get_var_int( mNcId, varId, ret.data() );
  if ( res != NC_NOERR )
    return QVector<int> ();

  return ret;
}

static std::vector<size_t> int_array_to_size_t_array( const QVector<int> &int_array )
{
  std::vector<size_t> size_t_array( static_cast<size_t>( int_array.count() ) );
  for ( int i = 0; i < int_array.count(); ++i )
    size_t_array[static_cast<size_t>( i )] = static_cast<size_t>( int_array.at( i ) );

  return size_t_array;
}

QVector<int> ReosNetCdfFile::getIntArray( const QString &variableName, const QVector<int> &starts, const QVector<int> &counts ) const
{
  Q_ASSERT( starts.count() == counts.count() );
  int varId = mVarNameToVarId.value( variableName );

  std::vector<size_t> startp = int_array_to_size_t_array( starts );
  std::vector<size_t> countp = int_array_to_size_t_array( counts );
  int totalSize = 1;
  for ( int i = 0; i < counts.count(); ++i )
    totalSize *= counts.at( i );

  QVector<int> ret;
  ret.resize( totalSize );
  int res = nc_get_vara_int( mNcId, varId, startp.data(), countp.data(), ret.data() );

  if ( res == NC_NOERR )
    return ret;

  return QVector<int>();
}

QVector<double> ReosNetCdfFile::getDoubleArray( const QString &variableName, int size )
{
  int varId = mVarNameToVarId.value( variableName );
  QVector<double> ret( size );
  int res = nc_get_var_double( mNcId, varId, ret.data() );
  if ( res != NC_NOERR )
    return QVector<double> ();

  return ret;
}

QVector<double> ReosNetCdfFile::getDoubleArray( const QString &variableName, const QVector<int> &starts, const QVector<int> &counts ) const
{
  Q_ASSERT( starts.count() == counts.count() );
  int varId = mVarNameToVarId.value( variableName );

  std::vector<size_t> startp = int_array_to_size_t_array( starts );
  std::vector<size_t> countp = int_array_to_size_t_array( counts );
  int totalSize = 1;
  for ( int i = 0; i < counts.count(); ++i )
    totalSize *= counts.at( i );

  QVector<double> ret;
  ret.resize( totalSize );
  int res = nc_get_vara_double( mNcId, varId, startp.data(), countp.data(), ret.data() );

  if ( res == NC_NOERR )
    return ret;

  return QVector<double>();
}

QVector<qint16> ReosNetCdfFile::getShortArray( const QString &variableName, const QVector<int> &starts, const QVector<int> &counts ) const
{
  Q_ASSERT( starts.count() == counts.count() );
  int varId = mVarNameToVarId.value( variableName );

  std::vector<size_t> startp = int_array_to_size_t_array( starts );
  std::vector<size_t> countp = int_array_to_size_t_array( counts );
  int totalSize = 1;
  for ( int i = 0; i < counts.count(); ++i )
    totalSize *= counts.at( i );

  QVector<qint16> ret;
  ret.resize( totalSize );
  int res = nc_get_vara_short( mNcId, varId, startp.data(), countp.data(), ret.data() );

  if ( res == NC_NOERR )
    return ret;

  return QVector<qint16>();
}
