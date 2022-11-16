/***************************************************************************
  reosgriddedrainfallprovider.cpp - ReosGriddedRainfallProvider

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
#include "reosgriddedrainfallprovider.h"
#include "reosgriddedrainitem.h"

ReosGriddedRainfallProvider::~ReosGriddedRainfallProvider()
{}

void ReosGriddedRainfallProvider::setDataSource( const QString &uri )
{
  mDataSource = uri;
}

void ReosGriddedRainfallProvider::setSourceValueType( ValueType valueType )
{
  mSourceValueType = valueType;
}


QString ReosGriddedRainfallMemoryProvider::key() const
{
  return staticKey();
}

QString ReosGriddedRainfallMemoryProvider::dataType()
{
  return ReosGriddedRainfall::staticType();
}

QString ReosGriddedRainfallMemoryProvider::staticKey()
{
    return  QString( QStringLiteral( "gridded-rainfall-memory" ) );
}

void ReosGriddedRainfallMemoryProvider::setExtent(const ReosRasterExtent &newExtent)
{
    mExtent = newExtent;
}

ReosGriddedRainfallProvider *ReosGriddedRainfallMemoryProviderFactory::createProvider( const QString & ) const
{
  return new ReosGriddedRainfallMemoryProvider;
}

QString ReosGriddedRainfallMemoryProviderFactory::key() const
{
  return ReosGriddedRainfallMemoryProvider::staticKey();
}
