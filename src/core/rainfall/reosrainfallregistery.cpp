/***************************************************************************
  reosrainfallregistery.cpp - ReosRainfallRegistery

 ---------------------
 begin                : 11.2.2021
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
#include "reosrainfallregistery.h"

#include "reosrainfallmodel.h"
#include "reosrainfallitem.h"

ReosRainfallRegistery *ReosRainfallRegistery::sRainfallRegistery = nullptr;

ReosRainfallRegistery::ReosRainfallRegistery()
{
  mRainfallModel.reset( new ReosRainfallModel );
}

ReosRainfallRegistery::~ReosRainfallRegistery() {}

ReosRainfallRegistery *ReosRainfallRegistery::instance()
{
  if ( !sRainfallRegistery )
    sRainfallRegistery = new ReosRainfallRegistery();

  return sRainfallRegistery;
}

bool ReosRainfallRegistery::isInstantiate()
{
  return sRainfallRegistery != nullptr;
}

ReosRainfallModel *ReosRainfallRegistery::rainfallModel() const
{
  return mRainfallModel.get();
}

ReosRainfallItem *ReosRainfallRegistery::item( const QString &uri ) const
{
  return mRainfallModel->uriToItem( uri );
}
