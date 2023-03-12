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
#include "reosidfcurves.h"

ReosRainfallRegistery *ReosRainfallRegistery::sRainfallRegistery = nullptr;

ReosRainfallRegistery::ReosRainfallRegistery( ReosModule *parentModule )
  : ReosModule( QStringLiteral("rainfall-registery"), parentModule )
{
  mRainfallModel = new ReosRainfallModel( this );

  ReosIdfFormulaRegistery::instantiate( this );

  connect( mRainfallModel, &ReosRainfallModel::loaded, this, [this]( const QString & filePath )
  {
    QString text = tr( "Rainfall data loaded from file: %1" ).arg( filePath );
    message( text );
  } );

  connect( mRainfallModel, &ReosRainfallModel::saved, this, [this]( const QString & filePath )
  {
    QString text = tr( "Rainfall data saved to file:%1" ).arg( filePath );
    message( text );
  } );
}


void ReosRainfallRegistery::instantiate( ReosModule *parentModule )
{
  if ( !sRainfallRegistery )
    sRainfallRegistery = new ReosRainfallRegistery( parentModule );
}

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
  return mRainfallModel;
}

ReosRainfallItem *ReosRainfallRegistery::itemByUri( const QString &uri ) const
{
  return mRainfallModel->uriToItem( uri );
}

ReosRainfallItem *ReosRainfallRegistery::itemByUniqueId( const QString &uid ) const
{
  return mRainfallModel->uniqueIdToItem( uid );
}
