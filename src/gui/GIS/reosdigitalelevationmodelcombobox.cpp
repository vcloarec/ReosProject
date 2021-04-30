/***************************************************************************
                      reosdigitalelevationmodelcombobox.cpp
                     --------------------------------------
Date                 : October-2020
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

#include "reosdigitalelevationmodelcombobox.h"


ReosDigitalElevationModelComboBox::ReosDigitalElevationModelComboBox( QWidget *parent, ReosGisEngine *gisEngine ): QComboBox( parent )
{
  setGisEngine( gisEngine );

  connect( this, QOverload<int>::of( &QComboBox::currentIndexChanged ), [this]()
  {
    emit this->currentDigitalElevationChanged( currentData().toString() );
  } );
}

void ReosDigitalElevationModelComboBox::setGisEngine( ReosGisEngine *gisEngine )
{
  if ( !gisEngine )
    return;

  if ( mGisEngine )
    disconnect( mGisEngine, &ReosGisEngine::updated, this, &ReosDigitalElevationModelComboBox::onDemChanged );

  mGisEngine = gisEngine;
  updateItems();

  connect( mGisEngine, &ReosGisEngine::updated, this, &ReosDigitalElevationModelComboBox::onDemChanged );
}

QString ReosDigitalElevationModelComboBox::currentDemLayerId() const
{
  return currentData().toString();
}

void ReosDigitalElevationModelComboBox::setCurrentDemLayer( const QString &layerId )
{
  if ( !mGisEngine->isDigitalElevationModel( layerId ) )
    return;

  setCurrentIndex( findData( layerId ) );
}

void ReosDigitalElevationModelComboBox::onDemChanged()
{
  QString currentId = currentData().toString();
  updateItems();
  int currentIndex = findData( currentId );
  if ( currentIndex >= 0 )
    setCurrentIndex( currentIndex );
  else
    setCurrentIndex( 0 );
}

void ReosDigitalElevationModelComboBox::updateItems()
{
  if ( !mGisEngine )
    return;
  clear();
  QString currentId = currentData().toString();
  QMap<QString, QString> demList = mGisEngine->digitalElevationModelRasterList();
  for ( const QString &id : demList.keys() )
    addItem( demList[id], id );
  if ( currentId != currentData().toString() )
    emit currentDigitalElevationChanged( currentData().toString() );
}
