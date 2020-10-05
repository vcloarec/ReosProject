/***************************************************************************
                      reoswatersheddelineating.cpp
                     --------------------------------------
Date                 : 04-10-2020
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

#include "reoswatersheddelineating.h"

ReosWatershedDelineating::ReosWatershedDelineating( ReosModule *parent, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mGisEngine( gisEngine )
{}

bool ReosWatershedDelineating::hasValidDigitalElevationModel() const
{
  if ( !mGisEngine )
    return false;

  return mGisEngine->isDigitalElevationModel( mDEMLayerId ) && mGisEngine->layerType( mDEMLayerId ) == ReosGisEngine::RasterLayer;

}

bool ReosWatershedDelineating::setDigitalElevationModelDEM( const QString &layerId )
{
  if ( mGisEngine->layerType( layerId ) == ReosGisEngine::RasterLayer && mGisEngine->isDigitalElevationModel( layerId ) )
  {
    QString oldLayer = layerId;
    mDEMLayerId = layerId;
    return true;
  }

  return false;
}



ReosWatershedDelineating::State ReosWatershedDelineating::currentState() const
{
  return mCurrentState;
}

void ReosWatershedDelineating::setDownstreamLine( const QPolygonF &downstreamLine )
{
  if ( mCurrentState == WaitingForDownstream )
  {
    mDownstreamLine = downstreamLine;
    mCurrentState = WaitingForExtent;
  }
}
