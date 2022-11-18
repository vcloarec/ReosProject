/***************************************************************************
  reosrenderedobject.cpp - ReosRenderedObject

 ---------------------
 begin                : 6.3.2022
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
#include "reosrenderedobject.h"
#include "reosrenderersettings_p.h"

ReosObjectRenderer::ReosObjectRenderer( ReosRenderedObject *object )
  : mObject( object )
{
}

void ReosObjectRenderer::start()
{
  render();
}

void ReosObjectRenderer::stop( bool stop )
{
  if ( stop )
    stopRendering();
}

const QImage ReosObjectRenderer::image() const
{
  return mImage;
}

QRectF ReosObjectRenderer::extent() const
{
  return mExtent;
}

void ReosObjectRenderer::setExtent( const QRectF &extent )
{
  mExtent = extent;
}

ReosRendererObjectMapTimeStamp *ReosObjectRenderer::releaseMapTimeStamp()
{
    return mMapTimeStamp.release();
}

ReosRendererObjectMapTimeStamp *ReosObjectRenderer::mapTimeStamp() const
{
    return mMapTimeStamp.get();
}

void ReosObjectRenderer::setMapTimeStamp(ReosRendererObjectMapTimeStamp *timeStamp)
{
    mMapTimeStamp.reset( timeStamp );
}


ReosRenderedObject *ReosObjectRenderer::object()
{
    return mObject;
}

ReosRenderedObject::ReosRenderedObject( QObject *parent ) : ReosDataObject( parent ) {}

std::unique_ptr<ReosRendererSettings> ReosRenderedObject::createRenderSettings( const void *settings )
{
  return std::unique_ptr<ReosRendererSettings>( new ReosRendererSettings_p( settings ) );
}

