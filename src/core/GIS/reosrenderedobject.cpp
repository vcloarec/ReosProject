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

ReosRenderedObject::ReosRenderedObject( QObject *parent ) : ReosDataObject( parent ) {}
