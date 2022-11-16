/***************************************************************************
  reosrenderersettings_p.cpp - ReosRendererSettings_p

 ---------------------
 begin                : 16.11.2022
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
#include "reosrenderersettings_p.h"

ReosRendererSettings_p::ReosRendererSettings_p( const void *settings )
{
  const QgsMapSettings *mapSettings = static_cast<const QgsMapSettings *>( settings );
  mSettings = *mapSettings;
}

const QgsMapSettings &ReosRendererSettings_p::settings() const
{
    return mSettings;
}
