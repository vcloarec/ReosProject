/***************************************************************************
  reosrenderersettings_p.h - ReosRendererSettings_p

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
#ifndef REOSRENDERERSETTINGS_P_H
#define REOSRENDERERSETTINGS_P_H

#include <qgsmapsettings.h>

#include "reosrenderedobject.h"

class ReosRendererSettings_p : public ReosRendererSettings
{
  public:
    ReosRendererSettings_p( const void *settings );

    const QgsMapSettings &settings() const;

  private:
    QgsMapSettings mSettings;
};

#endif // REOSRENDERERSETTINGS_P_H
