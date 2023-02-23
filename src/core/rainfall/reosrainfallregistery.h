/***************************************************************************
  reosrainfallregistery.h - ReosRainfallRegistery

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
#ifndef REOSRAINFALLREGISTERY_H
#define REOSRAINFALLREGISTERY_H

#include <memory>
#include <QString>

#include "reoscore.h"
#include "reosmodule.h"

class ReosRainfallModel;
class ReosRainfallItem;

//! Singleton class that register and handle data related to rainfall
class REOSCORE_EXPORT ReosRainfallRegistery: public ReosModule
{
    Q_OBJECT
  public:

    static void instantiate( ReosModule *parentModule = nullptr );
    static ReosRainfallRegistery *instance();
    static bool isInstantiate();

    ReosRainfallModel *rainfallModel() const;
    ReosRainfallItem *itemByUri( const QString &uri ) const;
    ReosRainfallItem *itemByUniqueId( const QString &uid ) const;

  private:
    ReosRainfallRegistery( ReosModule *parentModule = nullptr );
    static ReosRainfallRegistery *sRainfallRegistery;
    ReosRainfallModel *mRainfallModel = nullptr;
};

#endif // REOSRAINFALLREGISTERY_H
