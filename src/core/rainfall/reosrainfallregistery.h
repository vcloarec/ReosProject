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

class ReosRainfallModel;
class ReosRainfallItem;

class ReosRainfallRegistery
{
  public:
    ReosRainfallRegistery();
    ~ReosRainfallRegistery();
    static ReosRainfallRegistery *instance();
    static bool isInstantiate();
    ReosRainfallModel *rainfallModel() const;

    ReosRainfallItem *item( const QString &uri ) const;



  private:
    static ReosRainfallRegistery *sRainfallRegistery;
    std::unique_ptr<ReosRainfallModel> mRainfallModel;
};

#endif // REOSRAINFALLREGISTERY_H
