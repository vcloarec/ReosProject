/***************************************************************************
  reosgdalutils.h - ReosGdalUtils

 ---------------------
 begin                : 11.11.2022
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
#ifndef REOSGDALUTILS_H
#define REOSGDALUTILS_H

#include <QString>
#include <gdal.h>

#include "reosmemoryraster.h"

class ReosGdalDataset
{
  public:
    ReosGdalDataset( const QString &fileName, bool readOnly = true );
    ~ReosGdalDataset();

    int bandCount();

    QMap<QString, QString> metadata() const;
    QMap<QString, QString> bandMetadata( int band ) const;

    ReosRasterExtent extent() const;

    bool isValid() const;

    ReosRasterMemory<double> values( int band );

  private:
    GDALDatasetH mHDataset = nullptr;
};






#endif // REOSGDALUTILS_H
