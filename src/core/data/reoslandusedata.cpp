/***************************************************************************
  reoslandusedata.h - ReosLandUseData

 ---------------------
 begin                : 9.2.2021
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


#include "reoslandusedata.h"


ReosLandUseDataProvider::ReosLandUseDataProvider( )
  : ReosDataProvider()
{}

ReosLandUseData::ReosLandUseData(const QString &dataSource,
                                 const QString &providerKey, QObject *parent)
: ReosDataObject(parent)
,mProvider(std::unique_ptr<ReosLandUseDataProvider>(qobject_cast<ReosLandUseDataProvider *>(ReosDataProviderRegistery::instance()->createProvider(formatKey(providerKey))))) 
{}

QString ReosLandUseData::formatKey(const QString &rawKey) const 
{
  if (rawKey.contains(QStringLiteral("::")))
    return rawKey;
  return rawKey + QStringLiteral("::") + ReosLandUseData::staticType();
}

QString ReosLandUseData::staticType() {
  return QStringLiteral("land-use-data");
}
