/***************************************************************************
  reoscdslccprovider.cpp - ReosCdslccProvider

 ---------------------
 begin                : 5.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoscdslccprovider.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosCdslccProviderFactory();
}

ReosCdslccProvider::ReosCdslccProvider()
: ReosLandUseDataProvider()
{
}

ReosCdslccProvider *ReosCdslccProviderFactory::createProvider(const QString &dataType) const {
  if (ReosCdslccProvider::dataType() == dataType)
    return new ReosCdslccProvider;

  return nullptr;
}
QVariantMap
ReosCdslccProviderFactory::uriParameters(const QString &dataType) const 
{
  QVariantMap ret;

  if (supportType(dataType)) 
    ret.insert(QStringLiteral("file-path"),
               QObject::tr("File where are stored the data"));
  

  return ret;
}
QString ReosCdslccProviderFactory::buildUri(const QString &dataType,
                                            const QVariantMap &parameters,
                                            bool &ok) const {
  if (supportType(dataType) && parameters.contains(QStringLiteral("file-path"))) 
  {
    QString uri = parameters.value(QStringLiteral("file-path")).toString();
    ok = true;
    return uri;
  } 
  else 
  {
    ok = false;
    return QString();
  }
}
