/***************************************************************************
  reoscdslccprovider.h - ReosCdslccProvider

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

#ifndef REOSCDSLCCPROVIDER_H
#define REOSCDSLCCPROVIDER_H

#include "reoslandusedata.h"
#include "reosdataprovider.h"


#endif // REOSCDSLCCPROVIDER_H
 class ReosCdslccProvider : public ReosLandUseDataProvider
{
  public:
    ReosCdslccProvider();
    ~ReosCdslccProvider() {}

    const QVector<int> data( int index ) const override {return QVector<int>();}
    ReosRasterExtent extent() const override {return ReosRasterExtent();}

    static QString dataType() { return ReosLandUseData::staticType(); }

    virtual void load() override {}
    virtual QStringList fileSuffixes() const override {};
    QString key() const { return QStringLiteral("cdslcc"); }
};


class ReosCdslccProviderFactory : public ReosDataProviderFactory
{
  public:
    ReosCdslccProvider *createProvider(const QString &dataType) const override;
    QString key() const override{return QStringLiteral("cdslcc");}
    bool supportType( const QString &dataType ) const override { return dataType.contains( ReosCdslccProvider::dataType() ); }
    QVariantMap uriParameters(const QString &dataType) const override;
    QString buildUri(const QString &dataType, const QVariantMap &parameters,bool &ok) const override;
};