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

class ReosNetCdfFile;

#define CDSLCC_KEY QStringLiteral( "cdslcc" )


#endif // REOSCDSLCCPROVIDER_H
class ReosCdslccProvider : public ReosLandUseDataProvider
{
  public:
    ReosCdslccProvider();
    ~ReosCdslccProvider() {}

    const QVector<int> data() const override;
    const QVector<int> data( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const override;
    ReosRasterExtent extent() const override;
    bool canReadUri( const QString &uri ) const override;

    static QString dataType() { return ReosLandUseData::staticType(); }

    virtual void load() override;
    virtual QStringList fileSuffixes() const override { return QStringList( { QStringLiteral( "nc" ) } ); };
    QString key() const override { return CDSLCC_KEY; }

  private:
    std::unique_ptr<ReosNetCdfFile> mFile;
    ReosRasterExtent mExtent;
};


class ReosCdslccProviderFactory : public ReosDataProviderFactory
{
  public:
    ReosCdslccProvider *createProvider( const QString &dataType ) const override;
    QString key() const override { return QStringLiteral( "cdslcc" ); }
    bool supportType( const QString &dataType ) const override { return dataType.contains( ReosCdslccProvider::dataType() ); }
    QVariantMap uriParameters( const QString &dataType ) const override;
    QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const override;
};