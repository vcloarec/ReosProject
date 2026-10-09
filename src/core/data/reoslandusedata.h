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
#ifndef REOSLANDUSEDATA_H
#define REOSLANDUSEDATA_H

#include "reosdataprovider.h"
#include "reosmemoryraster.h"
#include "reosdataobject.h"

class ReosWatershed;

class REOSCORE_EXPORT ReosLandUseDataProvider : public ReosDataProvider
{
    Q_OBJECT
  public:
    explicit ReosLandUseDataProvider();
    ~ReosLandUseDataProvider() {}

    QString dataSource() const;
    void setDataSource( const QString &uri );

    virtual const QVector<int> data() const = 0;
    virtual ReosRasterExtent extent() const = 0;
    virtual const QVector<int> data( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const = 0;

  private:
    QString mDataSource;
};


class REOSCORE_EXPORT ReosLandUseData : public ReosDataObject
{
    Q_OBJECT
  public:
    explicit ReosLandUseData( const QString &dataSource, const QString &providerKey, QObject *parent = nullptr );
    static QString staticType();

    QVector<int> data() const;

    QVector<int> data( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const;
    ReosRasterMemory<int> rasterData( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const;

    ReosRasterExtent extent() const;

  private:
    std::unique_ptr<ReosLandUseDataProvider> mProvider;

    QString formatKey( const QString &rawKey ) const;
};

class REOSCORE_EXPORT ReosLandUseDataOnWatershed
{
  public:
    ReosLandUseDataOnWatershed( ReosLandUseData *landUseData, ReosWatershed *watershed );
    QMap<int, double> landUseDistribution() const;

  private:
    ReosLandUseData *mLandUseData;
    ReosWatershed *mWatershed = nullptr;
};


#endif // REOSLANDUSEDATA_H