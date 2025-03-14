/***************************************************************************
  reosmeteohdf5.h - ReosMeteoHdf5Provider

 ---------------------
 begin                : 14.03.2025
 copyright            : (C) 2025 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMETEOHDF5PROVIDER_H
#define REOSMETEOHDF5PROVIDER_H

#include <QDateTime>
#include <QCache>

#include "reosmodule.h"
#include "reosgriddedrainfallprovider.h"
#include "reosmemoryraster.h"

#define METEO_HDF5_KEY QStringLiteral("meteo-hdf5")

class ReosMeteoHdf5Provider : public ReosGriddedDataProvider
{
    Q_OBJECT
  public:
    ReosMeteoHdf5Provider();

    ReosMeteoHdf5Provider *clone() const override;
    void load() override;
    QStringList fileSuffixes() const override;
    QString key() const override {return staticKey();}
    FileDetails details( const QString &, ReosModule::Message & ) const override;
    bool isValid() const override;
    int count() const override;
    bool canReadUri( const QString &uri ) const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    ReosRasterExtent extent() const override;
    const QVector<double> data( int index ) const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    void calculateMinMax( double &min, double &max ) const override;
    QString htmlDescription() const override;

    static QString dataType();

    //! Returns the key of this provider
    static QString staticKey();

    static QString uri( const QString &sourcePath );
    static QString sourcePathFromUri( const QString &uri );

    bool sourceIsValid( const QString &source, ReosModule::Message &message ) const;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;

  private:
    struct Frame
    {
      QString gdalUri;
      QDateTime startTime;
      QDateTime endTime;
    };
    QList<Frame> mFrames;
    ReosRasterExtent mExtent;
    qint64 mReferenceTime = -1;

    bool mIsValid = false;
    ReosModule::Message mLastMessage;

    mutable bool mHasMinMaxCalculated = false;
    mutable double mMin = std::numeric_limits<double>::max();
    mutable double mMax = -std::numeric_limits<double>::max();

    mutable QCache<int, QVector<double>> mCache;

    QStringList getFiles( const QString &path ) const;

    static void giveName( FileDetails &details );
};

class ReosMeteoHdf5ProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosMeteoHdf5Provider *createProvider( const QString &dataType ) const override;
    QString key() const override;
    bool supportType( const QString &dataType ) const override;
    QVariantMap uriParameters( const QString &dataType ) const override;
    QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const override;
};

#endif // REOSMETEOHDF5PROVIDER_H
