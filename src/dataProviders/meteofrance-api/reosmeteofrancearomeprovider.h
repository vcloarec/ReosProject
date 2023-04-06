/***************************************************************************
  reosmeteofrancearomeprovider.h - ReosMeteoFranceAromeProvider

 ---------------------
 begin                : 31.3.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMETEOFRANCEAROMEPROVIDER_H
#define REOSMETEOFRANCEAROMEPROVIDER_H

#include <QSet>

#include "reosgriddedrainfallprovider.h"
#include "reosmeteofranceapi.h"

#define AROME_KEY QStringLiteral("meteofrance-api-arome")


class ReosMeteoFranceAromeApiProvider : public ReosGriddedRainfallProvider
{
    Q_OBJECT

  public:
    ReosMeteoFranceAromeApiProvider();

    ReosGriddedRainfallProvider *clone() const override;
    QStringList fileSuffixes() const override {return QStringList();}
    QString key() const override {return staticKey();}
    void setDataSource( const QString &dataSource ) override;

    FileDetails details( const QString &, ReosModule::Message & ) const override;
    bool isValid() const override;
    int count() const override;
    bool canReadUri( const QString &path ) const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    ReosRasterExtent extent() const override;
    const QVector<double> data( int index ) const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    void calculateMinMax( double &min, double &max ) const override;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;

    static QString dataType();
    static QString staticKey();
    static QString uri( const QString &apiKeyFileName, const QString &zone, const QString &resol, const ReosMapExtent &extent, int runIndex );
    static QString apiKeyFileNamefromUri( const QString &uri );
    static QString zoneFromUri( const QString &uri );
    static QString resolFromUri( const QString &uri );
    static ReosMapExtent extentFromUri( const QString &uri );
    static QList<double> extentListFromUri( const QString &uri );
    static int runIndexfromUri( const QString &uri );

  private slots:
    void onConnected( const QString &connectError );
    void receiveRunInfo( const QByteArray &data, const QDateTime &run );
    void receiveData( const QByteArray &data, int frameIndex );

  private:
    std::unique_ptr<ReosMeteoFranceApiArome> mService;
    bool mIsValid = false;
    ReosMeteoFranceApiArome::Model mModel;
    QDateTime mRun;
    ReosMapExtent mRequestedExtent;
    ReosRasterExtent mExtent;

    QList<QVector<double>> mValues;
    ReosMeteoFranceApiArome::RunInfo mRunInfo;
    QSet<int> mReceivedFrames;

    void loadFrame();
};

class ReosMeteoFranceAromeApiProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosGriddedRainfallProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;

    bool hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const override;

  private:
    ReosDataProvider::Capabilities mCapabilities = {ReosDataProvider::Net | ReosDataProvider::Spatial};
};


#endif // REOSMETEOFRANCEAROMEPROVIDER_H
