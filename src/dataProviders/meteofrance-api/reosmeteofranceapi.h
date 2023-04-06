/***************************************************************************
  reosmeteofranceapi.h - ReosMeteoFranceApi

 ---------------------
 begin                : 28.3.2023
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
#ifndef REOSMETEOFRANCEAPI_H
#define REOSMETEOFRANCEAPI_H

#include <memory>

#include <QString>
#include <QObject>
#include <QDateTime>
#include <QMap>

#include "reosmapextent.h"

class QNetworkAccessManager;
class QNetworkReply;
class ReosGriddedRainfall;

class ReosRemoteForecastApiService
{
  public:
    //! Returns the name of the service
    virtual QString name() const = 0;

    /**
     * Returns a rainfall forecast
     * \param name the name of the rainfall forecast
     * \param extent the extent of the returned rainfall
     * \param index a interger that represent the returned run,  0 corresponding to the last run available.
     */
    //virtual ReosGriddedRainfall *rainfallForecast( const QString &name, const ReosMapExtent &extent, int index = 0 ) const = 0;

};

class ReosMeteoFranceApiArome : public QObject
{
    Q_OBJECT
  public:

    struct Model
    {
      QString zone;
      QString resol;
    };

    struct RunInfo
    {
      ReosMapExtent extent;
      QDateTime startTime;
      QDateTime endTime;
      int frameCount = 0;
    };

    //! Constructors
    explicit ReosMeteoFranceApiArome( const QString &keyFileName );

    //! Returns whether the service has a key
    bool hasKey() const;

    //! Connects to the service with the \a model and returns \a error, wait for reply
    bool connectToServiceBlocking( const Model &model, QString &error );

    //! Connects to the service with the \a model and returns \a error
    void connectToService( const Model &model, QString &error );

    //! Returns the available runs, must be connected \see connectToService()
    QList<QDateTime> availableRuns() const;

    //! Returns information about the \a run, must be connected \see connectToService()
    RunInfo runInfoBlocking( const QDateTime &run ) const;

    //! Lanches a request to obtain information about the \a run. Once receive the signal runInfoReady is connected with data as paramters
    void requestRunInfo( const QDateTime &run, QString &error );

    /**
     * Requests a rainfall frame with \a extent fot the \a run and with index \a frameIndexReturns, must be connected \see connectToService()
     * The request is blocked until response is finished
     */
    QByteArray requestFrameBlocking( const ReosMapExtent &extent, const QDateTime &run, int frameIndex );

    //! Launch a request for a rainfall frame with \a extent fot the \a run and with index \a frameIndexReturns, must be connected \see connectToService()
    void requestFrame( const ReosMapExtent &extent, const QDateTime &run, int frameIndex );

    /**
     * Launch a defferred request for a rainfall frame with \a extent fot the \a run and with index \a frameIndexReturns,
     * with deffered \a delay
     * must be connected \see connectToService()
     *
     */
    void requestFrameDeffered( const ReosMapExtent &extent, const QDateTime &run, int frameIndex, int delay );


    static QStringList extentToLonLatList( const ReosMapExtent &extent, int precision = 16 );
    static QList<Model> availableModels();
    static RunInfo decodeRunInfo( const QByteArray &bytes, QString &error );

  signals:
    void connected( const QString &error );
    void runInfoReady( const QByteArray &bytes, const QDateTime &run );
    void valuesReady( const QByteArray &bytes, int frameIndex );

  private:
    const QString mServerUrl = QStringLiteral( "https://public-api.meteofrance.fr/public/arome/1.0" );
    QString mKeyFileName;
    QString mKeyApi;
    static QList<Model> sAvailableModels;
    int mTimeOutDelay = 60000;
    Model mModel;
    QString mLastError;

    enum Status
    {
      Unconnected,
      Connecting,
      Connected,
    };

    Status mStatus = Unconnected;

    struct ServiceVersion
    {
      QString service;
      QString version;
      QString language;
    };

    ServiceVersion mVersion = {QStringLiteral( "WCS" ), QStringLiteral( "2.0.1" ), QStringLiteral( "eng" )};

    QMap<QDateTime, QString> mCoverageIds;

    QString capabilitiesUrl( const ReosMeteoFranceApiArome::Model &model ) const;
    QString baseUrl( const Model &model ) const;
    QString capabilitiesRequest() const;
    QString describeCoverageRequest( const QDateTime &run ) const;
    QString coverageRequest( const QDateTime &run, int frameIndex, const ReosMapExtent &extent ) const;
    QByteArray networkRequestBlocking( const QString &stringRequest, QString &error ) const;
    QNetworkReply *networkRequest( const QString &stringRequest, QString &error ) const;
    static QByteArray preventThrottling( QNetworkReply *reply, bool &throttled );

    bool onConnectionReply( const QByteArray &bytes, QString &error );

    bool setKey( const QString &fileName );
};

class ReosMeteoFranceApi : public QObject, public ReosRemoteForecastApiService
{
    Q_OBJECT
  public:
    explicit ReosMeteoFranceApi( const QString &keyFileName );

    QString name() const override;
    ReosMeteoFranceApiArome *aromeService() const;

  private:
    bool mIsValid = false;
    std::unique_ptr<ReosMeteoFranceApiArome> mAromeService;
};

#endif // REOSMETEOFRANCEAPI_H
