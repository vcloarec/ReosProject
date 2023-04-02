/***************************************************************************
  reoshubeauserver.h - ReosHubEauServer

 ---------------------
 begin                : 31.10.2021
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
#ifndef REOSHUBEAUSERVER_H
#define REOSHUBEAUSERVER_H

#include <QObject>
#include <QVariant>
#include <memory>

#include "reosmapextent.h"
#include "reosmodule.h"
#include "reostimeserieprovider.h"


class QNetworkAccessManager;
class QNetworkReply;
class ReosMapExtent;
class ReosMap;
class ReosHydrograph;

struct ReosHubEauStation
{
  QString id;
  QVariantMap meta;
  double longitude;
  double latitude;
};

/**
 * Class that represents a connection to a the hub-eau server through the web API
 */
class ReosHubEauConnection: public QObject
{
    Q_OBJECT
  public:
    ReosHubEauConnection( QObject *parent = nullptr );

    //! Launchs a request to the server with an \a operation string following hub-eau API specification
    void request( const QString &operation );

    //! Launchs a request to the server with a complete \a Url
    void requestByUrl( const QString &Url );

    //! Returns the error code of the last request
    int errorCode() const;

    //! Returns the error reason if any
    QString errorString() const;

    //! Returns the result of the last request
    QVariantMap result() const;

  signals:
    //! Emitted when the replied of the request is ready
    void repliedReady();

  private slots:
    void onReplied( QNetworkReply *reply );
    void launchRequest();

  private:
    //TODO : rework the logic t use ReosNetworkAccessManager instead
    QNetworkAccessManager *mNetworkAccessManager = nullptr;
    QNetworkReply *mWaitedReply = nullptr;
    QString mBaseUri;
    QString mRequest;
    QVariantMap mResult;
    int mErrorCode = -1;
    QString mErrorString;
    bool mRequestInProgress = false;
};

//! Class that represents a controller of the connection to the hub-eau server that is living on another thread.
class ReosHubEauConnectionControler: public QObject
{
    Q_OBJECT
  public:

    ReosHubEauConnectionControler( QObject *parent = nullptr );
    ~ReosHubEauConnectionControler();

    //! Makes a request to the controller with an \a operation string following hub-eau API specification
    void request( const QString &operation );

    //! Makes a request to the controller with an \a operation string following hub-eau API specification and wait for a reply
    void requestAndWait( const QString &stringRequest );

    //! Returns the last error following a requuest
    int lastError() const;

    //! Returns the last error following a requuest
    QString lastErrorReason() const;

  signals:
    //! Emitted when the result of the request is ready and sends it to the rceiver of the slot
    void resultReady( const QVariantMap &result );

    //! Emitted when the request is finished, that is all replies have been done.
    void requestFinished();

    //! Emitted when an error occured
    void errorOccured();

  public slots:
    //! Slot called by the connection to inform that the server replied
    void onReplied();

  private:
    ReosHubEauConnection *mConnection = nullptr;
    QThread *mThread = nullptr;
    int mError = -1;
    QString mNextURL;
};

class ReosHubEauServer : public QObject
{
    Q_OBJECT
  public:
    ReosHubEauServer( QObject *parent = nullptr );

    //! Tests connection to the hub-eau server
    bool testConnection();

    //! Returns the list of the available stations
    QList<ReosHubEauStation> stations() const;

    //! Create a new hydrograph from the \a stationId, caller take ownership
    ReosHydrograph *createHydrograph( const QString &stationId, const QVariantMap &meta, QObject *parent = nullptr ) const;

    //! Returns the last message produce by the server
    ReosModule::Message lastMessage() const;

  signals:
    //! Emitted when stations are updated following a reply of the server
    void stationsUpdated();

    //! Emitted when an error occured with the server
    void errorOccured();

  public slots:
    //! Sets the extent of the area of interest to require stations
    void setExtent( const ReosMapExtent &extent );

  private slots:
    void addStations( const QVariantMap &requestResult );
    void onErrorOccured();

  private:
    ReosMap *mMap = nullptr;
    ReosHubEauConnectionControler *mStationsRequestControler = nullptr;
    QList<ReosHubEauStation> mStations;
    ReosMapExtent mExtent;
    ReosModule::Message mLastMessage;
};

#endif // REOSHUBEAUSERVER_H
