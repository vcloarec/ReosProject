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


class QNetworkAccessManager;
class QNetworkReply;
class ReosMapExtent;
class ReosMap;

struct ReosHubEauStation
{
  QString id;
  QString name;
  double longitude;
  double latitude;

};

class ReosHubEauConnection: public QObject
{
    Q_OBJECT
  public:
    ReosHubEauConnection( QObject *parent = nullptr );

    void request( const QString &string );
    void requestByUrl( const QString &Url );
    int errorCode() const;
    QVariantMap result() const;

  signals:
    void repliedReady();

  private slots:
    void onReplied( QNetworkReply *reply );
    void launchRequest();

  private:
    QNetworkAccessManager *mNetworkAccessManager = nullptr;
    QNetworkReply *mWaitedReply = nullptr;
    QString mBaseUri;
    QString mRequest;
    QVariantMap mResult;
    int mErrorCode = -1;
    bool mRequestInProgress = false;
};

class ReosHubEauConnectionControler: public QObject
{
    Q_OBJECT
  public:

    ReosHubEauConnectionControler( QObject *parent = nullptr );
    ~ReosHubEauConnectionControler();

    void request( const QString &stringRequest );
    void requestAndWait( const QString &stringRequest );

    int lastError() const;

  signals:
    void resultReady( const QVariantMap &result );
    void requestFinished();

  public slots:
    void onReplied();

  private:
    ReosHubEauConnection *mConnection = nullptr;
    QThread *mThread = nullptr;
    int mError = -1;
    QString mNextURL;
};


class ReosHubEauAccess : public QObject
{
    Q_OBJECT
  public:
    ReosHubEauAccess( QObject *parent = nullptr );
    bool testConnection();

    QList<ReosHubEauStation> stations() const;

  signals:
    void stationsUpdated();

  public slots:
    void setExtent( const ReosMapExtent &extent );

  private slots:
    void addStations( const QVariantMap &requestResult );

  private:
    ReosMap *mMap = nullptr;
    ReosHubEauConnectionControler *mStationsRequestControler = nullptr;
    QList<ReosHubEauStation> mStations;
    ReosMapExtent mExtent;
};

#endif // REOSHUBEAUSERVER_H
