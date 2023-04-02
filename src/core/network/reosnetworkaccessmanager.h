/***************************************************************************
  reosnetworkaccessmanager.h - ReosNetworkAccessManager

 ---------------------
 begin                : 29.3.2023
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
#ifndef REOSNETWORKACCESSMANAGER_H
#define REOSNETWORKACCESSMANAGER_H

#include <QNetworkAccessManager>
#include <QThreadStorage>

#include "reoscore.h"

#define SIP_NO_FILE

class REOSCORE_EXPORT ReosNetworkAccessManager : public QNetworkAccessManager
{
  public:
    ReosNetworkAccessManager( QObject *parent = nullptr );

    /**
     * Returns an instance of this class.
     *
     * \note there is one instance per thread
     */
    static ReosNetworkAccessManager *instance();

    //! Makes a GET request and wait for reply, the thread is blocked until the reply is finished
    QNetworkReply *getBlocking( const QNetworkRequest &request,  QString &error, int timeOut = 0 );

  private:
    static QThreadStorage<ReosNetworkAccessManager> sInstances;
    static ReosNetworkAccessManager *sMainNam;
};

#endif // REOSNETWORKACCESSMANAGER_H
