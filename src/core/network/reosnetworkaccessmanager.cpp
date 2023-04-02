/***************************************************************************
  reosnetworkaccessmanager.cpp - ReosNetworkAccessManager

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
#include "reosnetworkaccessmanager.h"

#include <QThread>
#include <QApplication>
#include <QNetworkReply>
#include <QTimer>

QThreadStorage<ReosNetworkAccessManager> ReosNetworkAccessManager::sInstances;
ReosNetworkAccessManager *ReosNetworkAccessManager::sMainNam = nullptr;

ReosNetworkAccessManager::ReosNetworkAccessManager( QObject *parent )
  : QNetworkAccessManager( parent )
{}

ReosNetworkAccessManager *ReosNetworkAccessManager::instance()
{
  ReosNetworkAccessManager *nam = &sInstances.localData();

  if ( nam->thread() == qApp->thread() )
    sMainNam = nam;;

  return nam;
}

QNetworkReply *ReosNetworkAccessManager::getBlocking( const QNetworkRequest &request, QString &error, int timeOut )
{
  QNetworkReply *reply = get( request );
  QTimer timer;
  timer.setSingleShot( true );
  QEventLoop loop;
  bool isTimeOut = false;


  connect( reply, &QNetworkReply::finished, &loop, [&]
  {
    timer.stop();
    loop.quit();
  } );

  if ( timeOut > 0 )
  {
    connect( &timer, &QTimer::timeout, &loop, [ & ]
    {
      isTimeOut = true;
      loop.quit();
    } );
    timer.start( timeOut );
  }

  loop.exec();

  if ( isTimeOut )
  {
    error = tr( "Network request time out." );
    return nullptr;
  }

  return reply;
}
