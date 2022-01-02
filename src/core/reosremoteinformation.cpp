/***************************************************************************
  reosremoteinformation.cpp - ReosRemoteInformation

 ---------------------
 begin                : 1.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosremoteinformation.h"

#include <QJsonDocument>
#include <QCoreApplication>
#include <QNetworkRequest>
#include <qnetworkreply.h>
#include <QNetworkAccessManager>

ReosRemoteInformation::ReosRemoteInformation( QObject *parent )
  : QObject( parent )
{
  mNetWorkAccess = new QNetworkAccessManager( this );
  connect( mNetWorkAccess, &QNetworkAccessManager::finished, this, &ReosRemoteInformation::onReply );
}

void ReosRemoteInformation::requestInformation()
{
  QNetworkRequest request( serverInformationAddress + QCoreApplication::applicationName() + QStringLiteral( "_info/info.txt" ) );
  mNetWorkAccess->get( request );
}

void ReosRemoteInformation::onReply( QNetworkReply *reply )
{
  QTextStream textStream( reply );
  QJsonDocument mJsonResult = QJsonDocument::fromJson( textStream.readAll().toUtf8() );
  QVariant var = mJsonResult.toVariant();
  if ( var.type() == QVariant::Map )
  {
    emit informationready( var.toMap() );
    return;
  }
}
