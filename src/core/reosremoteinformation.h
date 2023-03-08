/***************************************************************************
  reosremoteinformation.h - ReosRemoteInformation

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
#ifndef REOSREMOTEINFORMATION_H
#define REOSREMOTEINFORMATION_H

#define SIP_NO_FILE

#include <QObject>
#include <QVariantMap>

#include "reoscore.h"

class QNetworkAccessManager;
class QNetworkReply;

static const QString serverInformationAddress( "https://www.reos.site/" );

class REOSCORE_EXPORT ReosRemoteInformation : public QObject
{
    Q_OBJECT
  public:
    ReosRemoteInformation( QObject *parent = nullptr );

    void requestInformation();

  signals:
    void informationready( const QVariantMap &information );

  private slots:
    void onReply( QNetworkReply *reply );

  private:
    QVariantMap mInformation;
    QNetworkAccessManager *mNetWorkAccess = nullptr;
};

#endif // REOSREMOTEINFORMATION_H
