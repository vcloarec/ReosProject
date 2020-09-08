/***************************************************************************
                      reosdocumentation.h
                     --------------------------------------
Date                 : 07-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSDOCUMENTATION_H
#define REOSDOCUMENTATION_H

#include<QtNetwork/QNetworkAccessManager>
#include <QDesktopServices>
#include <QUrl>

#include "reosversion.h"


static const QString serverDocumentationAddress( "https://www.reos.site/availableDocumentation/" );

class ReosDocumentation: public QObject
{
    Q_OBJECT
  public:
    ReosDocumentation( const ReosVersion &version, QObject *parent );

  public slots:
    void call();

  private:
    ReosVersion mVersion;
    QNetworkAccessManager *mNetWorkAccess = nullptr;
  private slots:
    void launchWebSite( QNetworkReply *reply );
};

#endif // REOSDOCUMENTATION_H
