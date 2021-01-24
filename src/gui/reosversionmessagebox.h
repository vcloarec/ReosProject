/***************************************************************************
                      reosversion.h
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

#ifndef REOSVERSIONMESSAGEBOX_H
#define REOSVERSIONMESSAGEBOX_H

#include <QString>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkReply>
#include <QMessageBox>
#include <QWidget>

#include "reosversion.h"
#include "reosgui.h"

//! Widget that give information to the user of avaibaility of new version of the application
class REOSGUI_EXPORT ReosVersionMessageBox: public QMessageBox
{
  public:
    ReosVersionMessageBox( QWidget *parent, const ReosVersion &mVersion, bool mStart = true );

    QString getDefaultWebSite() const;
    void setDefaultWebSite( const QString &value );

  public slots:
    void receiveNetWorkRequest( QNetworkReply *reply );

  private:
    ReosVersion mVersion;
    QNetworkAccessManager *mNetWorkAccess = nullptr;
    bool mStart;
    QString mDefaultWebSite = serverVersionAddress;
};

#endif // REOSVERSIONMESSAGEBOX_H
