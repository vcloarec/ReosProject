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

#ifndef REOSVERSION_H
#define REOSVERSION_H

#define SIP_NO_FILE

#include <QString>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkReply>
#include <QDataStream>

#include "reoscore.h"

//! Class that store the version of Reos software
class REOSCORE_EXPORT ReosVersion
{
  public:
    //! Default constructor
    ReosVersion() = default;
    //! Constructor with software name and version number
    ReosVersion( const QString &name, int major, int minor, int sub );

    ReosVersion( const QByteArray &bytes, QDataStream::Version v );

    bool operator==( const ReosVersion &other );
    bool operator>( const ReosVersion &other );
    bool operator<( const ReosVersion &other );
    QString getSoftName() const;
    void setSoftName( const QString &value );
    QString softwareNameWithVersion() const;
    QString stringVersion() const;

    QByteArray bytesVersion() const;

    static ReosVersion currentApplicationVersion();

    static void setCurrentApplicationVersion( const ReosVersion &value );

  private:
    QString mSoftName;
    int mMajor = 0;
    int mMinor = 0;
    int mSub = 0;

    static ReosVersion sCurrentApplicationVersion;
};


#endif // REOSVERSION_H
