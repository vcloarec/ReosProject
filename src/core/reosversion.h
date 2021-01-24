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

#include <QString>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkReply>

#include "reoscore.h"

static const QString serverVersionAddress( "https://www.reos.site/availableVersion/" );

//! Class that store the version of Reos software
class REOSCORE_EXPORT ReosVersion
{
  public:
    //! Default constructor
    ReosVersion() = default;
    //! Constructor with software name and version number
    ReosVersion( const QString &name, int major, int minor, int sub );

    bool operator==( const ReosVersion &other );
    bool operator>( const ReosVersion &other );
    bool operator<( const ReosVersion &other );
    QString getSoftName() const;
    void setSoftName( const QString &value );
    QString softwareNameWithVersion() const;
    QString stringVersion() const;

  private:
    QString mSoftName;
    int mMajor = 0;
    int mMinor = 0;
    int mSub = 0;
};


#endif // REOSVERSION_H
