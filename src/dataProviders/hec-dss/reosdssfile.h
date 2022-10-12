/***************************************************************************
  reosdssfile.h - ReosDssFile

 ---------------------
 begin                : 11.10.2022
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
#ifndef REOSDSSFILE_H
#define REOSDSSFILE_H

#include <memory>
#include <array>
#include <QString>
#include <QDateTime>

#include <reosduration.h>

class ReosHydrograph;

class ReosDssFile
{
  public:

    ReosDssFile( const QString &filePath, bool create = false );
    ~ReosDssFile();

    ReosDssFile &operator=( ReosDssFile &other ) = delete;
    ReosDssFile &operator=( ReosDssFile &&other );

    bool isValid() const;
    bool isOpen() const;

    bool addHydrograph( ReosHydrograph *hydrograph, const QDateTime &startTime = QDateTime(), const ReosDuration &interval = ReosDuration() );

    static ReosDuration closestValidInterval( const ReosDuration &interval );

  private:
    std::unique_ptr<std::array<long long, 250>> mIfltab;
    int mStatus = -1;
    bool mIsValid = false;
    bool mIsOpen = false;

};

#endif // REOSDSSFILE_H
