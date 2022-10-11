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

#include <array>

class ReosDssFile
{
  public:
    ReosDssFile();

  private:
    std::array<long long, 250> mIfltab;
    int mStatus = 0;
};

#endif // REOSDSSFILE_H
