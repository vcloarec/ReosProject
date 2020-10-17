/***************************************************************************
                      reosrastercompressed.h
                     --------------------------------------
Date                 : 16-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSRASTERCOMPRESSED_H
#define REOSRASTERCOMPRESSED_H

#include <QByteArray>
#include "reosmemoryraster.h"


/**
 * Class used to store unsigned char raser that has value unde 128 by a compress way
 *
 */
class ReosRasterByteCompressed
{
  public:
    ReosRasterByteCompressed() = default;
    //! Constructor with an existing \a raster
    ReosRasterByteCompressed( const ReosRasterMemory<unsigned char> &raster );

    //! Decompresses the raster and returns it
    ReosRasterMemory<unsigned char> uncompressRaster() const;

  private:
    int mRowCount = 0;
    int mColumnCount = 0;
    QByteArray mData;
};


#endif // REOSRASTERCOMPRESSED_H
