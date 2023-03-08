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

#define SIP_NO_FILE

#include <QByteArray>
#include "reosmemoryraster.h"


/**
 * Class used to store unsigned char raser that has value unde 128 by a compress way
 *
 */
class REOSCORE_EXPORT ReosRasterByteCompressed
{
  public:
    ReosRasterByteCompressed() = default;
    //! Constructor with an existing \a raster
    ReosRasterByteCompressed( const ReosRasterMemory<unsigned char> &raster );

    //! Decompresses the raster and returns it
    ReosRasterMemory<unsigned char> uncompressRaster() const;

    //! Return whether the instance of this object contains data
    bool hasData() const;

    //! Encodes the instance and returns a encodeded element
    ReosEncodedElement encode() const;
    //! Creates a new instance of this class by decoding the \a element
    static ReosRasterByteCompressed decode( const ReosEncodedElement &element );

    bool operator==( const ReosRasterByteCompressed &other ) const;
    bool operator!=( const ReosRasterByteCompressed &other ) const;

  private:
    int mRowCount = 0;
    int mColumnCount = 0;
    QByteArray mData;
};


#endif // REOSRASTERCOMPRESSED_H
