/***************************************************************************
  reosgdalutils.h - ReosGdalUtils

 ---------------------
 begin                : 11.11.2022
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
#ifndef REOSGDALUTILS_H
#define REOSGDALUTILS_H

#define SIP_NO_FILE

#include <QString>
#include <gdal.h>

#include "reosmemoryraster.h"

class REOSCORE_EXPORT ReosGriddedDataSource
{
  public:
    ReosGriddedDataSource() = default;
    virtual ~ReosGriddedDataSource();
    virtual bool isValid() const = 0;
    virtual int frameCount() const = 0;
    virtual ReosRasterExtent extent( int frameIndex = 0 ) const = 0;
    virtual ReosRasterMemory<double> values( int frameIndex ) const = 0;

};

class REOSCORE_EXPORT ReosGdalDataset : public ReosGriddedDataSource
{
  public:
    ReosGdalDataset( const QString &fileName, bool readOnly = true );
    ~ReosGdalDataset();

    bool isValid() const override ;
    int frameCount() const override;
    ReosRasterExtent extent( int frameIndex = 0 ) const override;
    ReosRasterMemory<double> values( int frameIndex ) const override;

    int bandCount() const;

    QMap<QString, QString> metadata() const;
    QMap<QString, QString> bandMetadata( int band ) const;

    ReosRasterMemory<double> valuesFromBand( int band ) const;
    ReosRasterMemory<int> valuesInt( int band ) const;
    ReosRasterMemory<unsigned char> valuesBytes( int band ) const;

    static bool writeByteRasterToFile( const QString &fileName, ReosRasterMemory<unsigned char> raster, const ReosRasterExtent &extent );
    static bool writeIntRasterToFile( const QString &fileName, ReosRasterMemory<int> raster, const ReosRasterExtent &extent );
    static bool writeDoubleRasterToFile( const QString &fileName, ReosRasterMemory<double> raster, const ReosRasterExtent &extent );

  private:
    GDALDatasetH mHDataset = nullptr;

    static bool sRegistered;
};






#endif // REOSGDALUTILS_H
