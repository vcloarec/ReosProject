/***************************************************************************
  reosgriddedrainitem.h - ReosGriddedRainItem

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
#ifndef REOSGRIDDEDRAINITEM_H
#define REOSGRIDDEDRAINITEM_H

#include "reosrainfallitem.h"

class ReosRasterExtent;
class ReosGriddedRainfallProvider;

class ReosGriddedRainfall : public ReosDataObject
{
  public:
    ReosGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent = nullptr );
    ~ReosGriddedRainfall();

    QString type() const override;

    static QString staticType();

    //! Returns the count of grids (e.g. time steps)
    int gridCount() const;

    //! Returns the start time related to the grif with \a index
    const QDateTime startTime( int index ) const;

    //! Returns the end time related to the grif with \a index
    const QDateTime endTime( int index ) const;

    /**
     * Returns all the values related to \a index, order of values can be deduced from the sign of sizes dx,dy)
     *  of the cell contained in the raster extent (see extent()
     */
    const QVector<double> data( int index ) const;

    //! Returns the raster extent of all the grids
    ReosRasterExtent extent() const;

    //! Returns whether the gridded rainfallis valid
    bool isValid() const;

    //! Overrides the coordinates system with the wkt string \a crs of the gridded rainfall without modifying the coordintaes
    void overrideCrs( const QString &crs );

    //! Transform the gridded rain to fit with extent \a destination with resolution \a resolX and \a resolY
    ReosGriddedRainfall *transform( const ReosMapExtent &destination, double resolX, double resolY, QObject *parent = nullptr ) const;

  private:
    ReosGriddedRainfall( QObject *parent = nullptr );
    std::unique_ptr<ReosGriddedRainfallProvider> mProvider;
    QString mOverridenCrs;
};

class ReosGriddedRainItem : public ReosRainfallItem
{
  public:
    ReosGriddedRainItem( const QString &name, const QString &description )
      : ReosRainfallItem( name, description, ReosRainfallItem::GriddedData )
    {

    }

    ReosEncodedElement encode() const {return ReosEncodedElement();}
};

#endif // REOSGRIDDEDRAINITEM_H
