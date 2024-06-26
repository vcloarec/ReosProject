/***************************************************************************
  reosgriddeddata.h - ReosGriddedData

 ---------------------
 begin                : 25.6.2024
 copyright            : (C) 2024 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSGRIDDEDDATA_H
#define REOSGRIDDEDDATA_H

#include "reosmemoryraster.h"
#include "reosrenderedobject.h"
#include "reos_sip.h"


class ReosRasterExtent;
class ReosGriddedDataProvider;
class ReosGriddedRainfallRendererFactory;
class ReosColorShaderSettings;
class ReosDuration;


/**
 * Class that represents gridded data.
 */
class REOSCORE_EXPORT ReosGriddedData : public ReosRenderedObject
{
    Q_OBJECT
  public:
    /**
     * Constructor.
     */
    ReosGriddedData( QObject *parent = nullptr );

    /**
    * Constructor with \a datasource or \a providerkey.
    */
    ReosGriddedData( const QString &dataSource, const QString &providerKey, QObject *parent = nullptr );


    QString type() const override SIP_SKIP;
    ReosObjectRenderer *createRenderer( ReosRendererSettings *settings ) override SIP_SKIP ;
    ReosRendererObjectMapTimeStamp *createMapTimeStamp( ReosRendererSettings *settings ) const override SIP_SKIP;
    ReosMapExtent extent() const override SIP_SKIP;
    QList<ReosColorShaderSettings *> colorShaderSettings() const override SIP_SKIP;

    static QString staticType();

    //! Returns the count of grids (e.g. time steps)
    int gridCount() const;

    /**
     * Returns all the values related to \a index, order of values can be deduced from the sign of sizes dx,dy)
     *  of the cell contained in the raster extent (see extent()
     */
    const QVector<double> values( int index ) const;

    //! Returns the start time related to the grid with \a index
    const QDateTime startTime( int index ) const;

    //! Returns the end time related to the grif with \a index
    const QDateTime endTime( int index ) const;

    //! Returns the time extent of the gridded rainfall
    virtual QPair<QDateTime, QDateTime> timeExtent() const SIP_SKIP;

    //! Returns the minimum time step of the gridded series
    ReosDuration minimumTimeStep() const;

    //! Returns whether the data support extraction of subgrid
    bool supportExtractSubGrid() const;

    bool getDirectMinMaxValue( double &min, double &max ) const SIP_SKIP;

    void calculateMinMaxValue( double &min, double &max ) const SIP_SKIP;

    //! Returns the index corresponding to \a time
    int dataIndex( const QDateTime &time ) const;

    //! Returns the raster extent of all the grids
    ReosRasterExtent rasterExtent() const;

    //! Copies data from another rainfall
    void copyFrom( ReosGriddedData *other ) SIP_SKIP;

    //! Copies new from a rainfall provider
    void copyFrom( ReosGriddedDataProvider *provider ) SIP_SKIP;

  public slots:
    void updateData() const override;

  signals:
    void loadingFinished();

  protected:
    void makeConnection();
    QString mOverridenCrs;
    std::unique_ptr<ReosGriddedRainfallRendererFactory> mRendererFactory;
    std::unique_ptr<ReosGriddedDataProvider> mProvider;

  private:
    QString formatKey( const QString &rawKey ) const;
};
#endif // REOSGRIDDEDDATA_H
