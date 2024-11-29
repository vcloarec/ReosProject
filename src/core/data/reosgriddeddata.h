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
#include "reosduration.h"
#include "reostimeseries.h"
#include "reos_sip.h"


class ReosWatershed;
class ReosRasterExtent;
class ReosGriddedDataProvider;
class ReosGriddedRainfallRendererFactory;
class ReosColorShaderSettings;


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

    //! Returns a pointer to the data provider
    virtual ReosGriddedDataProvider *dataProvider() const SIP_SKIP;

    //! Returns whether the gridded rainfallis valid
    bool isValid() const;

    //! Returns the count of grids (e.g. time steps)
    int gridCount() const;

    /**
     * Returns all the values related to \a index, order of values can be deduced from the sign of sizes dx,dy)
     *  of the cell contained in the raster extent (see extent()
     */
    const QVector<double> values( int index ) const;

    const QVector<double> valuesInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const;

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

    //! Exports to a TIFF file with path \a fileName the grid at position \a index
    void exportToTiff( int index, const QString &fileName ) const;

  public slots:
    void updateData() const override;

  signals:
    void loadingFinished();

  protected:
    void makeConnection();
    QString mOverridenCrs;

    void decodeProvider( const ReosEncodedElement &element, const ReosEncodeContext &context )SIP_SKIP;
    void setProvider( ReosGriddedDataProvider *provider ) SIP_SKIP;

    void setRenderer( ReosGriddedRainfallRendererFactory *rendererFactory ) SIP_SKIP;
    ReosGriddedRainfallRendererFactory *renderer() const SIP_SKIP;

  private:
    QString formatKey( const QString &rawKey ) const;

    std::unique_ptr<ReosGriddedRainfallRendererFactory> mRendererFactory;
    std::unique_ptr<ReosGriddedDataProvider> mProvider;
};

#ifndef SIP_RUN

class AverageCalculation : public ReosProcess
{
  public:
    ReosRasterExtent gridExtent;
    QPolygonF watershedPolygon;
    ReosDuration timeStep;
    bool usePrecision = false;
    void start() override;

    ReosRasterMemory<double> rasterizedWatershed;
    ReosRasterExtent rasterizedExtent;
    int xOri = -1;
    int yOri = -1;
};
#endif // No SIP_RUN

class REOSCORE_EXPORT ReosDataGriddedOnWatershed SIP_ABSTRACT
{
  public:
    ReosDataGriddedOnWatershed( ReosWatershed *watershed, ReosGriddedData *griddeddata );

    double calculateValueAt( int index ) const;

    virtual void preCalculate() const = 0;

  protected:

    virtual void onCalculationFinished() = 0;
    virtual void onDataChanged() const = 0;
    virtual QDateTime timeAtIndex( int i ) const = 0;
    virtual void setDataActualized() const = 0;

    void launchCalculation();

  private:
    mutable AverageCalculation *mCurrentCalculation = nullptr;

    QPointer<ReosWatershed> mWatershed;
    QPointer<ReosGriddedData> mGriddedData;

    ReosRasterMemory<double> mRasterizedWatershed;
    ReosRasterExtent mRasterizedExtent;
    int mXOri = -1;
    int mYOri = -1;

    AverageCalculation *getCalculationProcess() const;
};


class REOSCORE_EXPORT ReosSeriesFromGriddedDataOnWatershed : public ReosTimeSeriesConstantInterval, public ReosDataGriddedOnWatershed
{
    Q_OBJECT
  public:
    ReosSeriesFromGriddedDataOnWatershed( ReosWatershed *watershed, ReosGriddedData *griddedData, QObject *parent = nullptr );
    ~ReosSeriesFromGriddedDataOnWatershed();

    //! Returns a new created instance from \a watershed and \a gridded rainfall. Caller takes ownership.
    static ReosSeriesFromGriddedDataOnWatershed *create( ReosWatershed *watershed, ReosGriddedData *griddedData ) SIP_FACTORY;

    double valueAt( int i ) const override;

    //! Calculates all values directly.
    void preCalculate() const override;

  signals:
    void calculationFinished();

#ifndef SIP_RUN
  protected:
    void updateData() const override;
    void onCalculationFinished() override;
    void onDataChanged() const override;
    QDateTime timeAtIndex( int i ) const override;
    void setDataActualized() const override;
#endif // No SIP_RUN

  private slots:
    void onWatershedGeometryChanged();

};





#endif // REOSGRIDDEDDATA_H
