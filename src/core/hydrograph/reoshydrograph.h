/***************************************************************************
  reoshydrograph.h

 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDROGRAPH_H
#define REOSHYDROGRAPH_H

#include <QColor>
#include <QMutex>
#include <QSet>

#include "reostimeserie.h"
#include "reossyntheticrainfall.h"
#include "reosprocess.h"

class ReosHydrograph;

//! Class that represents a hydrograph
class REOSCORE_EXPORT ReosHydrograph : public ReosTimeSerieVariableTimeStep
{
    Q_OBJECT
  public:
    ReosHydrograph( QObject *parent = nullptr, const QString &providerKey = QString(), const QString &dataSource = QString() );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosTimeSerieVariableTimeStep::staticType() + ':' + QStringLiteral( "hydrograph" );}

    ReosEncodedElement encode() const;
    static ReosHydrograph *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

    bool hydrographIsObsolete() const ;
    void setHydrographObsolete();

  protected:
    ReosHydrograph( const ReosEncodedElement &element, QObject *parent = nullptr );

    void updateData() const override;

    friend class ReosHydrographGroup;
};

//! Process abstract class that handle the calculation of the hydrograph an onother thread
class REOSCORE_EXPORT ReosHydrographCalculation : public ReosProcess
{
    Q_OBJECT
  public:
    //! Returns the hydrograph result, has to be call after the process is finished, if \a parent is not specified, the caller need to take ownership
    ReosHydrograph *getHydrograph( QObject *parent = nullptr );

    //! Returns a pointer to the hydrograph keeping ownership
    ReosHydrograph *hydrograph();

  protected:
    std::unique_ptr<ReosHydrograph> mHydrograph;
};


class REOSCORE_EXPORT ReosHydrographGroup: public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydrographGroup( QObject *parent = nullptr ) : ReosDataObject( parent )
    {}

    //! Returns the count of hydrographs stored
    virtual int hydrographCount() const = 0;

  protected:
    void registerInputdata( ReosDataObject *input, ReosHydrograph *hydrograph );
    void deregisterInputData( ReosDataObject *input, ReosHydrograph *hydrograph );

  protected slots:
    virtual void updateHydrographFromSignal();
    virtual void updateHydrograph( ReosHydrograph *hydrographs );

  private slots:
    virtual void onInputDataDestroy();

  private:
    QMap<ReosDataObject *, QList<QPointer<ReosHydrograph>>> mMapInputToHydrographs;
};

class REOSCORE_EXPORT ReosHydrographsStore : public ReosHydrographGroup
{
    Q_OBJECT
  public:
    ReosHydrographsStore( QObject *parent = nullptr );

    //! Add an hydrograph to the sore, take ownership
    void addHydrograph( ReosHydrograph *hydrograph );

    //! Remove and destroy the hydrograph at position \a index
    void removeHydrograph( int index );

    //! Returns a pointer to the hydrograph at position \a index, nullptr if not exists
    ReosHydrograph *hydrograph( int index ) const;

    //! Returns the count of hydrographs stored
    int hydrographCount() const override;

    //! Returns the list of the hydrograph names
    QStringList hydrographNames() const;

    //! Returns the hydrographs that are in the time range \a startTime, \a endTime
    QList<ReosHydrograph *> hydrographsForTimeRange( const QDateTime &startTime, const QDateTime &endTime ) const;

    //! Returns all the hydrographs
    QList<ReosHydrograph *> allHydrographs() const;

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &element );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosDataObject::staticType() + ':' +  QStringLiteral( "hydrograph-store" );}

  signals:
    void hydrographRemoved( int index ) const;
    void hydrographChanged();

  private:
    QList<ReosHydrograph *>  mHydrographs;
};


class ReosMeteorologicModelsCollection;
class ReosMeteorologicModel;
class ReosWatershed;
class ReosTransferFunctionCalculation;
class ReosSeriesRainfall;
class ReosRunoffModelsGroup;
class ReosRunoff;


/**
 * Class that handle the runoff hydrograph of watersheds considering all the meteorological models contains
 * in a ReosMeteorologicModelsCollection instance.
 *
 * The current watershed is set with setWatershed(), then the runoff hydrograph produced by this watershed depending
 * of a meteoroloical model is obtains by hydrograph( ReosMeteorologicModel *meteoModel )
 */
class REOSCORE_EXPORT ReosRunoffHydrographsStore: public ReosHydrographGroup
{
    Q_OBJECT
  public:
    ReosRunoffHydrographsStore( ReosMeteorologicModelsCollection *meteoModelCollection,
                                QObject *parent = nullptr );

    void setWatershed( ReosWatershed *watershed );

    //! Returns the count of hydrographs stored
    int hydrographCount() const override;

    /**
     * Return pointer to the hydrograph corresponding to \a meteomodel.
     *
     * \note  Following event, the hydrograph can be deleted or updated without warning.
     * Caller of his method has to care about this and not use the raw pointer being
     * sure there is always controle of the dangling raw pointer
     */
    QPointer<ReosHydrograph> hydrograph( ReosMeteorologicModel *meteoModel );

    /**
     * Return pointer to the runoff corresponding to \a meteomodel.
     *
     * \note  Following even, the hydrograph can be deleted or updated without warning.
     * Caller of his method has to care about this and not use the raw pointer being
     * sure there is always controle of the dangling raw pointer
     */
    QPointer<ReosRunoff> runoff( ReosMeteorologicModel *meteoModel );

    void updateHydrograph( ReosHydrograph *hydrograph ) override;

  public slots:
    void updateStore();

  signals:
    void hydrographReady( ReosHydrograph *hydrograph );

  private:
    struct HydrographCalculationData
    {
      QPointer<ReosSeriesRainfall> rainfall;
      ReosRunoff *runoff = nullptr;
      ReosHydrograph *hydrograph = nullptr;
      bool hasBeenAsked = false;
    };

    QMap < ReosMeteorologicModel *, HydrographCalculationData> mMeteoModelToHydrographCalculationData;
    QPointer<ReosMeteorologicModelsCollection> mMeteoModelsCollection;
    QPointer<ReosWatershed> mWatershed;

    QSet<ReosMeteorologicModel *> mModelMeteoToUpdate;
    QMap<ReosMeteorologicModel *, ReosHydrographCalculation *> mHydrographCalculation;

    bool mCalculationCanBeLaunch = true;

    void launchCalculation( ReosMeteorologicModel *meteoModel );

    int updateCount = 0;

    friend class ReosWatersehdTest;

};

#endif // REOSHYDROGRAPH_H
