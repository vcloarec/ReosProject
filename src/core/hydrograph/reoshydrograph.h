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

#include "reostimeserie.h"
#include "reossyntheticrainfall.h"

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

    void setInputData( ReosDataObject *dataObject );
    bool hydrographIsObsolete() const ;

  protected:
    ReosHydrograph( const ReosEncodedElement &element, QObject *parent = nullptr );

    void updateData() const override;

    friend class ReosHydrographSource;

};

class ReosHydrographSource: public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydrographSource( QObject *parent = nullptr ) : ReosDataObject( parent )
    {}

    //! Returns the count of hydrographs stored
    virtual int hydrographCount() const = 0;

  protected:
    void registerInputdata( ReosDataObject *input, ReosHydrograph *hydrograph );
    void deregisterInputData( ReosDataObject *input, ReosHydrograph *hydrograph );

  protected slots:
    virtual void updateHydrographFromSignal();
    virtual void updateHydrographs( ReosHydrograph *hydrographs );

  private slots:
    virtual void onInputDataDestroy();

  private:
    QMap<ReosDataObject *, QList<QPointer<ReosHydrograph>>> mMapInputToHydrographs;
};

class REOSCORE_EXPORT ReosHydrographStore : public ReosHydrographSource
{
    Q_OBJECT
  public:
    ReosHydrographStore( QObject *parent = nullptr ): ReosHydrographSource( parent ) {}

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

    QList<ReosHydrograph *> hydrographsForTimeRange( const QDateTime &startTime, const QDateTime &endTime );

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &element );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosDataObject::staticType() + ':' +  QStringLiteral( "hydrograph-store" );}

  private:
    QList<ReosHydrograph *>  mHydrographs;
};


class ReosMeteorologicModelsCollection;
class ReosMeteorologicModel;
class ReosWatershed;
class ReosTransferFunctionCalculation;
class ReosSerieRainfall;
class ReosRunoffModelsGroup;
class ReosRunoff;

class ReosRunoffHydrographStore: public ReosHydrographSource
{
    Q_OBJECT
  public:
    ReosRunoffHydrographStore( ReosMeteorologicModelsCollection *meteoModelCollection,
                               QObject *parent = nullptr );

    void setWatershed( ReosWatershed *watershed );

    //! Returns the count of hydrographs stored
    int hydrographCount() const override;

    /**
     * Return pointer to the hydrograph corresponding to \a meteomodel.
     *
     * \note  Following even, the hydrograph can be deleted or updated without warning.
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

  public slots:
    void updateStore();

  signals:
    void hydrographReady( ReosHydrograph *hydrograph );

  protected slots:
    void updateHydrographs( ReosHydrograph *hydrograph ) override;

  private:
    struct HydrographData
    {
      QPointer<ReosSerieRainfall> rainfall;
      ReosRunoff *runoff = nullptr;
      ReosHydrograph *hydrograph = nullptr;
    };

    QMap < ReosMeteorologicModel *, HydrographData> mMeteoModelToHydrograph;
    QPointer<ReosMeteorologicModelsCollection> mMeteoModelsCollection;
    QPointer<ReosWatershed> mWatershed;

    QSet<ReosMeteorologicModel *> mModelMeteoToUpdate;
    QMap<ReosMeteorologicModel *, ReosTransferFunctionCalculation *> mHydrographCalculation;

    bool mCalculationCanBeLaunch = true;

    void launchCalculation( ReosMeteorologicModel *meteoModel );

    int updateCount = 0;

    friend class ReosWatersehdTest;

};

#endif // REOSHYDROGRAPH_H
