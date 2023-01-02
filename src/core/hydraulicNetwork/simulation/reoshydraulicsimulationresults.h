/***************************************************************************
  reoshydraulicsimulationresults.h - ReosHydraulicSimulationResults

 ---------------------
 begin                : 1.4.2022
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
#ifndef REOSHYDRAULICSIMULATIONRESULTS_H
#define REOSHYDRAULICSIMULATIONRESULTS_H

#include <QObject>
#include <QVector>
#include <QDateTime>

#include "reoscore.h"
#include "reosmeshdatasetsource.h"
#include "reosduration.h"

class ReosHydraulicSimulation;
class ReosHydrograph;
class ReosMesh;
class ReosSpatialPosition;

class REOSCORE_EXPORT ReosHydraulicSimulationResults : public ReosMeshDatasetSource
{
    Q_OBJECT
  public:
    enum class DatasetType
    {
      None,
      WaterLevel,
      WaterDepth,
      Velocity,
    };

    ReosHydraulicSimulationResults( const ReosHydraulicSimulation *simulation, QObject *parent = nullptr );

    int groupCount() const override;
    QString groupName( int groupIndex ) const override;
    bool groupIsScalar( int groupIndex ) const override;

    QString groupId( DatasetType type ) const;
    QString groupId( int groupIndex ) const;
    DatasetType datasetType( int groupIndex ) const;
    int groupIndex( DatasetType type ) const;

    virtual QDateTime runDateTime() const = 0;
    virtual int datasetIndex( int groupIndex, const QDateTime &time ) const = 0;

    //! Returns reuslt hydrograph related to the ids of boundaries
    virtual QMap<QString, ReosHydrograph *> outputHydrographs() const = 0;

    virtual QList<QDateTime> timeSteps() const {return QList<QDateTime>();}

    double interpolateResultOnMesh( ReosMesh *mesh, const ReosSpatialPosition &position, const QDateTime &time, DatasetType dataType );

    virtual QString unitString( DatasetType dataType ) const = 0;

    QVector<double> resultValues( DatasetType datasetType, int index ) const;

  protected:
    void registerGroups( const QList<DatasetType> &types );

  private:
    QList<DatasetType> mGroupIndexToType;
};


class ReosHydraulicSimulationResultsDummy : public ReosHydraulicSimulationResults
{
  public:
    explicit ReosHydraulicSimulationResultsDummy( const ReosHydraulicSimulation *simulation, QObject *parent = nullptr );

    int datasetCount( int ) const override {return 0;}
    Location groupLocation( int ) const override {return Location::Vertex;}
    void groupMinMax( int, double &, double & ) const override {};
    QDateTime groupReferenceTime( int ) const override {return QDateTime();}
    ReosDuration datasetRelativeTime( int, int ) const override {return ReosDuration();}
    bool datasetIsValid( int, int ) const override {return false;}
    void datasetMinMax( int, int, double &, double & ) const override {}
    int datasetValuesCount( int, int ) const override {return 0;}
    QVector<double> datasetValues( int, int ) const override {return QVector<double>();}
    QVector<int> activeFaces( int ) const override {return QVector<int>();}
    int datasetIndexClosestBeforeTime( int, const QDateTime & ) const override {return 0;}
    QDateTime runDateTime() const override {return QDateTime();}
    int datasetIndex( int, const QDateTime & ) const override {return 0;}
    QMap<QString, ReosHydrograph *> outputHydrographs() const override {return mOutputHydrographs;}
    QString unitString( DatasetType ) const override {return QString();}

  private:
    QMap<QString, ReosHydrograph *> mOutputHydrographs;
};

#endif // REOSHYDRAULICSIMULATIONRESULTS_H
