/***************************************************************************
  reoshecrassimulationresults.h - ReosHecRasSimulationResults

 ---------------------
 begin                : 6.11.2022
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
#ifndef REOSHECRASSIMULATIONRESULTS_H
#define REOSHECRASSIMULATIONRESULTS_H

#include <mdal.h>

#include "reoshydraulicsimulationresults.h"
#include "reoshecrasproject.h"

class ReosHecRasSimulation;

struct CacheDataset
{
  QVector<int> activeFaces;
};

class ReosHecRasSimulationResults : public ReosHydraulicSimulationResults
{
  public:
    ReosHecRasSimulationResults( const ReosHecRasSimulation *simulation, QObject *parent );
    ~ReosHecRasSimulationResults();

    // ReosMeshDatasetSource interface

    //****** members that are the same for Telemax (factorization possible)
    int datasetCount( int groupIndex ) const override;
    QDateTime groupReferenceTime( int groupIndex ) const override;
    ReosDuration datasetRelativeTime( int groupIndex, int datasetIndex ) const override;
    void groupMinMax( int groupIndex, double &minimum, double &maximum ) const override;
    bool datasetIsValid( int groupIndex, int datasetIndex ) const override;
    void datasetMinMax( int groupIndex, int datasetIndex, double &min, double &max ) const override;
    int datasetValuesCount( int groupIndex, int datasetIndex ) const override;
    int datasetIndexClosestBeforeTime( int groupIndex, const QDateTime &time ) const override;
    int datasetIndex( int groupIndex, const QDateTime &time ) const override;
    //******

    ReosMeshDatasetSource::Location groupLocation( int groupIndex ) const override;
    QVector<double> datasetValues( int groupIndex, int index ) const override;
    QVector<int> activeFaces( int index ) const override;
    QMap<QString, ReosHydrograph *> outputHydrographs() const override;

    QDateTime runDateTime() const override;

    QString unitString( DatasetType dataType ) const {return QString();}

  private:
    const ReosHecRasProject mProject;
    const QString mPlanId;

    //****** members that are the same for Telemax (factorization possible)
    MDAL_MeshH mMeshH;
    mutable QDateTime mReferenceTime;
    mutable QMap<ReosDuration, int> mTimeToTimeStep;
    mutable QVector<ReosDuration> mTimeSteps;
    mutable QVector<CacheDataset> mCache;
    QMap<DatasetType, int> mTypeToSourceGroupIndex;

    int groupIndexToSourceIndex( int groupIndex ) const;

    void populateTimeStep() const;
    //*****
};

#endif // REOSHECRASSIMULATIONRESULTS_H
