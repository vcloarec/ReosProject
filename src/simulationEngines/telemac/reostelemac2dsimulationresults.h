/***************************************************************************
  reostelemac2dsimulationresult.h - ReosTelemac2DSimulationResult

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
#ifndef REOSTELEMAC2DSIMULATIONRESULTS_H
#define REOSTELEMAC2DSIMULATIONRESULTS_H

#include <mdal.h>
#include <QMap>
#include <QDateTime>

#include "reoshydraulicsimulationresults.h"

class ReosTelemac2DSimulation;
class ReosMesh;

struct CacheDataset
{
  QVector<int> activeFaces;
  QVector<double> waterDepth;
};

class ReosTelemac2DSimulationResults : public ReosHydraulicSimulationResults
{
  public:
    ReosTelemac2DSimulationResults( const ReosTelemac2DSimulation *simulation, const ReosMesh *mesh, const QString &fileName, QObject *parent = nullptr );
    ~ReosTelemac2DSimulationResults();

    int groupCount() const override;
    int datasetCount( int groupIndex ) const override;
    DatasetType datasetType( int groupIndex ) const override;
    int groupIndex( DatasetType type ) const override;
    void groupMinMax( int groupIndex, double &minimum, double &maximum ) const override;
    QDateTime groupReferenceTime( int groupIndex ) const override;
    ReosDuration datasetRelativeTime( int groupIndex, int datasetIndex ) const override;
    bool datasetIsValid( int groupIndex, int datasetIndex ) const override;
    void datasetMinMax( int groupIndex, int datasetIndex, double &min, double &max ) const override;
    QVector<double> datasetValues( int groupIndex, int index ) const override;
    QVector<int> activeFaces( int index ) const override;
    QDateTime runDateTime() const override;
    QMap<QString, ReosHydrograph *> outputHydrographs() const override;
    int datasetIndexClosestBeforeTime( int groupIndex, const QDateTime &time ) const override;
    QString unitString( DatasetType dataType ) const override;


  private:
    QString mFileName;
    MDAL_MeshH mMeshH = nullptr;
    mutable QDateTime mReferenceTime;
    QMap<DatasetType, int> mTypeToTelemacGroupIndex;
    double mDryDepthValue = 0.015;
    QVector<QVector<int>> mFaces;
    mutable QVector<CacheDataset> mCache;
    QMap<QString, ReosHydrograph *> mOutputHydrographs;
    mutable QMap<ReosDuration, int> mTimeToTimeStep;
    mutable QVector<ReosDuration> mTimeSteps;

    void populateTimeStep() const;

};

#endif // REOSTELEMAC2DSIMULATIONRESULTS_H
