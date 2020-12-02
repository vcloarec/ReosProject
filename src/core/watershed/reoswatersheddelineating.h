/***************************************************************************
                      reoswatersheddelineating.h
                     --------------------------------------
Date                 : 04-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSWATERSHEDDELINEATING_H
#define REOSWATERSHEDDELINEATING_H

#include <memory>

#include "reosgisengine.h"
#include "reosmapextent.h"
#include "reosmodule.h"
#include "reosprocess.h"
#include "reoswatershed.h"
#include "reosrasterwatershed.h"
#include "reosdigitalelevationmodel.h"

class ReosRasterFillingWangLiu;
class ReosWatershedTree;

class ReosWatershedDelineatingProcess: public ReosProcess
{
  public:
    ReosWatershedDelineatingProcess( ReosDigitalElevationModel *dem,
                                     const ReosMapExtent &mapExtent,
                                     const QPolygonF &downtreamLine,
                                     const QList<QPolygonF> &burningLines );

    ReosWatershedDelineatingProcess( ReosWatershed *downstreamWatershed,
                                     const QPolygonF &downtreamLine );

    void start() override;

    QPolygonF watershedPolygon() const;
    QPolygonF streamLine() const;
    ReosRasterWatershed::Directions directions() const;

    ReosRasterExtent outputRasterExtent() const;

  private:
    ReosMapExtent mExtent;
    std::unique_ptr<ReosDigitalElevationModel> mEntryDem;
    const QPolygonF mDownstreamLine;
    const QList<QPolygonF> mBurningLines;

    ReosRasterWatershed::Directions mDirections;
    QPolygonF mOutputWatershed;
    QPolygonF mOutputStreamline;
    ReosRasterExtent mOutputRasterExtent;

    static void burnRasterDem( ReosRasterMemory<float> &rasterDem, const QList<QPolygonF> &burningLines, const ReosRasterExtent &rasterExtent );
};


class ReosWatershedDelineating : public ReosModule
{
  public:
    //! State of the tool chain
    enum State
    {
      NoDigitalElevationModel,
      WaitingForDownstream,
      WaitingForExtent,
      WaitingWithBroughtBackExtent,
      WaitingforProceed,
      WaitingForValidate
    };

    ReosWatershedDelineating( ReosModule *parent, ReosWatershedTree *watershedTree, ReosGisEngine *gisEngine );
    State currentState() const;

    // -------- Settings
    //! Returns wheher the instance has a registered DEM to operate
    bool hasValidDigitalElevationModel() const;
    //! Sets the DEM to operate
    bool setDigitalElevationModelDEM( const QString &layerId );

    //! Sets the downstream line, return true if sucessful
    bool setDownstreamLine( const QPolygonF &downstreamLine );
    //! Sets the predefined extent, return true if sucessful
    bool setPreDefinedExtent( const ReosMapExtent &extent );

    //! Adds a burning line
    void setBurningLines( const QList<QPolygonF> &burningLines );

    //! Returns whether the module has direction data ready for proceed
    bool hasDirectionData() const;

    // -------- Processing
    //! Start the delineating, return true if starting this process is sucessful
    bool startDelineating();
    ReosProcess *delineatingProcess();

    //! Returns if the delineating process is finished
    bool isDelineatingFinished() const;

    // ------ Results
    //! Returns the last wateshed polygon delineated
    QPolygonF lastWatershedDelineated() const;

    //! Returns the last downstream line polyline delineated
    QPolygonF lastStreamLine() const;

    // -------- validating and watershed producing

    //! Validates and and add the xwatershed to the \a store, returns pointer to the watershed
    ReosWatershed *validateWatershed();

  private slots:
    void onDelineatingFinished();

  private:
    ReosWatershedTree *mWatershedTree;
    ReosGisEngine *mGisEngine = nullptr;
    QString mDEMLayerId;
    State mCurrentState = NoDigitalElevationModel;
    QPolygonF mDownstreamLine;
    ReosMapExtent mExtent;
    ReosWatershed *mDownstreamWatershed = nullptr;
    QList<QPolygonF> mBurningLines;
    bool mIsBurningLineUpToDate = false;

    std::unique_ptr<ReosWatershedDelineatingProcess> mProcess;

    //! Considering result, test if the predefined extent is valid, if not return false and set the state to WaitingWithBroughtBackExtent
    void testPredefinedExtentValidity();


};

#endif // REOSWATERSHEDDELINEATING_H
