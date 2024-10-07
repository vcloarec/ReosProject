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
#include "reosrastercompressed.h"

class ReosRasterFillingWangLiu;
class ReosWatershedTree;

#ifndef SIP_RUN
class ReosWatershedDelineatingProcess: public ReosProcess
{
    Q_OBJECT
  public:
    ReosWatershedDelineatingProcess(
      ReosDigitalElevationModel *dem,
      const ReosMapExtent &mapExtent,
      const QPolygonF &downtreamLine,
      const QString &downstreamLineCrs,
      const QList<QPolygonF> &burningLines,
      bool calculateAverageElevation = false );

    ReosWatershedDelineatingProcess(
      ReosWatershed *downstreamWatershed,
      const QPolygonF &downstreamLine,
      const QString &downstreamLineCrs,
      const QString &layerId,
      bool calculateAverageElevation = false );

    ReosWatershedDelineatingProcess(
      ReosDigitalElevationModel *dem,
      const ReosRasterWatershed::Directions &directions,
      const ReosRasterExtent &directionsExtent,
      const QPolygonF &downstreamLine,
      const QString &downstreamLineCrs,
      bool calculateAverageElevation = false );

    void start() override;

    QPolygonF watershedPolygon() const;
    QPolygonF streamLine() const;

    ReosRasterWatershed::Directions directions() const;
    ReosRasterWatershed::Watershed rasterizedWatershed() const;

    //! Returns the entry extent matching to dem raster resolution
    ReosRasterExtent predefinedRasterExtent() const;
    ReosRasterExtent outputRasterExtent() const;

    double averageElevation() const;

    bool calculateAverageElevation() const;

  private:
    ReosMapExtent mExtent;
    std::unique_ptr<ReosDigitalElevationModel> mEntryDem;
    const QPolygonF mDownstreamLine;
    const QList<QPolygonF> mBurningLines;

    ReosRasterWatershed::Directions mDirections;
    ReosRasterWatershed::Watershed mRasterizedWatershed;
    QPolygonF mOutputWatershed;
    QPolygonF mOutputStreamline;
    ReosRasterExtent mPredefinedRasterExtent;
    ReosRasterExtent mOutputRasterExtent;
    bool mCalculateAverageElevation = false;
    double mAverageElevation = 0;
};

#endif //No SIP_RUN


class REOSCORE_EXPORT ReosWatershedDelineating : public ReosModule
{
    Q_OBJECT
  public:
    //! State of the tool chain
    enum State
    {
      NoDigitalElevationModel,
      WaitingForDownstream,
      WaitingForExtent,
      WaitingWithBroughtBackExtent,
      WaitingforProceed,
      Delineating,
      WaitingForValidate,
      WaitingToRecord
    };

    ReosWatershedDelineating( ReosModule *parent, ReosWatershedTree *watershedTree, ReosGisEngine *gisEngine );
    State currentState() const;

    // -------- Settings
    //! Returns wheher the instance has a registered DEM to operate
    bool hasValidDigitalElevationModel() const;

    //! Sets the DEM to operate
    bool setDigitalElevationModelDEM( const QString &layerId );

    /**
     *  Sets the downstream line and its crs, return true if sucessful.
     *  If the crs is not provided, it is supposed the crs is the same than other inputs (extent or at least DEM crs)
     */
    bool setDownstreamLine( const QPolygonF &downstreamLine, const QString &lineCrs = QString() );

    //! Sets the predefined extent, return true if sucessful
    bool setPreDefinedExtent( const ReosMapExtent &extent );

    //! Adds a burning line
    void setBurningLines( const QList<QPolygonF> &burningLines );

    //! Returns all burning lines
    QList<QPolygonF> burninglines() const;

    //! Sets whether the average elevation of the watershed is calculated after delineating
    void setCalculateAverageElevation( bool calculate );

    //! Returns whether the module has direction data ready for proceed
    bool hasDirectionData() const;

    //! Returns the current extent
    ReosMapExtent currentExtent() const;

    // -------- Processing
    //! Prepare the delineating, return true if sucessful
    bool prepareDelineating();

    ReosProcess *delineatingProcess();

    //! Returns if the delineating process is finished
    bool isDelineatingFinished() const;

    // ------ Results
    //! Returns the last wateshed polygon delineated
    QPolygonF lastWatershedDelineated() const;

    //! Returns the last downstream line polyline delineated
    QPolygonF lastStreamLine() const;

    // -------- validating and watershed producing

    //! Validates the watershed and returns true if modification of delineating in necessary to add it
    bool validateWatershed( bool &needAdjusting );

    //! Store the watershed in the tree, returns pointer to the new watershed
    ReosWatershed *storeWatershed( bool adjustIfNeeded );

    ReosEncodedElement encode() const SIP_SKIP;
    void decode( const ReosEncodedElement &element ) SIP_SKIP;

    //! Considering result, test if the predefined extent is valid, if not return false and set the state to WaitingWithBroughtBackExtent
    void testPredefinedExtentValidity();

    //! Resets the delineating toolto waiting for a downstream line
    void reset();

    //! Clears all the data from the delineating tool
    void clear();

    static QString staticName() {return QStringLiteral( "watershed-delineating" );}

    struct DelineateResult
    {
      ReosRasterExtent outputRasterExtent;
      QPolygonF delineateWatershed;
      QPolygonF streamLine;
      double averageElevation;
    };

    static DelineateResult delineateWatershed(
      const QString &demLayerId,
      const QString &directionFile,
      const QPolygonF &downstreamLine,
      const QString &dsLineCrs,
      ReosGisEngine *gisEngine );

    static bool directionFromDem( const QString &demLayerId,
                                  const ReosMapExtent &extent,
                                  ReosGisEngine *gisEngine,
                                  const QString &fileName,
                                  const QString &burningLinesLayerUri = QString() );

    static void burnRasterDem( ReosRasterMemory<float> &rasterDem, const QList<QPolygonF> &burningLines, const ReosRasterExtent &rasterExtent ) SIP_SKIP;

  signals:
    void hasBeenReset();

  private slots:
    void onDelineatingFinished();
  private:
    ReosWatershedTree *mWatershedTree;
    ReosGisEngine *mGisEngine = nullptr;
    QString mDEMLayerId;
    State mCurrentState = NoDigitalElevationModel;
    QPolygonF mDownstreamLine;
    QString mDSLineCrs;
    ReosMapExtent mExtent;
    ReosWatershed *mDownstreamWatershed = nullptr;
    QList<QPolygonF> mBurningLines;
    bool mIsBurningLineUpToDate = false;
    bool mCalculateAverageElevation = true;

    std::unique_ptr<ReosWatershedDelineatingProcess> mProcess;

    std::unique_ptr<ReosWatershed> mCurrentWatershed;
};

#endif // REOSWATERSHEDDELINEATING_H
