/***************************************************************************
                      reosrasterwatershed.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDRASTERWATERSHED_H
#define HDRASTERWATERSHED_H

#define SIP_NO_FILE

#include <queue>
#include <thread>
#include <algorithm>
#include <iostream>
#include <QPolygonF>
#include <QtConcurrentMap>

#include "reosprocess.h"
#include "reosmemoryraster.h"
#include "reosrasterline.h"
#include "reosrastertrace.h"


namespace ReosRasterWatershed
{
  typedef ReosRasterMemory<unsigned char> Directions;
  typedef ReosRasterMemory<unsigned char> Watershed;
  typedef ReosRasterMemory<float> Dem;

  struct Climber
  {
    Climber() = default;
    Climber( const ReosRasterCellPos &p ): pos( p )
    {}
    Climber( const ReosRasterCellPos &p, double length ): pos( p ), lengthPath( length ) {}
    ReosRasterCellPos pos;
    double lengthPath = 0;
  };
}

class ReosRasterWatershedFromDirectionAndDownStreamLine;

class REOSCORE_EXPORT ReosRasterWatershedDirectionCalculation: public ReosProcess
{
  public:
    ReosRasterWatershedDirectionCalculation( const ReosRasterWatershed::Dem &dem );

    void stop( bool b ) override;

    int currentProgression() const override;
    int maxProgression() const override;

    void start() override;

    struct Job
    {
      int startRow;
      int endRow;
      ReosRasterWatershed::Dem *dem;
      ReosRasterWatershed::Directions *directions;
    };

    static void calculateDirection( Job job );

    ReosRasterWatershed::Directions directions() const;

  private:
    ReosRasterWatershed::Dem mDem;
    ReosRasterWatershed::Directions mDirections;
    std::vector<std::thread> mThreads;

    QFuture<void> mFuture;

};

/**
 * Class used to mark cells from a direction raster that are in the same watershed.
 * To do that, the class ascends the direction raster until the highest cells.
 * During this ascension, the length of the path is calculated.
 * For this a "Climber" is used that store the cell position and the length of the path from the beginning
 *
 * The instance of this class is created from an instance ReosWatershedFromDirectionAndDownStreamLine and link with this
 * instance that can provide new starting cell if needed. Several instance of ReosRasterWatershedMarkFromDirection can work
 * in different parralel threads with a common ReosWatershedFromDirectionAndDownStreamLine parent instance.
 *
 */

class REOSCORE_EXPORT ReosRasterWatershedMarkerFromDirection: public ReosProcess
{
  public:

    ReosRasterWatershedMarkerFromDirection( ReosRasterWatershedFromDirectionAndDownStreamLine *mParent,
                                            const ReosRasterWatershed::Climber &initialClimb,
                                            const ReosRasterWatershed::Directions &directions,
                                            ReosRasterWatershed::Watershed &watershed,
                                            const ReosRasterLine &excludedCell );

    //! Set a dem for

    void start() override;

  private:
    ReosRasterWatershedFromDirectionAndDownStreamLine *mParent;
    const ReosRasterWatershed::Directions mDirections;
    ReosRasterWatershed::Watershed &mWatershed;
    ReosRasterLine mExcludedPixel;
    std::queue<ReosRasterWatershed::Climber> mClimberToTreat;
    size_t mMaxClimberStored = 100;
};

/**
 * Class that produce a raster defining a watershed with unique value from a direction raster and a downstream line
 */
class REOSCORE_EXPORT ReosRasterWatershedFromDirectionAndDownStreamLine: public ReosProcess
{
  public:
    //! Constructor with \a rasterDirection and downstream \a line
    ReosRasterWatershedFromDirectionAndDownStreamLine(
      const ReosRasterWatershed::Directions &rasterDirection,
      const ReosRasterLine &line );

    ReosRasterWatershedFromDirectionAndDownStreamLine(
      const ReosRasterWatershed::Directions &rasterDirection,
      const ReosRasterLine &line,
      ReosRasterTestingCell *testingCell );

    void start() override;
    void stop( bool b ) override;

    //! Returns the raster watershed defined by this class after calculation
    ReosRasterWatershed::Watershed watershed() const;

    //! Returns the first defined cells, that is on the middle of the downstream line
    ReosRasterCellPos firstCell() const;
    //! Returns the cells at the end of the longer path
    ReosRasterCellPos endOfLongerPath() const;

  private:
    ReosRasterWatershed::Directions mDirections;
    ReosRasterWatershed::Watershed mWatershed;
    ReosRasterLine mDownstreamLine;
    std::list<ReosRasterWatershed::Climber> mPoolCellsToTreat;
    int mCounter;
    std::vector<std::thread> mThreads;
    std::vector<std::unique_ptr<ReosRasterWatershedMarkerFromDirection>> mJobs;
    ReosRasterCellPos mFirstCell;

    ReosRasterWatershed::Climber mEndOfLongerPath;

    mutable QMutex mMutexClimber;
    mutable QMutex mMutexEndOfPath;

    std::unique_ptr<ReosRasterTestingCell> mTestingCell;

    //! methods used by ReosRasterWatershedMarkerFromDirection in other threads
    ReosRasterWatershed::Climber getClimberFromPool( bool &available );
    void addClimberInPool( const ReosRasterWatershed::Climber &climb );
    void proposeEndOfPath( ReosRasterWatershed::Climber climber );
    bool testCell( const ReosRasterCellPos &cell ) const;

    friend class ReosRasterWatershedMarkerFromDirection;
};

class REOSCORE_EXPORT ReosRasterWatershedToVector: public ReosProcess
{
  public:

    ReosRasterWatershedToVector( ReosRasterWatershed::Watershed rasterWatershed,
                                 const ReosRasterExtent &extent,
                                 const ReosRasterCellPos &cellInWatershed );
    void start() override;

    const QPolygonF watershed() const;

  private:
    ReosRasterWatershed::Watershed mRasterWatershed;
    ReosRasterExtent mExtent;
    std::unique_ptr<ReosRasterTraceBetweenCellsUniqueValue<unsigned char>> mWatershedTrace = nullptr;
    QList<QPoint> mEliminationPoint;
};

class REOSCORE_EXPORT ReosRasterWatershedTraceDownstream: public ReosProcess
{
  public:
    //! Constructor with the \a directionRaster, the \a stopLine, the \a extent of the raster in the map and the position of the starting point \a startPos
    ReosRasterWatershedTraceDownstream(
      ReosRasterWatershed::Directions directionRaster,
      const ReosRasterLine stopLine,
      const ReosRasterExtent &extent,
      const ReosRasterCellPos &startPos );

    /**
     * Constructor with the \a directionRaster, the polygon limit \a polyLimit, the \a extent of the raster in the map and the position of the startinpoint \a startPos
     *
     * \note the tracing start in polyLimit and will stop when the tracing comes out this limit
     */
    ReosRasterWatershedTraceDownstream(
      ReosRasterWatershed::Directions directionRaster,
      const QPolygonF &polyLimit,
      const ReosRasterExtent &extent,
      const ReosRasterCellPos &startPos );

    void start() override;

    //! Returns the resulting polyline
    QPolygonF resultPolyline() const;
  private:

    ReosRasterWatershed::Directions mDirectionRaster;
    ReosRasterLine mStopLine;
    ReosRasterExtent mEmpriseRaster;
    ReosRasterCellPos mPos;

    QPolygonF mResultPolyline;
    QPolygonF mPolyLimit;
};

class REOSCORE_EXPORT ReosRasterAverageValueInPolygon: public ReosProcess
{
    Q_OBJECT
  public:
    ReosRasterAverageValueInPolygon( const ReosRasterMemory<float> &entryRaster,
                                     ReosRasterExtent &rasterExtent,
                                     const QPolygonF &polygon ):
      mEntryRaster( entryRaster ), mRasterExtent( rasterExtent ), mPolygon( polygon )
    {}

    void start() override;
    float result() const;

    struct Job
    {
      int startRow;
      int endRow;
      ReosRasterMemory<float> *entryRaster;
      ReosRasterMemory<char> *rasterizedPolygon;
      int valueCount;
      double sum;
    };

  private:

    ReosRasterMemory<float> mEntryRaster;
    ReosRasterExtent mRasterExtent;
    const QPolygonF mPolygon;
    float mResult;

};

static void averageOnJob( typename ReosRasterAverageValueInPolygon::Job &job );





#endif // HDRASTERWATERSHED_H
