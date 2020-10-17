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

#include <queue>
#include <thread>
#include <algorithm>
#include <QPolygonF>

#include "reosprocess.h"
#include "reosmemoryraster.h"
#include "reosrasterline.h"
#include "reosrastertrace.h"


namespace ReosRasterWatershed
{
  typedef ReosRasterMemory<unsigned char> Directions;
  typedef ReosRasterMemory<unsigned char> Watershed;

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

class ReosRasterWatershedMarkerFromDirection: public ReosProcess
{
  public:

    //! Struct used to store information during ascension of the direction raster

    ReosRasterWatershedMarkerFromDirection( ReosRasterWatershedFromDirectionAndDownStreamLine *mParent,
                                            const ReosRasterWatershed::Climber &initialClimb,
                                            ReosRasterWatershed::Directions directions,
                                            ReosRasterWatershed::Watershed &watershed,
                                            const ReosRasterLine &excludedCell );

    void start() override;

  private:
    ReosRasterWatershedFromDirectionAndDownStreamLine *mParent;
    ReosRasterWatershed::Directions mDirections;
    ReosRasterWatershed::Watershed &mWatershed;
    ReosRasterLine mExcludedPixel;
    std::queue<ReosRasterWatershed::Climber> mClimberToTreat;
    size_t mMaxClimberStored = 100;
};

/**
 * Class that produce a raster defining a watershed with unique value from a direction raster and a downstream line
 */
class ReosRasterWatershedFromDirectionAndDownStreamLine: public ReosProcess
{
  public:
    //! Constrcutor with \a rasterDirection and downstream \a line
    ReosRasterWatershedFromDirectionAndDownStreamLine( ReosRasterWatershed::Directions rasterDirection, const ReosRasterLine &line );
    ReosRasterWatershedFromDirectionAndDownStreamLine( ReosRasterWatershed::Directions rasterDirection, const ReosRasterLine &line, ReosRasterTestingCell *testingCell );

    void start() override;
    void stopAsSoonAsPossible( bool b ) override;

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

    std::mutex mMutexClimber;
    std::mutex mMutexEndOfPath;

    std::unique_ptr<ReosRasterTestingCell> mTestingCell;

    //! methods used by ReosRasterWatershedMarkerFromDirection in other threads
    ReosRasterWatershed::Climber getClimberFromPool( bool &available );
    void addClimberInPool( const ReosRasterWatershed::Climber &climb );
    void proposeEndOfPath( ReosRasterWatershed::Climber climber );
    bool testCell( const ReosRasterCellPos &cell ) const;

    friend class ReosRasterWatershedMarkerFromDirection;
};

class ReosRasterWatershedToVector: public ReosProcess
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

class ReosRasterWatershedTraceDownstream: public ReosProcess
{
  public:
    //! Constructor with the \a directionRaster, the \a stopLine, the \a extent of the raster in the map and the position of the startinpoint \a startPos
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

#endif // HDRASTERWATERSHED_H
