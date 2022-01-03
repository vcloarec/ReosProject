/***************************************************************************
                      reoswatershed.h
                     --------------------------------------
Date                 : 10-2020
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

#ifndef REOSWATERSHED_H
#define REOSWATERSHED_H

#include "reoscore.h"
#include "reosmapextent.h"
#include "reosrastercompressed.h"
#include "reosrasterwatershed.h"
#include "reosexception.h"
#include "memory"

#include "reosgeometryutils.h"
#include "reosrasterwatershed.h"
#include "reosencodedelement.h"
#include "reosparameter.h"
#include "reosconcentrationtimecalculation.h"
#include "reoshydrograph.h"

class ReosGisEngine;
class ReosRunoffModelsGroup;
class ReosTransferFunction;
class ReosSerieRainfall;
class ReosHydrograph;

class REOSCORE_EXPORT ReosWatershed: public ReosDataObject
{
    Q_OBJECT
  public:
    enum Type
    {
      None,
      Automatic,
      Manual,
      Residual
    };

    ReosWatershed();

    //! Constructor used with manual delineating
    ReosWatershed( const QPolygonF &delineating,
                   const QPointF &outletPoint,
                   Type type );

    //! Constructor used with automatic delineating with direction data containded in the upstream watershed
    ReosWatershed( const QPolygonF &delineating,
                   const QPointF &outletPoint,
                   Type type,
                   const QPolygonF &downstreamLine,
                   const QPolygonF &streamPath,
                   const ReosRasterWatershed::Watershed &rasterizedWatershed,
                   const ReosRasterExtent &rasterizedWatershedExtent,
                   const QString &refLayerId );

    //! Constructor used with automatic delineating with direction data
    ReosWatershed( const QPolygonF &delineating,
                   const QPointF &outletPoint,
                   Type type,
                   const QPolygonF &downstreamLine,
                   const QPolygonF &streamPath,
                   const ReosRasterWatershed::Directions &direction,
                   const ReosRasterWatershed::Watershed &rasterizedWatershed,
                   const ReosRasterExtent &rasterExtent,
                   const QString &refLayerId );

    Type watershedType() const {return mType;}

    //! Returns the name of the watershed
    ReosParameterString *watershedName() const;

    //! Sets the name of the watershed
    void setWatershedName( const QString &name );

    //! Returns the extent of the watershed
    ReosMapExtent extent() const;

    //! Returns the delineating of the watershed
    QPolygonF delineating() const;

    //! Sets the delineating of the watershed
    void setDelineating( const QPolygonF &del );

    //! Returns the outlet point of the watershed
    QPointF outletPoint() const;

    //! Sets the outlet point of the watershed
    void setOutletPoint( const QPointF &outletPoint );

    //! Returns whether the watrshed or its parent contoins direction data
    bool hasDirectiondata( const QString &layerId ) const;

    //! Returns the directions raster associated with this watershed, returned raster is invalid if there is none
    ReosRasterWatershed::Directions directions( const QString &layerId ) const;

    //! Returns the extent of the raster direction, \see directions()
    ReosRasterExtent directionExtent( const QString &layerId ) const;

    //! Removes direction data present in the watershed or in its children
    void removeDirectionData();

    /**
     * Returns whether the watershed includes the \a point
     *
     * \note if the point is exactly on a segment of the delineating polygon, this point is considered outside
     */
    bool contain( const QPointF &point ) const;

    /**
     * Returns how the polygon or polyline \a line is contained in the watershed
     *
     * \note if a point of the line is exactly on a segment of the delineating polygon, this point is considered outside
     */
    ReosInclusionType contain( const QPolygonF &line ) const;

    //! Returns how this watershed is included by \a other
    ReosInclusionType isContainedBy( const ReosWatershed &other ) const;

    //! Returns the count of upstream watersheds
    int upstreamWatershedCount() const;

    //! Returns the count of watersheds that are directly upstream
    int directUpstreamWatershedCount() const;

    //! Returns the ith direct upstream watershed, return nullptr if there is not
    ReosWatershed *directUpstreamWatershed( int i ) const;

    /**
     * Adds a \a upstream watershed (take ownership) and returns a pointer to this new watershed
     * If \a adaptUpstreamDelineating is true, the upstram watershed delineating will be adapted to with its directly
     * downstream watershed or its sibling.
     */
    ReosWatershed *addUpstreamWatershed( ReosWatershed *upstreamWatershed, bool adjustIfNeeded );

    /**
     * Removes (if present) the watershed from the direct upstream watershed at position \a i, but do not delete it, returns a pointer to it.
     * Sub watershed are removed from the extracted watershed and are put in \a this watershed
     */
    ReosWatershed *extractOnlyDirectUpstreamWatershed( int i );

    /**
     * Removes (if present) the watershed from the direct upstream watershed at position \a i, but do not delete it, returns a pointer to it.
     * Sub watershed are maintained in the extracted watershed
     */
    ReosWatershed *extractCompleteDirectUpstreamWatershed( int i );

    //! Returns the smallest sub watershed that is downstream the line, if the line is partially included by any watershed, ok is false and return nullptr
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *upstreamWatershed( const QPolygonF &line, bool &ok ) const;

    //! Returns the smallest upstream watershed that contains the point
    ReosWatershed *upstreamWatershed( const QPointF &point, bool excludeResidual = false );

    //! Returns, if exists, a pointer to the direct downstream watershed, if not returns nullptr
    ReosWatershed *downstreamWatershed() const;

    //! Returns the position in the downstream watershed
    int positionInDownstreamWatershed() const;

    //! Returns a list of all upstream watershed sorted from upstream to downstream
    QList<ReosWatershed *> allUpstreamWatershedsFromUSToDS() const;

    //! Returns a list of all upstream watershed sorted downstream to upstream
    QList<ReosWatershed *> allUpstreamWatershedsFromDSToUS() const;

    //! Cuts the delineating of this delineating to fit in the \a other watershed delineating (cut all that is outside the other and intersect with sub watershed)
    void fitIn( const ReosWatershed &other );

    //! Adjusts the delineating of this watershed to not intersect with the \a other watershed delineating (cut all that is inside the other)
    void adjust( const ReosWatershed &other );

    //! Extents the delineating of this watershed to fit with the delineating of \a other
    void extentTo( const ReosWatershed &other );

    //! Returns the downstream line of the watersheds
    QPolygonF downstreamLine() const;

    //! Returns the stream path line of the watershed
    QPolygonF streamPath() const;

    //! Sets the stream path line of the watershed
    void setStreamPath( const QPolygonF &streamPath );

    //! Returns the residual watershed if exists, if not returns nullptr
    ReosWatershed *residualWatershed() const;

    //! Returns the longitudinale profile of the watershed
    QPolygonF profile() const;

    //! Sets the longitudinale profile of the watershed
    void setProfile( const QPolygonF &profile );

    void setGeographicalContext( ReosGisEngine *gisEngine );

    ReosParameterArea *area() const;
    ReosParameterSlope *slope() const;
    ReosParameterDouble *drop() const;
    ReosParameterDouble *longestPath() const;
    ReosParameterDouble *averageElevation() const;

    ReosParameterDuration *concentrationTime() const;
    ReosConcentrationTimeCalculation concentrationTimeCalculation() const;
    void setConcentrationTimeCalculation( const ReosConcentrationTimeCalculation &concentrationTimeCalculation );

    //! Creates a hydrograph from the \a rainfall, caller has to take ownership if \a hydrograph parent is not specified
    ReosHydrograph *createHydrograph( ReosSerieRainfall *rainfall, QObject *hydrographParent = nullptr );

    ReosEncodedElement encode() const;
    static ReosWatershed *decode( const ReosEncodedElement &element );

    bool operator==( const ReosWatershed &other ) const;

    ReosRunoffModelsGroup *runoffModels() const;

    ReosTransferFunction *currentTransferFunction() const;
    void setCurrentTransferFunction( const QString &type );

    bool usedConstantTimeStepForOutputHydrograph() const;
    void setUsedConstantTimeStepForOutputHydrograph( bool usedFixedTimeStepForOutputHydrograph );

    ReosDuration timeStepForOutputHydrograph() const;
    void setTimeStepForOutputHydrograph( const ReosDuration &timeStepForOutputHydrograph );

    //! Return a pointer to the gauged hydrographs store of this watershed
    ReosHydrographsStore *gaugedHydrographs() const;

  signals:
    void outletPositionChange();

  public slots:
    void calculateArea();

  private slots:
    void calculateSlope();
    void calculateLongerPath();
    void calculateDrop();
    void calculateConcentrationTime();
    void calculateAverageElevation();

  private:
    Type mType = None;

    ReosParameterString *mName;

    //! Hierarchical link in watershed tree
    std::vector<std::unique_ptr<ReosWatershed>> mUpstreamWatersheds;
    ReosWatershed *mDownstreamWatershed = nullptr;

    //! Geographics characteristic
    ReosMapExtent mExtent;
    QPolygonF mDelineating;
    QString mDelineatingReferenceLayer;
    QPointF mOutletPoint;
    QPolygonF mDownstreamLine;
    QPolygonF mStreamPath;
    QPolygonF mProfile;
    ReosGisEngine *mGisEngine = nullptr;

    //! Return mGisEngine or the one of the parent watershed if nullptr
    ReosGisEngine *geographicalContext() const;

    struct DirectionData
    {
      ReosRasterByteCompressed directionRaster;
      ReosRasterExtent directionExtent;
    };
    std::map<QString, DirectionData> mDirectionData;

    struct RasterizedWatershedData
    {
      ReosRasterByteCompressed rasterizedWatershed;
      ReosRasterExtent rasterizedWatershedExtent;
    };
    std::map<QString, RasterizedWatershedData> mRasterizedWatershedData;

    //! Geomorphological characteristic
    ReosParameterArea *mArea = nullptr;
    ReosParameterSlope *mSlope = nullptr;
    ReosParameterDouble *mDrop = nullptr;
    ReosParameterDouble *mLongestStreamPath = nullptr;
    ReosParameterDouble *mAverageElevation = nullptr;

    //! Hydrological data
    ReosConcentrationTimeCalculation mConcentrationTimeCalculation;
    ReosParameterDuration *mConcentrationTimeValue = nullptr;

    ReosRunoffModelsGroup *mRunoffModels;
    QMap<QString, ReosTransferFunction *> mTransferFunctions;
    QString mCurrentTransferFuntion;

    //!*********************

    ReosHydrographsStore *mGaugedHydrographs = nullptr;

    bool mUsedConstantTimeStepForOutputHydrograph = false;
    ReosDuration mTimeStepForOutputHydrograph;

    void init();
    void connectParameters();
    void updateResidual();
};

#endif // REOSWATERSHED_H
