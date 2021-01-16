#ifndef REOSWATERSHED_H
#define REOSWATERSHED_H

#include "reosmapextent.h"
#include "reosrastercompressed.h"
#include "reosrasterwatershed.h"
#include "reosexception.h"
#include "memory"

#include "reosgeometryutils.h"
#include "reosrasterwatershed.h"


class ReosWatershed
{
  public:
    enum Type
    {
      None,
      Automatic,
      Manual,
      Residual
    };

    ReosWatershed() = default;

    ReosWatershed( const QPolygonF &delineating,
                   const QPointF &outletPoint,
                   Type type,
                   const QPolygonF &downstreamLine = QPolygonF(),
                   const QPolygonF &streamPath = QPolygonF(),
                   const ReosRasterWatershed::Directions &direction = ReosRasterWatershed::Directions(),
                   const ReosRasterExtent &directionExent = ReosRasterExtent() );

    Type type() const {return mType;}

    //! Returns the name of the watershed
    QString name() const;

    //! Sets the name of the watershed
    void setName( const QString &name );

    //! Returns the extent of the watershed
    ReosMapExtent extent() const;

    //! Returns the delineating of the watershed
    QPolygonF delineating() const;

    //! Returns the outlet point of the watershed
    QPointF outletPoint() const;

    //! Returns whether the watrshed or its parent contoins direction data
    bool hasDirectiondata() const;

    //! Returns the directions raster associated with this watershed, returned raster is invalid if there is none
    ReosRasterWatershed::Directions directions() const;

    //! Returns the extent of the raster direction, \see directions()
    ReosRasterExtent directionExtent() const;

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
    ReosWatershed *upstreamWatershed( const QPointF &point );

    //! Returns, if exists, a pointer to the direct downstream watershed, if not returns nullptr
    ReosWatershed *downstreamWatershed() const;

    //! Returns the position in the downstream watershed
    int positionInDownstreamWatershed() const;

    //! Returns a list of all upstream watershed
    QList<ReosWatershed *> allUpstreamWatershed() const;

    //! Cuts the delineating of this delineating to fit in the \a other watershed delineating (cut all that is outside the other and intersect with sub watershed)
    void fitIn( const ReosWatershed &other );

    //! Adjusts the delineating of this watershed to not intersect with the \a other watershed delineating (cut all that is inside the other)
    void adjust( const ReosWatershed &other );

    //! Extents the delineating of this watershed to fit with the delineating of \a other
    void extentTo( const ReosWatershed &other );

    //! Returns the stream path line of the watershed
    QPolygonF streamPath() const;

    //! Returns the residual watershed if exists, if not returns nullptr
    ReosWatershed *residualWatershed() const;

    //! Returns the longitudinale profile of the watershed
    QPolygonF profile() const;

    //! Sets the longitudinale profile of the watershed
    void setProfile( const QPolygonF &profile );

  private:
    Type mType = None;
    QString mName;
    ReosMapExtent mExtent;
    QPolygonF mDelineating;
    QPointF mOutletPoint;
    QPolygonF mDownstreamLine;
    ReosRasterByteCompressed mDirectionRaster;
    ReosRasterExtent mDirectionExtent;
    QPolygonF mStreamPath;

    QPolygonF mProfile;

    std::vector<std::unique_ptr<ReosWatershed>> mUpstreamWatersheds;
    ReosWatershed *mDownstreamWatershed = nullptr;

    void updateResidual();
};

#endif // REOSWATERSHED_H
