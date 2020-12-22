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
    ReosWatershed() = default;

    ReosWatershed( const QPolygonF &delineating,
                   const QPointF &outletPoint,
                   const QPolygonF &downstreamLine = QPolygonF(),
                   const ReosRasterWatershed::Directions &direction = ReosRasterWatershed::Directions(),
                   const ReosRasterExtent directionExent = ReosRasterExtent() );

    //! Returns the name of the watershed
    QString name() const;

    //! Sets the name of the watershed
    void setName( const QString &name );

    //! Returns the extent of the watershed
    ReosMapExtent extent() const;

    /**
     * Returns whether the watershed include the \a point
     *
     * \note if a point of the line is exactly on a segment of the delineating polygon, this point is considered outside
     */
    bool contain( const QPointF &point ) const;

    //! Returns how this watershed is included by \a other
    ReosInclusionType contain( const ReosWatershed &other ) const;

    /**
     * Returns how the polygon or polyline \a line is contained in the watershed
     *
     * \note if a point of the line is exactly on a segment of the delineating polygon, this point is considered outside
     */
    ReosInclusionType contain( const QPolygonF &line ) const;

    //! Returns whether the watrshed or its parent contoins direction data
    bool hasDirectiondata() const;

    //! Returns the directions raster associated with this watershed, returned raster is invalid if there is none
    ReosRasterWatershed::Directions directions() const;

    //! Returns the extent od the raster direction, \see directions()
    ReosRasterExtent directionExtent() const;

    //! Returns the delineating of the watershed
    QPolygonF delineating() const;

    //! Returns the outlet point of the watershed
    QPointF outletPoint() const;

    //! Returns the count if upstream watershed
    int upstreamWatershedCount() const;

    //! Returns the count of watershed that are directky upstream
    int directUpstreamWatershedCount() const;

    //! Returns the ith direct upstream watershed, return nullptr if there is not
    ReosWatershed *directUpstreamWatershed( int i ) const;

    /**
     * Adds a \a upstream watershed (take ownership) and return the downstram watershed where it was added.
     * If \a adaptUpstreamDelineating is true, the upstram watershed delineating will be adapted to fot with its directly
     * downstream watershed or its sibling. If false, a exception will be throwed if the new watershed delinetaing interset
     * downstream or siling watershed.
     */
    ReosWatershed *addUpstreamWatershed( ReosWatershed *upstreamWatershed );

    //! Returns the smallest sub watershed that is downstream the line, if the line is partially included by any watershed, ok is false and return nullptr
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *upstreamWatershed( const QPolygonF &line, bool &ok ) const;

    //! Returns the smallest sub watershed that contains the point
    const ReosWatershed *upstreamWatershed( const QPointF &point ) const;

    //! Returns, if exists a pointer to the direct downstream watershed, if not returns nullptr
    ReosWatershed *downstreamWatershed() const;

    //! Returns the position in the downstream watershed
    int positionInDownstreamWatershed() const;

    //! Returns a list of all upstream watershed
    QList<ReosWatershed *> allDownstreamWatershed() const;

    //! Removes direction data present in the watershed or in its children
    void removeDirectionData();

    //! Adjusts the delineating of this delineating to fit in the \a other watershed delineating (cut all that is outside the other and intersect with sub watershed)
    void fitIn( const ReosWatershed &other );

    //! Adjusts the delineating of this delineating to not intersect with the \a other watershed delineating (cut all that is inside the other)
    void adjust( const ReosWatershed &other );



  private:
    QString mName;
    ReosMapExtent mExtent;
    QPolygonF mDelineating;
    QPointF mOutletPoint;
    QPolygonF mDownstreamLine;
    ReosRasterByteCompressed mDirectionRaster;
    ReosRasterExtent mDirectionExtent;

    std::vector<std::unique_ptr<ReosWatershed>> mUpstreamWatersheds;
    ReosWatershed *mDownstreamWatershed = nullptr;
};

#endif // REOSWATERSHED_H
