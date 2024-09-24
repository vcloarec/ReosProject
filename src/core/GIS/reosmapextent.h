/***************************************************************************
                      reosmapextent.h
                     --------------------------------------
Date                 : 01-10-2020
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

#ifndef REOSMAPEXTENT_H
#define REOSMAPEXTENT_H


#include <QRectF>
#include <QString>
#include <QPolygon>
#include <QMetaType>

#include "reosencodedelement.h"

class REOSCORE_EXPORT ReosSpatialPosition
{
  public:
    ReosSpatialPosition() = default;
    explicit ReosSpatialPosition( const QPointF &position, const QString &crs = QString() );
    ReosSpatialPosition( double x, double y, const QString &crs = QString() );

    QPointF position() const;
    QString crs() const;

    bool isValid() const;

    static ReosSpatialPosition decode( const ReosEncodedElement &element );
    ReosEncodedElement encode() const;

  private:
    QPointF mPosition;
    QString mCrs;
    bool mIsValid = false;
};

Q_DECLARE_METATYPE( ReosSpatialPosition )

/**
 * Class that represent a rectangular extent in a map
 */
class REOSCORE_EXPORT ReosMapExtent
{
  public:

    //! Default constrcutor
    ReosMapExtent() = default;

    //! Constructor with a rectangel \a extent, the crs is invalid (see setCrs())
    ReosMapExtent( const QRectF &extent );

    //! Constructor with the coordinates of the foor corners, the crs is invalid (see setCrs())
    ReosMapExtent( double xMapMin, double yMapMin, double xMapMax, double yMapMax );

    //! Construct an extent with the bounding box of the polygon, the crs is invalid (see setCrs())
    ReosMapExtent( const QPolygonF &polygon, const QString &crs = QString() );

    /**
     * Constructor with two spatial positions \a pos1 and \a pos2, the extent contains this two position and
     * has the crs of \a pos1
     */
    ReosMapExtent( const ReosSpatialPosition &pos1, const ReosSpatialPosition &pos2 );

    //! Returns the width of the extent
    double width() const;

    //! Returns the height of the extent
    double height()const;

    //! Returns the x min
    double xMapMin() const;

    //! Returns the x max
    double xMapMax() const;

    //! Returns the y min
    double yMapMin() const;

    //! Returns the y max
    double yMapMax() const;

    //! Returns true if the extent contain the point
    bool contains( const QPointF &point ) const;

    //! Return true if the extent cotains, even partially, the \a line
    bool containsPartialy( const  QPolygonF &line ) const;

    //! Extends the extent by adding a point
    void addPointToExtent( const QPointF &pt );

    //! Extends the extent combining it with \a other extent.
    void extendWithExtent( const ReosMapExtent &other );

    //! Extents the extent with a \a buffer distance.
    void extendByBuffer( double buffer );

    //! Returns the WKT coordinates system of this extent
    QString crs() const;

    //! Sets the WKT coordinates system if this extent
    void setCrs( const QString &crs );

    //! Returns a rectangular polygon corresponding to this extent
    QPolygonF toPolygon() const;

    //! Returns a rectangle corresponding to this extent
    QRectF toRectF() const;

    //! Returns whether this extent is valid
    bool isValid() const;

    bool operator==( const ReosMapExtent &other ) const;
    bool operator!=( const ReosMapExtent &other ) const;

    ReosMapExtent operator*( const ReosMapExtent &other ) const;

#ifndef SIP_RUN
    static ReosMapExtent decode( const ReosEncodedElement &element );
    ReosEncodedElement encode() const;

  protected:
    double mXMin = std::numeric_limits<double>::max();
    double mXMax = -std::numeric_limits<double>::max();
    double mYMin = std::numeric_limits<double>::max();
    double mYMax = -std::numeric_limits<double>::max();

    QString mCrs;

#endif //ifndef SIP_RUN
};

#endif // REOSMAPEXTENT_H
