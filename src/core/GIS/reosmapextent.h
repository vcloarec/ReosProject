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
    ReosSpatialPosition( const QPointF &position, const QString &crs = QString() );

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
    ReosMapExtent() = default;
    ReosMapExtent( const QRectF &extent );
    ReosMapExtent( double xMapMin, double yMapMin, double xMapMax, double yMapMax );
    //! Construct an extent with the bounding box of the polygon
    ReosMapExtent( const QPolygonF &polygon );

    double width() const;
    double height()const;

    double xMapMin() const;
    double xMapMax() const;
    double yMapMin() const;
    double yMapMax() const;

    //! Returns true if the extent contain the point
    bool contains( const QPointF &point ) const;
    //! Return true if the extent cotains, even partially, the \a line
    bool containsPartialy( const  QPolygonF &line ) const;

    void addPointToExtent( const QPointF &pt );

    QString crs() const;
    void setCrs( const QString &crs );

    bool operator==( const ReosMapExtent &other ) const;
    bool operator!=( const ReosMapExtent &other ) const;

    ReosMapExtent operator*( const ReosMapExtent &other ) const;

    QPolygonF toPolygon() const;
    QRectF toRectF() const;

    static ReosMapExtent decode( const ReosEncodedElement &element );
    ReosEncodedElement encode() const;

  protected:
    double mXMin = std::numeric_limits<double>::max();
    double mXMax = -std::numeric_limits<double>::max();
    double mYMin = std::numeric_limits<double>::max();
    double mYMax = -std::numeric_limits<double>::max();


    QString mCrs;
};

#endif // REOSMAPEXTENT_H
