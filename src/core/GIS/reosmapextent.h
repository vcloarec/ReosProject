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

/**
 * Class that represent a rectangular extent in a map
 */
class ReosMapExtent
{
  public:
    ReosMapExtent() = default;
    ReosMapExtent( QRectF extent );
    ReosMapExtent( double xMapMin, double yMapMin, double xMapMax, double yMapMax );

    double width() const;
    double height()const;

    double xMapMin() const;
    double xMapMax() const;
    double yMapMin() const;
    double yMapMax() const;

    bool inExtent( const QPointF &point ) const;

    bool operator==( const ReosMapExtent &other ) const;

    ReosMapExtent operator*( const ReosMapExtent &other ) const;

    QString crs() const;
    void setCrs( const QString &crs );

    bool contains( const QPointF &point ) const;

    //! Return true if the extent cotains, even partially, the \a line
    bool containsPartialy( const  QPolygonF &line ) const;

  protected:
    double mXMin = std::numeric_limits<double>::quiet_NaN();
    double mXMax = std::numeric_limits<double>::quiet_NaN();
    double mYMin = std::numeric_limits<double>::quiet_NaN();
    double mYMax = std::numeric_limits<double>::quiet_NaN();

    QString mCrs;
};

#endif // REOSMAPEXTENT_H
