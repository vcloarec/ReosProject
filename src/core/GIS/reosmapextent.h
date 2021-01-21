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

#include "reosencodedelement.h"

/**
 * Class that represent a rectangular extent in a map
 */
class ReosMapExtent
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

    void addPointToExtent( const QPointF &pt )
    {
      if ( mXMin > pt.x() )
        mXMin = pt.x();
      if ( mXMax < pt.x() )
        mXMax = pt.x();
      if ( mYMin > pt.y() )
        mYMin = pt.y();
      if ( mYMax < pt.y() )
        mYMax = pt.y();
    }

    QString crs() const;
    void setCrs( const QString &crs );

    bool operator==( const ReosMapExtent &other ) const;
    bool operator!=( const ReosMapExtent &other ) const;

    ReosMapExtent operator*( const ReosMapExtent &other ) const;

    QPolygonF toPolygon() const;

    static ReosMapExtent decode( const ReosEncodedElement &element )
    {
      if ( element.description() == QStringLiteral( "map-extent" ) )
      {
        double xMin;
        if ( !element.getData( QStringLiteral( "xmin" ), xMin ) )
          return ReosMapExtent();
        double xMax;
        if ( !element.getData( QStringLiteral( "xmax" ), xMax ) )
          return ReosMapExtent();
        double yMin;
        if ( !element.getData( QStringLiteral( "ymin" ), yMin ) )
          return ReosMapExtent();
        double yMax;
        if ( !element.getData( QStringLiteral( "ymax" ), yMax ) )
          return ReosMapExtent();
        QString crs;
        if ( !element.getData( QStringLiteral( "crs" ), crs ) )
          return ReosMapExtent();

        ReosMapExtent ret( xMin, yMin, xMax, yMax );
        ret.setCrs( crs );

        return ret;
      }
      else
      {

      }
    }

    ReosEncodedElement encode() const
    {
      ReosEncodedElement ret( QStringLiteral( "map-extent" ) );

      ret.addData( QStringLiteral( "xmin" ), mXMin );
      ret.addData( QStringLiteral( "xmax" ), mXMax );
      ret.addData( QStringLiteral( "ymin" ), mYMin );
      ret.addData( QStringLiteral( "ymax" ), mYMax );
      ret.addData( QStringLiteral( "crs" ), mCrs );

      return ret;
    }

  protected:
    double mXMin = std::numeric_limits<double>::max();
    double mXMax = -std::numeric_limits<double>::max();
    double mYMin = std::numeric_limits<double>::max();
    double mYMax = -std::numeric_limits<double>::max();


    QString mCrs;
};

#endif // REOSMAPEXTENT_H
