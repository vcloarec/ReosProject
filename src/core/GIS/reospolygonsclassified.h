/***************************************************************************
  reospolygonsclassified.h - ReosPolygonsClassified

 ---------------------
 begin                : 5.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSPOLYGONSCLASSIFIED_H
#define REOSPOLYGONSCLASSIFIED_H

#define SIP_NO_FILE

#include <memory>

#include "reosgeometrycomplex.h"
#include "reosmapextent.h"


class QUndoStack;

class ReosPolygonsClassifiedValues
{
  public:
    virtual ~ReosPolygonsClassifiedValues() {}

    virtual double value( double x, double y, bool acceptClose = false ) const = 0;
    virtual void setDefaultValue( double defVal ) = 0;
    virtual double defaultValue() const = 0;
};

/**
 * Class ReosPolygonsClassified
 * Represents a set of polygons associated with associated classes and values.
 */
class REOSCORE_EXPORT ReosPolygonsClassified : public ReosGeometryComplex
{
    Q_OBJECT
  public:
    virtual ReosPolygonsClassified *clone() const = 0;

    //! Creates and returns polylines structure with specified \a crs
    static std::unique_ptr<ReosPolygonsClassified> createPolygonStructure( const QString &crs = QString() );

    static std::unique_ptr<ReosPolygonsClassified> createPolygonStructure( const ReosEncodedElement &encodedElement );

    virtual void addPolygon( const QPolygonF &polygon, const QString &classId, const QString &sourceCrs = QString() ) = 0;
    virtual void addClass( const QString &classId, double value ) = 0;
    virtual void removeClass( const QString &classId ) = 0;
    virtual QStringList classes() const = 0;
    virtual QColor color( const QString &classId ) const = 0;
    virtual double value( const QString &classId ) const = 0;
    virtual QString valueToClass( double value ) const = 0;

    virtual int polygonsCount() const = 0;

    virtual QUndoStack *undoStack() const = 0;

    virtual ReosEncodedElement encode() const = 0;

    virtual ReosPolygonsClassifiedValues *values( const QString &destinationCrs ) const = 0;

    QPolygonF searchPolygon( const ReosSpatialPosition &, bool = true ) const override { return QPolygonF(); }

  signals:
    void classesChanged();
};

#endif // REOSPOLYGONSCLASSIFIED_H
