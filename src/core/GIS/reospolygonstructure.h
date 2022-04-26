/***************************************************************************
  reospolygonstructure.h - ReosPolygonStructure

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
#ifndef REOSPOLYGONSTRUCTURE_H
#define REOSPOLYGONSTRUCTURE_H

#include <memory>

#include "reosgeometrystructure.h"
#include "reosmapextent.h"


class QUndoStack;

class ReosPolygonStructureValues
{
  public:
    virtual ~ReosPolygonStructureValues() {}

    virtual double value( double x, double y, bool acceptClose = false ) const = 0;

};

class REOSCORE_EXPORT ReosPolygonStructure : public ReosGeometryStructure
{
    Q_OBJECT
  public:

    virtual ReosPolygonStructure *clone() const = 0;

    //! Creates and returns polylines structure with specified \a crs
    static std::unique_ptr<ReosPolygonStructure> createPolygonStructure( const QString &crs = QString() );

    static std::unique_ptr<ReosPolygonStructure> createPolygonStructure( const ReosEncodedElement &encodedElement );

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

    virtual ReosPolygonStructureValues *values( const QString &destinationCrs ) const = 0;

    QPolygonF searchPolygon( const ReosSpatialPosition &, bool  = true ) const override {return QPolygonF();}

  signals:
    void classesChanged();

};

#endif // REOSPOLYGONSTRUCTURE_H
