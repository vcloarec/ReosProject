/***************************************************************************
  reospolygonstructure_p.h - ReosPolygonStructure_p

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
#ifndef REOSPOLYGONSTRUCTURE_P_H
#define REOSPOLYGONSTRUCTURE_P_H

#include <QVariantMap>

#include "reospolygonstructure.h"
#include "reospolylinesstructure_p.h"

class QgsVectorLayer;
class QgsCategorizedSymbolRenderer;

class ReosPolygonStructure_p : public ReosPolygonStructure, private ReosGeometryStructure_p
{
    Q_OBJECT
  public:
    ReosPolygonStructure_p( const QString &wktCrs );
    ~ReosPolygonStructure_p();

    QObject *data() override;
    double value( const ReosSpatialPosition &position, bool acceptClose = false ) const override;
    void addPolygon( const QPolygonF &polygon, const QString &classId, const QString &sourceCrs ) override;
    QStringList classes() const override;
    void addClass( const QString &classId, double value ) override;
    ReosMapExtent extent( const QString &crs ) const override;
    QColor color( const QString &classId ) const override;
    double value( const QString &classId ) const override;

    QUndoStack *undoStack() const override;

  private:
    QVariantMap mClasses;
    QgsCategorizedSymbolRenderer *mRenderer;

    QColor symbolColor( QgsSymbol *sym ) const;

    double mTolerance = 0.01;

};

#endif // REOSPOLYGONSTRUCTURE_P_H
