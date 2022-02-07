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

#include "reospolygonstructure.h"
#include "reospolylinesstructure_p.h"

class QgsVectorLayer;
class QgsCategorizedSymbolRenderer;

class ReosPolygonStructure_p : public ReosPolygonStructure, private ReosGeometryStructure_p
{
  public:
    ReosPolygonStructure_p( const QString &wktCrs );
    ~ReosPolygonStructure_p();

    QObject *data() override;

    void addPolygon( const QPolygonF &polygon, const QString &classId, const QString &sourceCrs ) override;
    int classIndex( const ReosSpatialPosition &position ) const override;
    QStringList classes() const override;
    ReosMapExtent extent( const QString &crs ) const override;

    QUndoStack *undoStack() const override;

  private:
    QStringList mClasses;
    QgsCategorizedSymbolRenderer *mRenderer;


    void addNewClass( const QString &classId );
};

#endif // REOSPOLYGONSTRUCTURE_P_H
