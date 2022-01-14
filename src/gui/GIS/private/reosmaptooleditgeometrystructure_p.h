/***************************************************************************
  reosmaptooleditgeometrustructure_p.h - ReosMapToolEditGeometruStructure_p

 ---------------------
 begin                : 12.1.2022
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
#ifndef REOSMAPTOOLEDITGEOMETRYSTRUCTURE_P_H
#define REOSMAPTOOLEDITGEOMETRYSTRUCTURE_P_H

#include "reosmaptool_p.h"

class ReosGeometryStructureVertex;

class ReosMapToolEditPolylineStructure_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolEditPolylineStructure_p( QgsMapCanvas *map );

    void setStructure( ReosPolylinesStructure *structure );


  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;

  private:
    ReosPolylinesStructure *mStructure;
    QgsVertexMarker *mVertexMarker;

    double mTolerance = 0;
    QString mMapCrs;
    ReosGeometryStructureVertex *mCurrentVertex = nullptr;

    ReosMapExtent searchZone( const QgsPointXY &point ) const;

};

#endif // REOSMAPTOOLEDITGEOMETRYSTRUCTURE_P_H
