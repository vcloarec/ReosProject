/***************************************************************************
  reosmaptooleditgeometrystructure.h - ReosMapToolEditGeometryStructure

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
#ifndef REOSMAPTOOLEDITGEOMETRYSTRUCTURE_H
#define REOSMAPTOOLEDITGEOMETRYSTRUCTURE_H

#include "reosmaptool.h"

class QActionGroup;

class ReosMapToolEditPolylineStructure_p;
class ReosMapToolEditPolygonStructure_p;
class ReosGeometryStructure;
class ReosPolygonStructure;


class ReosMapToolEditPolylineStructure : public ReosMapTool
{
  public:

    ReosMapToolEditPolylineStructure( ReosPolylinesStructure *structure, QObject *parent, ReosMap *map );
    ~ReosMapToolEditPolylineStructure();

    QActionGroup *mainActions() const;

  private:
    QPointer<ReosMapToolEditPolylineStructure_p> d;
    ReosMapTool_p *tool_p() const;

};

class ReosMapToolEditPolygonStructure : public ReosMapTool
{
  public:

    ReosMapToolEditPolygonStructure( ReosPolygonStructure *structure, QObject *parent, ReosMap *map );
    ~ReosMapToolEditPolygonStructure();

    QActionGroup *mainActions() const;

  private:
    QPointer<ReosMapToolEditPolygonStructure_p> d;
    ReosMapTool_p *tool_p() const;

};

#endif // REOSMAPTOOLEDITGEOMETRYSTRUCTURE_H
