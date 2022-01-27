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
class ReosGeometryStructure;

class ReosMapToolEditGeometryStructure : public ReosMapTool
{
  public:

    ReosMapToolEditGeometryStructure( ReosPolylinesStructure *structure, QObject *parent, ReosMap *map );
    ~ReosMapToolEditGeometryStructure();
    void setStructure( ReosPolylinesStructure *structure );

    QActionGroup *mainActions() const;

  private:
    QPointer<ReosMapToolEditPolylineStructure_p> d;
    ReosMapTool_p *tool_p() const;

};

#endif // REOSMAPTOOLEDITGEOMETRYSTRUCTURE_H
