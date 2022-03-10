/***************************************************************************
  reosmaptooleditmeshframe.h - ReosMapToolEditMeshFrame

 ---------------------
 begin                : 9.3.2022
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
#ifndef REOSMAPTOOLEDITMESHFRAME_H
#define REOSMAPTOOLEDITMESHFRAME_H

#include "reosmaptool.h"

class ReosMesh;
class ReosMapToolEditMeshFrame_p;

class ReosMapToolEditMeshFrame : public ReosMapTool
{
  public:
    ReosMapToolEditMeshFrame( ReosMesh *mesh, QObject *parent, ReosMap *map );
    ~ReosMapToolEditMeshFrame();

    QActionGroup *mainActions() const;

  private:
    ReosMapTool_p *tool_p() const;
    QPointer<ReosMapToolEditMeshFrame_p> d;

};

#endif // REOSMAPTOOLEDITMESHFRAME_H
