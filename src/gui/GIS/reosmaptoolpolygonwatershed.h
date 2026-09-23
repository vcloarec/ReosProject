/***************************************************************************
                      reosmaptoolpolygonwatershed.h - ReosMapToolEditPolygonWatershed

 ---------------------
 begin                : 1.9.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMAPTOOLPOLYGONWATERSHED_H
#define REOSMAPTOOLPOLYGONWATERSHED_H

#include <QPointer>
#include <QPolygonF>
#include <QString>

#include "reosmaptool.h"

class ReosPolygonWatershed;
class ReosMapToolEditPolygonWatershed_p;
class ReosMapToolSelectPolygonWatershed_p;

class ReosMapToolEditPolygonWatershed : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolEditPolygonWatershed( ReosMap *map );

    void setPolygonWatersed( ReosPolygonWatershed *polygonWatershed );
    void setCurrentWatershedId( const QString &watershedId );

  private:
    QPointer<ReosMapToolEditPolygonWatershed_p> d;
    ReosMapTool_p *tool_p() const override;
};

class ReosMapToolSelectPolygonWatershed : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolSelectPolygonWatershed( ReosMap *map );
    void setPolygonWatersed( ReosPolygonWatershed *polygonWatershed );

  signals:
    void watershedFound( const QString &watershedId, const QPointF &position );

  private:
    QPointer<ReosMapToolSelectPolygonWatershed_p> d;
    ReosMapTool_p *tool_p() const override;
};

#endif // REOSMAPTOOLPOLYGONWATERSHED_H
