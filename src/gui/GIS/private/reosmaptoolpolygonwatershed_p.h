/***************************************************************************
  reosmaptoolpolygonwatershed_p.h - ReosMapToolEditPolygonWatershed_p

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

#ifndef REOSMAPTOOLPOLYGONWATERSHED_P_H
#define REOSMAPTOOLPOLYGONWATERSHED_P_H


#include "reosmaptool_p.h"

class QgsMapCanvas;
class QgsRubberBand;
class QgsVertexMarker;
class ReosPolygonWatershed;


class ReosMapToolEditPolygonWatershed_p: public ReosMapTool_p
{
public:
  ReosMapToolEditPolygonWatershed_p( QgsMapCanvas *mapCanvas );

  void setPolygonWatershed( ReosPolygonWatershed *polygonWatershed );
  void setCurrentWatershed( const QString &watershedId );

protected:
  void canvasMoveEvent( QgsMapMouseEvent *e ) override;
  void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
  void keyPressEvent( QKeyEvent *e ) override;

private:
  enum State
  {
    None,
    DraggingVertex,
    InsertingVertex
  };
  State mCurrentState = None;
  QgsRubberBand *mRubberBand = nullptr;
  QgsVertexMarker *mMarkerVertex = nullptr;
  int mPrevVertexIndex = -1;
  int mCurrentSelectedVertexIndex = -1;
  int mNextVertexIndex = -1;

  ReosPolygonWatershed *mPolygonWatershed = nullptr;
  QString mCurrentWatershedId;

  void reset();
};

class ReosMapToolSelectPolygonWatershed_p: public ReosMapTool_p
{
  Q_OBJECT
public:
  ReosMapToolSelectPolygonWatershed_p( QgsMapCanvas *mapCanvas );
  void setPolygonWatershed( ReosPolygonWatershed *polygonWatershed );

signals:
  void watershedFound(const QString &watershedId, const QPointF &position);

protected:
  void canvasReleaseEvent( QgsMapMouseEvent *e ) override;

private:
  ReosPolygonWatershed *mPolygonWatershed=nullptr;
};

#endif // REOSMAPTOOLPOLYGONWATERSHED_P_H
