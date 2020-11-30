/***************************************************************************
                      reosmaptool_p.h
                     --------------------------------------
Date                 : October-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSMAPTOOL_P_H
#define REOSMAPTOOL_P_H

#include <QPointer>

#include <qgsmapmouseevent.h>
#include <qgsmapcanvas.h>
#include <qgsmaptool.h>
#include <qgsrubberband.h>
#include <qobjectuniqueptr.h>

#include "reosmap.h"


class ReosMapTool_p: public QgsMapTool
{
  public:
    ReosMapTool_p( QgsMapCanvas *canvas );
    void activate();
    void deactivate();

  protected:
    void keyPressEvent( QKeyEvent *e );
};

class ReosMapToolDrawPolyline_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyline_p( QgsMapCanvas *map );
    ~ReosMapToolDrawPolyline_p();

    void deactivate() override;

    QPointer<QgsRubberBand> mRubberBand;

    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;

  signals:
    void polylineDrawn( const QPolygonF &polyline ) const;


};

class ReosMapToolDrawExtent_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolDrawExtent_p( QgsMapCanvas *map );
    ~ReosMapToolDrawExtent_p();

    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    void deactivate() override;

    QPointer<QgsRubberBand> mRubberBand;

  signals:
    void extentDrawn( const QRectF &rectangularExtent );

  private:

    bool mIsDrawing = false;

    QgsPointXY mStartPoint;
    QgsPointXY mEndPoint;

    void drawExtent()
    {
      QgsRectangle rect( mStartPoint, mEndPoint );

      mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
      mRubberBand->addPoint( QgsPointXY( rect.xMinimum(), rect.yMinimum() ), false );
      mRubberBand->addPoint( QgsPointXY( rect.xMaximum(), rect.yMinimum() ), false );
      mRubberBand->addPoint( QgsPointXY( rect.xMaximum(), rect.yMaximum() ), false );
      mRubberBand->addPoint( QgsPointXY( rect.xMinimum(), rect.yMaximum() ), true );
    }

};

#endif // REOSMAPTOOL_P_H
