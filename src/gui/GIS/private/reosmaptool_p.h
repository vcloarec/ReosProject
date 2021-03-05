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
#include "reosmenupopulator.h"


class ReosMapTool_p: public QgsMapTool
{
  public:
    ReosMapTool_p( QgsMapCanvas *canvas );
    void activate() override;
    void deactivate() override;
    bool populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent *event ) override;

    //! Sets context menu populator, take ownership
    void setContextMenuPopulator( ReosMenuPopulator *populator );

  private:
    std::unique_ptr<ReosMenuPopulator> mContextMenuPopulator;
};

class ReosMapToolDrawPoint_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolDrawPoint_p( QgsMapCanvas *map );
    ~ReosMapToolDrawPoint_p();

    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;

  signals:
    void pointDrawn( const QPointF &point ) const;
};

class ReosMapToolDrawPolyline_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyline_p( QgsMapCanvas *map, bool closed = false );
    ~ReosMapToolDrawPolyline_p();

    void deactivate() override;
    QPointer<QgsRubberBand> mRubberBand;

    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;

  signals:
    void polylineDrawn( const QPolygonF &polyline ) const;

  private:
    bool mClosed = false;

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

class ReosMapToolSelectMapItem_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolSelectMapItem_p( QgsMapCanvas *map, int targetType = -1 );
    ReosMapToolSelectMapItem_p( QgsMapCanvas *map, const QString &targetDescription );

    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    bool populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent *event ) override;

    void setSearchUnderPoint( bool underPoint );

    Flags flags() const override { return ShowContextMenu; }

  signals:
    void found( ReosMapItem *item, const QPointF &point );

  private:
    int mTargetType = -1;
    QString mTargetDescritpion;
    bool mUnderPoint = false;
};

class ReosMapToolEditPolygon_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolEditPolygon_p( QgsMapCanvas *map );
    void setMapPolygon( ReosMapPolygon_p *polygon );

    void activate() override;
    void deactivate() override;

    Flags flags() const override { return ShowContextMenu; }

    bool populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event ) override;

  protected:
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent * ) override;

  signals:
    void polygonEdited();

  private:
    ReosMapPolygon_p *mPolygon = nullptr;
    QSize mSearchZone = QSize( 18, 18 );
    int mMovingVertex = -1;
    bool mIsEdited = false;

    QgsRectangle mapSearchZone( const QPoint &pt );
    QRectF viewSearchZone( const QPoint &pt );
};

#endif // REOSMAPTOOL_P_H
