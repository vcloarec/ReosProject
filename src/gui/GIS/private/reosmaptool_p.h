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
#include "reosmappolygon_p.h"


class ReosMapTool_p: public QgsMapTool
{
  public:
    ReosMapTool_p( QgsMapCanvas *canvas );
    void activate() override;
    void deactivate() override;
    bool populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent *event ) override;

    //! Sets context menu populator, take ownership
    void setContextMenuPopulator( ReosMenuPopulator *populator );

  protected:
    QRectF viewSearchZone( const QPoint &pt );

  private:
    std::unique_ptr<ReosMenuPopulator> mContextMenuPopulator;

    QSize mSearchZone = QSize( 18, 18 );
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

    void drawExtent();

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
    int mMovingVertex = -1;
    bool mIsEdited = false;
};

class ReosMapToolMoveItem_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolMoveItem_p( QgsMapCanvas *map );
    void setCurrentItem( ReosMapItem_p *item );
    void setMovingColor( const QColor &movingColor );

  signals:
    void itemMoved( ReosMapItem *item );

  protected:
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent * ) override;

  private:
    ReosMapItem_p *mCurrentItem = nullptr;
    std::unique_ptr<ReosMapItem_p> mMovingItem;
    bool mIsMoving = false;
    QPointF mStartPoint;
    QColor mMovingColor;

    bool searchItem( const QPoint &p );

};

#endif // REOSMAPTOOL_P_H
