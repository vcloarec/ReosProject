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
#include <qgssnapindicator.h>

#include <qobjectuniqueptr.h>

#include "reosmap.h"
#include "reosmenupopulator.h"
#include "reosmappolygon_p.h"


class ReosMapTool_p: public QgsMapTool
{
    Q_OBJECT
  public:
    ReosMapTool_p( QgsMapCanvas *canvas );
    void activate() override;
    void deactivate() override;
    bool populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent *e ) override;

    //! Sets context menu populator, take ownership
    void setContextMenuPopulator( ReosMenuPopulator *populator );

    //! Sets if the search of item is under only a point or under a zone arround the point
    void setSearchUnderPoint( bool underPoint );

    //! Sets the size of the search zone
    void setSearchZoneSize( const QSizeF &size );

    void setSearchTargetDescription( const QString &description );

    void setSeachWhenMoving( bool seachWhenMoving );

    void clearHoveredItem();

    void enableSnapping( bool enable );
    bool snappingEnabled() const;

    bool hasFeatureOnMap( const QPointF &mapPoint ) const;

    void setActivateMovingSignal( bool activateMovingSignal );

  signals:
    void foundItemWhenMoving( ReosMapItem_p *item );
    void move( const QPointF &point );
    void keyPressed( int key );

  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void keyPressEvent( QKeyEvent *e ) override;

    QString mapCrs() const;

    QRectF viewSearchZone( const QPoint &pt );
    ReosMapItem_p *searchItem( const QPointF &p ) const;
    QgsGeometry selectFeatureOnMap( QgsMapMouseEvent *e );
    double tolerance() const;

    ReosMapItem_p *mFoundItem = nullptr;
    std::unique_ptr<QgsSnapIndicator> mSnappingIndicator;
    bool mClosed = false;

  private:
    std::unique_ptr<ReosMenuPopulator> mContextMenuPopulator;
    QSizeF mSearchZone = QSizeF( 12, 12 );
    bool mSeachWhenMoving = false;
    bool mUnderPoint = false;
    QString mTargetDescritpion;
    bool mSnappingEnabled = false;
    bool mActivateMovingSignal = false;
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

  private:
    std::unique_ptr<QgsSnapIndicator> mSnapIndicator;
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

    void setColor( const QColor &color );
    void setFillColor( const QColor &color );

    void setAllowSelfIntersect( bool allowSelfIntersect );

  signals:
    void polylineDrawn( const QPolygonF &polyline ) const;

  private:
    QColor mColor;
    QColor mFillColor;
    bool mAllowSelfIntersect = true;

    bool selfIntersect() const;
    void updateColor();

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
    ReosMapToolSelectMapItem_p( QgsMapCanvas *map, const QString &targetDescription );

    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    bool populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent *event ) override;

    Flags flags() const override { return ShowContextMenu; }

  signals:
    void found( ReosMapItem *item, const QPointF &point );

  private:
    int mTargetType = -1;
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
    ~ReosMapToolMoveItem_p();
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

    bool isItemUnderPoint( const QPoint &p );
};


class ReosMapToolDrawHydraulicNetworkLink_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolDrawHydraulicNetworkLink_p( QgsMapCanvas *mapCanvas );

    void appendItem( ReosMapItem_p *item );

    QList<ReosMapItem_p *> mLinkedItems;
    QgsRubberBand *mRubberBand = nullptr;

  signals:
    void itemSelected( ReosMapItem_p *item );

  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent * ) override;
};


class ReosMapToolMoveHydraulicNetworkNode_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolMoveHydraulicNetworkNode_p( QgsMapCanvas *map ): ReosMapTool_p( map )
    {}

    void setCurrentItem( ReosMapItem_p *item );
  protected:
    void canvasPressEvent( QgsMapMouseEvent *event ) override
    {
      ReosMapItem_p *item = searchItem( event->localPos() );
      if ( item )
        mCurrentItem = item;
      else
        mCurrentItem = nullptr;
    }

    void canvasMoveEvent( QgsMapMouseEvent *e ) override
    {
      if ( mCurrentItem )
        emit itemMoved( mCurrentItem->base->description(), e->mapPoint().toQPointF() );
      else
        ReosMapTool_p::canvasMoveEvent( e );
    }

    void canvasReleaseEvent( QgsMapMouseEvent *e ) override
    {
      if ( mCurrentItem )
        emit itemMoved( mCurrentItem->base->description(), e->mapPoint().toQPointF() );

      mCurrentItem = nullptr;
    }
  signals:
    void itemMoved( const QString &decription, const QPointF &position );

  private:
    ReosMapItem_p *mCurrentItem;
};

#endif // REOSMAPTOOL_P_H
