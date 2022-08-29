/***************************************************************************
                      reosmaptool.h
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

#ifndef REOSMAPTOOL_H
#define REOSMAPTOOL_H

#include <memory>

#include "reosgui.h"
#include "reosmap.h"

class ReosMapToolEditPolygon_p;
class ReosMapToolMoveItem_p;
class ReosMapToolDrawPoint_p;
class ReosMapToolDrawPolyline_p;
class ReosMapToolDrawExtent_p;
class ReosMapToolSelectMapItem_p;
class ReosMapTool_p;
class ReosMenuPopulator;

class REOSGUI_EXPORT ReosMapTool : public QObject
{
    Q_OBJECT
  public:
    virtual ~ReosMapTool();

    void activate();
    void deactivate();

    bool isActive() const;

    void setCurrentToolInMap() const;

    //! Set default map tool for map
    void quitMap();

    bool isCurrentToolInMap() const;

    void setAction( QAction *action );

    void setCursor( const QCursor &cursor );

    //! Sets context menu populator, take ownership
    void setContextMenuPopulator( ReosMenuPopulator *populator );

    //! Sets whether item have to be search when moving the map tool
    void setSearchItemWhenMoving( bool b );

    //! Sets a description of the item element that have to be search
    void setSearchingItemDecription( const QString &description );

    void activateMovingSignal( bool activate );

    void enableSnapping( bool enable );

    QString crs() const;
  signals:
    void itemFoundWhenMoving( ReosMapItem *item );
    void move( const QPointF &point );
    void activated();
    void deactivated();

  protected:
    ReosMapTool( QObject *parent, ReosMap *map );
    void setUp();

    ReosMap *map() const;

  protected slots:
    virtual void keyPressed( int key );

  private:
    virtual ReosMapTool_p *tool_p() const = 0;
    QPointer<ReosMap> mMap = nullptr;
};

class ReosMapToolNeutral: public ReosMapTool
{
  public:
    ReosMapToolNeutral( ReosMap *map );
    ReosMapToolNeutral( QObject *parent, ReosMap *map );
    ~ReosMapToolNeutral();

  private:
    QPointer<ReosMapTool_p> d;
    ReosMapTool_p *tool_p() const;
};


class ReosMapToolDrawPoint: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawPoint( ReosMap *map );
    ReosMapToolDrawPoint( QObject *parent, ReosMap *map );
    ~ReosMapToolDrawPoint();

  signals:
    void drawn( const QPointF &point );

  private:
    QPointer<ReosMapToolDrawPoint_p> d;
    ReosMapTool_p *tool_p() const override;
};


class REOSGUI_EXPORT ReosMapToolDrawPolyRubberBand : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyRubberBand( ReosMap *map, bool closed );
    ReosMapToolDrawPolyRubberBand( QObject *parent, ReosMap *map, bool closed );
    ~ReosMapToolDrawPolyRubberBand();

    void setStrokeWidth( double width );
    void setColor( const QColor &color );
    void setSecondaryStrokeColor( const QColor &color );
    void setLineStyle( Qt::PenStyle style );

    void setAllowSelfIntersect( bool b );

  protected:
    QPointer<ReosMapToolDrawPolyline_p> d;
  private:
    ReosMapTool_p *tool_p() const override;
};

class REOSGUI_EXPORT ReosMapToolDrawPolyline : public ReosMapToolDrawPolyRubberBand
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyline( ReosMap *map );
    ReosMapToolDrawPolyline( QObject *parent, ReosMap *map );

  signals:
    void drawn( const QPolygonF &polyline ) const;
};

class ReosMapToolDrawPolygon : public ReosMapToolDrawPolyRubberBand
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolygon( ReosMap *map );
    ReosMapToolDrawPolygon( QObject *parent, ReosMap *map );

    void setFillColor( const QColor &color );

  signals:
    void drawn( const QPolygonF &polygon ) const;
};

class ReosMapToolDrawExtent: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawExtent( ReosMap *map );
    ReosMapToolDrawExtent( QObject *parent, ReosMap *map );
    ~ReosMapToolDrawExtent();

    void setStrokeWidth( double width );
    void setColor( const QColor &color );
    void setSecondaryStrokeColor( const QColor &color );
    void setFillColor( const QColor &color );
    void setLineStyle( Qt::PenStyle style );

  signals:
    void extentDrawn( const QRectF &extent );

  private:
    QPointer<ReosMapToolDrawExtent_p> d;
    ReosMapTool_p *tool_p() const override;
};

class REOSGUI_EXPORT ReosMapToolSelectMapItem : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolSelectMapItem( ReosMap *map, const QString &targetDescription );
    ReosMapToolSelectMapItem( QObject *parent, ReosMap *map, const QString &targetDescription );
    ~ReosMapToolSelectMapItem();

    void setSearchUnderPoint( bool b );

    void clearHoveredItem();

  signals:
    void found( ReosMapItem *item, const QPointF &point );

  private:
    QPointer<ReosMapToolSelectMapItem_p> d;
    ReosMapTool_p *tool_p() const;
};

class ReosMapToolEditMapPolyline : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolEditMapPolyline( ReosMap *map );
    ReosMapToolEditMapPolyline( QObject *parent, ReosMap *map );
    ~ReosMapToolEditMapPolyline();

    //! Sets the map \a polyline to edit
    void setMapPolyline( ReosMapPolyline *polyline );

  signals:
    void polylineEdited();
  private:
    QPointer<ReosMapToolEditPolygon_p> d;
    ReosMapTool_p *tool_p() const;
};

class ReosMapToolEditMapPolygon : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolEditMapPolygon( ReosMap *map );
    ReosMapToolEditMapPolygon( QObject *parent, ReosMap *map );
    ~ReosMapToolEditMapPolygon();

    //! Sets the map \a polyline to edit
    void setMapPolygon( ReosMapPolygon *polygon );

  signals:
    void polygonEdited();
  private:
    QPointer<ReosMapToolEditPolygon_p> d;
    ReosMapTool_p *tool_p() const;
};

class ReosMapToolMoveMapItem: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolMoveMapItem( ReosMap *map );
    ReosMapToolMoveMapItem( QObject *parent, ReosMap *map );
    ~ReosMapToolMoveMapItem();

    void setCurrentMapItem( ReosMapItem *item );
    void setMovingColor( const QColor &movingColor );

  signals:
    void itemMoved( ReosMapItem *item );

  private:
    QPointer<ReosMapToolMoveItem_p> d;
    ReosMapTool_p *tool_p() const;
};


#endif // REOSMAPTOOL_H
