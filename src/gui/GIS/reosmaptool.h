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
#include "reosmap.h"
#include <QPointer>

class ReosMapToolDrawPoint_p;
class ReosMapToolDrawPolyline_p;
class ReosMapToolDrawExtent_p;
class ReosMapToolSelectMapItem_p;
class ReosMapToolEditPolyline_p;
class ReosMapTool_p;
class ReosMenuPopulator;

class ReosMapTool : public QObject
{
  public:
    virtual ~ReosMapTool();
    void activate();
    void deactivate();
    void setCurrentToolInMap() const;
    void setAction( QAction *action );

    void setCursor( const QCursor &cursor );

    //! Sets context menu populator, take ownership
    void setContextMenuPopulator( ReosMenuPopulator *populator );

  protected:
    ReosMapTool( ReosMap *map );

  private:
    virtual ReosMapTool_p *tool_p() const = 0;
    ReosMap *mMap = nullptr;
};

class ReosMapToolNeutral: public ReosMapTool
{
  public:
    ReosMapToolNeutral( ReosMap *map );
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
    ~ReosMapToolDrawPoint();

  signals:
    void drawn( const QPointF &point );

  private:
    QPointer<ReosMapToolDrawPoint_p> d;
    ReosMapTool_p *tool_p() const override;
};


class ReosMapToolDrawPolyRubberBand : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyRubberBand( ReosMap *map, bool closed );
    ~ReosMapToolDrawPolyRubberBand();

    void setStrokeWidth( double width );
    void setColor( const QColor &color );
    void setSecondaryStrokeColor( const QColor &color );
    void setLineStyle( Qt::PenStyle style );

  protected:
    QPointer<ReosMapToolDrawPolyline_p> d;
  private:
    ReosMapTool_p *tool_p() const override;
};

class ReosMapToolDrawPolyline : public ReosMapToolDrawPolyRubberBand
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyline( ReosMap *map );

  signals:
    void drawn( const QPolygonF &polyline ) const;
};

class ReosMapToolDrawPolygon : public ReosMapToolDrawPolyRubberBand
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolygon( ReosMap *map );

    void setFillColor( const QColor &color );

  signals:
    void drawn( const QPolygonF &polygon ) const;
};

class ReosMapToolDrawExtent: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawExtent( ReosMap *map );
    ~ReosMapToolDrawExtent();

    void setStrokeWidth( double width );
    void setColor( const QColor &color );
    void setSecondaryStrokeColor( const QColor &color );
    void setFillColor( const QColor &color );
    void setLineStyle( Qt::PenStyle style );

  signals:
    void extentDrawn( const QRectF &extent );

  private:
    ReosMapToolDrawExtent_p *d;
    ReosMapTool_p *tool_p() const override;
};

class ReosMapToolSelectMapItem : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolSelectMapItem( ReosMap *map, int targetType = -1 );
    ReosMapToolSelectMapItem( ReosMap *map, const QString &targetDescription );
    ~ReosMapToolSelectMapItem();

    void setSearchUnderPoint( bool b );

  signals:
    void found( ReosMapItem *item, const QPointF &point );

  private:
    ReosMapToolSelectMapItem_p *d;
    ReosMapTool_p *tool_p() const;
};

class ReosMapToolEditMapPolyline : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolEditMapPolyline( ReosMap *map );

    //! Sets the map \a polyline to edit
    void setMapPolyline( ReosMapPolyline *polyline );

  signals:
    void polylineEdited();
  private:
    ReosMapToolEditPolyline_p *d = nullptr;
    ReosMapTool_p *tool_p() const;
};

#endif // REOSMAPTOOL_H
