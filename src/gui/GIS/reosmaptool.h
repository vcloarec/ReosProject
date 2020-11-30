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

class ReosMapToolDrawPolyline_p;
class ReosMapToolDrawExtent_p;
class ReosMapTool_p;

class ReosMapTool : public QObject
{
  public:
    void activate();
    void deactivate();
    void setCurrentToolInMap() const;

    void setAction( QAction *action );

  private:
    virtual ReosMapTool_p *tool_p() const = 0;
};

class ReosMapToolDrawPolyline : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawPolyline( ReosMap *map );

    void setStrokeWidth( double width );
    void setColor( const QColor &color );
    void setSecondaryStrokeColor( const QColor &color );
    void setLineStyle( Qt::PenStyle style );

  signals:
    void polylineDrawn( const QPolygonF &polyline ) const;

  private:
    ReosMapToolDrawPolyline_p *d;
    ReosMapTool_p *tool_p() const override;
};

class ReosMapToolDrawExtent: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawExtent( ReosMap *map );

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

#endif // REOSMAPTOOL_H
