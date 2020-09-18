/***************************************************************************
                      reosmapitem.h
                     --------------------------------------
Date                 : 17-09-2020
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

#ifndef REOSMAPITEM_H
#define REOSMAPITEM_H

#include <QPolygonF>

class ReosMap;
class QgsMapCanvas;
class ReosMapPolygon_p;

class ReosMapItem
{
  public:
    virtual ~ReosMapItem();

};

class ReosMapItemFactory
{
  private:
    virtual ReosMapItem *create( QgsMapCanvas *mapcanvas ) = 0;
    friend class ReosMap;
};

class ReosMapPolygon: public ReosMapItem
{
  public:
    ~ReosMapPolygon() override;

    void setPolygon( const QPolygonF &polygon );
    void movePoint( int pointIndex, const QPointF &p );

  private:
    ReosMapPolygon_p *d = nullptr;
    ReosMapPolygon( ReosMapPolygon_p * );

    friend class ReosMapPolygonFactory;
};

class ReosMapPolygonFactory: public ReosMapItemFactory
{
  public:
    ReosMapPolygon *mapItem( ReosMap *map, const QPolygonF &polygon );

  private:
    virtual ReosMapItem *create( QgsMapCanvas *map ) override;

    friend class ReosMap;
};


#endif // REOSMAPITEM_H
