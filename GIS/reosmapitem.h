/***************************************************************************
                      reosmapitem.h
                     --------------------------------------
Date                 : 02-05-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDMAPITEM_H
#define HDMAPITEM_H

#include <QPainter>

#include <qgsmapcanvasitem.h>
#include <qgsmapcanvas.h>
#include <qgsgeometry.h>



#define WATERSHED 67000
#define BURNINGLINE 67100

class ReosMapItem;

class ReosMapItemDomain : public QObject
{
  public:
    ReosMapItemDomain( QObject *parent, QgsMapCanvas *canvas );
    ReosMapItemDomain( ReosMapItemDomain *parent, QgsMapCanvas *canvas );

    QgsMapCanvas *canvas() const {return mCanvas;}

    void addItem( ReosMapItem *item );
    ReosMapItem *item( int i ) const;
    ReosMapItem *item( const QRectF &rect );
    ReosMapItem *item( const QPointF &point );
    void removeItem( ReosMapItem *item );
    int itemsCount() const;

    void setZValue( int z );
    int zValue() const {return mZValue;}

    void clear();

  protected:

    void addSubDomain( ReosMapItemDomain *sub );

  private:
    ReosMapItem *itemInDomain( const QList<QGraphicsItem *> &items );



  private:
    ReosMapItemDomain *mSuperDomain;
    QList<ReosMapItemDomain *> mSubDomains;
    QgsMapCanvas *mCanvas = nullptr;
    QList<ReosMapItem *> mItemsList;
    int mZValue = 0;


};


class ReosMapItem : public QgsMapCanvasItem
{
  public:
    ReosMapItem( QgsMapCanvas *canvas );
    virtual ~ReosMapItem();

    QgsMapCanvas *canvas() const {return mMapCanvas;}

  private:
    QgsMapCanvas *mCanvas;
};

class ReosMapItemSegment;

class ReosMapItemNode : public ReosMapItem
{
  public:
    virtual ~ReosMapItemNode() override;

    void setPosition( const QPointF &pt );
    QPointF position() const {return mapPosition.toQPointF();}

    void setPen( const QPen &pen );
    QPen pen();
    void setBrush( const QBrush &brush );

    void setSize( int size );

    void updatePosition() override;

    int segmentsCount() const;
    void removeSegmentAt( int i );

  protected:
    ReosMapItemNode( QgsPointXY center, QgsMapCanvas *canvas );
    void paint( QPainter *painter ) override;

    void addSegment( ReosMapItemSegment *segment );
    ReosMapItemSegment *segment( int i ) const ;
    ReosMapItemSegment *segment( ReosMapItemNode *otherNode ) const ;
    void removeSegment( ReosMapItemSegment *seg );


  private:
    QgsPointXY mapPosition;
    QPen mPen;
    QBrush mBrush;
    int mSize = 1;
    QList<ReosMapItemSegment *> mSegments;

    // QGraphicsItem interface
  public:
    QRectF boundingRect() const override
    {
      return QRectF( -mSize / 2, -mSize / 2, mSize, mSize );
    }
};

class ReosMapItemSegment : public ReosMapItem
{
  public:

    bool isOtherExtremity( ReosMapItemNode *current, ReosMapItemNode *other ) const
    {
      return ( ( current == mNode1 && other == mNode2 ) || ( current == mNode2 && other == mNode1 ) );
    }

    bool isNodeExtremity( ReosMapItemNode *node ) const
    {
      return ( node == mNode2 || node == mNode1 );
    }


    QRectF boundingRect() const override
    {
      QRectF bounding = QRectF( mNode1->pos(), mNode2->pos() );
      return bounding.normalized();
    }

    QPainterPath shape() const override
    {
      QPainterPathStroker pps( mPen );
      QPainterPath path;
      QPolygonF poly;

      poly << mNode1->pos();
      poly << mNode2->pos();
      path.addPolygon( poly );
      return pps.createStroke( path );
    }

  protected:
    ReosMapItemSegment( QgsMapCanvas *canvas, ReosMapItemNode *n1, ReosMapItemNode *n2 ): ReosMapItem( canvas ), mNode1( n1 ), mNode2( n2 )
    {

    }

    ReosMapItemNode *node1() {return mNode1;}
    ReosMapItemNode *node2() {return mNode2;}

    virtual ReosMapItemNode *otherExtremity( ReosMapItemNode *node ) const
    {
      if ( node == mNode1 )
        return mNode2;

      if ( node == mNode2 )
        return  mNode1;

      return nullptr;
    }

    void setPen( const QPen &pen )
    {
      mPen = pen;
    }

  private:
    ReosMapItemNode *mNode1;
    ReosMapItemNode *mNode2;
    QPen mPen;


    // QgsMapCanvasItem interface
  protected:
    void paint( QPainter *painter ) override
    {
      painter->save();
      painter->setPen( mPen );
      painter->drawLine( mNode1->pos(), mNode2->pos() );
      painter->restore();
    }
};




class ReosMapItemPolyline: public ReosMapItem
{
  public:
    //! Constructor with a QGis polyline
    ReosMapItemPolyline( QgsMapCanvas *canvas, const QgsPolylineXY &polyline );

    //!Constructor with a Qt polyline
    ReosMapItemPolyline( QgsMapCanvas *canvas, const QPolygonF &polyline );

    //! destructor
    ~ReosMapItemPolyline() override {}

    //!set the polyline with a Qt polyline
    void setPolyline( const QPolygonF &polyline );
    //!set the polyline with a QGis polyline
    void setPolyline( const QgsPolylineXY &poly );
    //!return the polyline in the view coordinate
    QPolygonF viewPolyline() const {return mViewPolyline;}
    //!return the polyline in the map coordinates
    QPolygonF polyline() const {return mMapPolyline;}

    //! update the position of the polyline in the view coordinate
    virtual void updatePosition() override;


    //! set the pen of the polyline
    void setPen( const QPen &p ) {mPen = p;}
    //! set the color of the pen
    void setColorPen( const QColor &c ) {mPen.setColor( c );}

    //! return the (x,y) in map coordinats of the vertex at position i. Caller has the responsability to check if i less than the vertex count
    const QPointF &vertex( int i ) const {return mMapPolyline.at( i );}
    //! return the vertices count in the polyline
    int vertexCount() const {return mMapPolyline.count();}
    //! add a vertex at the end of the polyline
    void addVertex( const QPointF &point );
    //! insert a vertyex at the position i. if position is greater than the vertices count, the vertex is added at the end of the polyline
    void insertVertex( const QPoint &point, int position );

    void setMapAndViewPoint( int i, const QPointF &mapPoint );

    void paint( QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget ) override;
    void paint( QPainter *painter ) override;

    //////////////////////////////////////////////////////
    /// method to handle selected vertex

    //! set selected all the vertices in the rect
    void selectVertices( const QgsRectangle &rect );
    //! deselected all the selected vertices
    void deselectVertices();

    //! return true if the vertex at position i is selected
    bool isVertexSelected( int i ) const;

    //! return a list of the selected vertices
    const QList<int> &selectedVertices() const;

    QRectF boundingRect() const override
    {
      return mViewPolyline.boundingRect();
    }

  private:
    QPolygonF mMapPolyline;
    QPolygonF mViewPolyline;

    QPen mPen;

    QList<int> mSelectedVertex;


};




class ReosMapRectangularItem: public ReosMapItem
{
  public:
    ReosMapRectangularItem( QgsMapCanvas *canvas, const QgsRectangle &rectMap );
    virtual ~ReosMapRectangularItem() override;

    void setPen( const QPen &pen );
    QgsRectangle getRectMap() const;

  private:
    QRectF rectView;
    QgsRectangle rectMap;
    QPen pen_;

    // QGraphicsItem interface
  public:
    QRectF boundingRect() const override;

    // QgsMapCanvasItem interface
  protected:
    void paint( QPainter *painter ) override;

    void paint( QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget ) override;

  public:
    void updatePosition() override;

};

#endif // HDMAPITEM_H
