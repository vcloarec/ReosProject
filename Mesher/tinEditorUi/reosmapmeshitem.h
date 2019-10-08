/***************************************************************************
                      hdmapmeshitem.h
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDMAPMESHITEM_H
#define HDMAPMESHITEM_H

#include <QPainter>

#include <qgsmapcanvas.h>
#include <qgsmapcanvasitem.h>

#include <../../GIS/reosmapitem.h>
#include "../ReosMesh/reosmesheditor.h"


class ReosMeshItemSegment;

class ReosMapMeshItem : public ReosMapItem
{
public:
    ReosMapMeshItem(QgsMapCanvas *canvas);
    virtual ~ReosMapMeshItem();
};


class ReosMeshItemVertex: public ReosMapItemNode
{
public:
    ReosMeshItemVertex(const QPointF &mapPosition,QgsMapCanvas *canvas);
    virtual ~ReosMeshItemVertex();

    void setRealWorldVertex(VertexPointer vertex);
    VertexPointer realWorldVertex() const;

    void addSegment(ReosMeshItemSegment* seg);

    ReosMeshItemSegment *segment(int i) const;
    ReosMeshItemSegment *segment(ReosMeshItemVertex* otherVertex);
    void removeSegment(ReosMeshItemSegment *seg);

private:

    ///////////////////////////////////////////////
    /// \brief mRealWorldVertex
    ///To link the graphic vertex with the real world vertex.
    ///This solution is prefered to a hash or a map to provide a faster acces from the graphic vertex
    VertexPointer mRealWorldVertex=nullptr;



};




class ReosMeshItemSegment: public ReosMapItemSegment
{
public:
    ReosMeshItemSegment(ReosMeshItemVertex *n0,ReosMeshItemVertex *n1,QgsMapCanvas *canvas);
    virtual ~ReosMeshItemSegment();

    ReosMeshItemVertex *otherVertex(ReosMeshItemVertex *vertex) const
    {
        return static_cast<ReosMeshItemVertex*>(ReosMapItemSegment::otherExtremity(vertex));
    }

    ReosMeshItemVertex *vertex1() {return static_cast<ReosMeshItemVertex*>(node1());}
    ReosMeshItemVertex *vertex2() {return static_cast<ReosMeshItemVertex*>(node2());}

    void removeFromNode()
    {
        vertex1()->removeSegment(this);
        vertex2()->removeSegment(this);
    }


private:

};

class ReosMeshItemFace: public ReosMapItem
{
public:
    ReosMeshItemFace(const QList<QPointF> &points,QgsMapCanvas *canvas):ReosMapItem(canvas),mMapPoints(points)
    {
        mPen=QPen(QColor(Qt::red));
        mBrush=QBrush(QColor(255,0,0,150),Qt::SolidPattern);
        updatePosition();
    }
    virtual ~ReosMeshItemFace() override;

    QRectF boundingRect() const override
    {
        double xmin=DBL_MAX;
        double ymin=DBL_MAX;
        double xmax=-DBL_MAX;
        double ymax=-DBL_MAX;

        for (auto p:mViewPoints)
        {
            if (p.x()<xmin)
                xmin=p.x();
            if (p.x()>xmax)
                xmax=p.x();
            if (p.y()<ymin)
                ymin=p.y();
            if (p.y()>ymax)
                ymax=p.y();
        }

        return QRectF(QPointF(xmin,ymin),QPointF(xmax,ymax));
    }

    void updatePosition() override
    {
        mViewPoints.clear();
        for (auto p:mMapPoints)
        {
            mViewPoints.append(toCanvasCoordinates(QgsPointXY(p)));
        }
    }

    void setBrushColor(const QColor &c){
        mBrush.setColor(c);
    }

protected:
    void paint(QPainter *painter) override{
        painter->save();
        painter->setPen(mPen);
        painter->setBrush(mBrush);
        painter->drawPolygon(mViewPoints.data(),mViewPoints.count());
        painter->restore();
    }

private:
    QList<QPointF> mMapPoints;
    QVector<QPointF> mViewPoints;

    QPen mPen;
    QBrush mBrush;

};





class ReosMapMeshEditorItemDomain: public ReosMapItemDomain{
public:
    ReosMapMeshEditorItemDomain(QObject *parent,QgsMapCanvas *canvas);

    ReosMeshItemVertex* addVertex(const QPointF &p);
    ReosMeshItemSegment* addSegmentHardLine(ReosMeshItemVertex *v1, ReosMeshItemVertex*v2);

    ReosMeshItemVertex *vertex(int n) const;
    ReosMeshItemVertex *vertex(const QRectF &rect) const;

    ReosMeshItemSegment* segment(int i) const
    {
        return static_cast<ReosMeshItemSegment*>(segmentsDomain->item(i));
    }
    ReosMeshItemSegment* segment(const QRectF &rect) const
    {
        return static_cast<ReosMeshItemSegment*>(segmentsDomain->item(rect));
    }

    int verticesCount() const;
    int segmentCount() const;

    void removeSegment(ReosMeshItemSegment *seg);
    void removeVertex(ReosMeshItemVertex *vert)
    {
        int i=0;
        while(vert->segmentsCount()!=0)
        {

            ReosMeshItemSegment* seg=vert->segment(i);
            seg->removeFromNode();
            segmentsDomain->removeItem(seg);
            delete seg;
        }

        verticesDomain->removeItem(vert);
        delete vert;
    }



private:
    ReosMapItemDomain *verticesDomain=nullptr;
    ReosMapItemDomain *segmentsDomain=nullptr;

};


#endif // HDMAPMESHITEM_H
