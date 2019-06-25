/***************************************************************************
                      reosmapitem.cpp
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

#include "reosmapitem.h"


ReosMapItem::ReosMapItem(QgsMapCanvas *canvas):QgsMapCanvasItem(canvas),mCanvas(canvas)
{}

ReosMapItem::~ReosMapItem() {}

ReosMapItemDomain::ReosMapItemDomain(QObject *parent,QgsMapCanvas* canvas):QObject(parent),mCanvas(canvas)
{

}

ReosMapItemDomain::ReosMapItemDomain(ReosMapItemDomain *parent, QgsMapCanvas *canvas):QObject(parent),mSuperDomain(parent),mCanvas(canvas)
{
    parent->addSubDomain(this);
}

void ReosMapItemDomain::addItem(ReosMapItem *item)
{
    mItemsList.append(item);
    item->setZValue(mZValue);
}

ReosMapItem *ReosMapItemDomain::item(int i) const
{
    return mItemsList.at(i);
}

ReosMapItem *ReosMapItemDomain::item(const QRectF &rect)
{
    QRect canvasRect=rect.toRect();
    auto itemsInRect=canvas()->items(canvasRect);

    return itemInDomain(itemsInRect);
}

ReosMapItem *ReosMapItemDomain::item(const QPointF &point)
{
    auto items=canvas()->items(canvas()->mapFromScene(point));

    return itemInDomain(items);
}

void ReosMapItemDomain::removeItem(ReosMapItem *item)
{
    mItemsList.removeOne(item);
}

int ReosMapItemDomain::itemsCount() const {return mItemsList.count();}

ReosMapItem* ReosMapItemDomain::itemInDomain(const QList<QGraphicsItem*> &items)
{
    bool found=false;
    auto itemInDomain=mItemsList.begin();
    while (itemInDomain!=mItemsList.end() && !found)
    {
        auto itemInRect=items.begin();
        while(itemInRect!=items.end() && !found)
        {
            found=(static_cast<ReosMapItem*>(*itemInRect))==(*itemInDomain);

            if (!found)
                itemInRect++;
        }
        if (!found)
            itemInDomain++;
    }

    if (found)
        return (*itemInDomain);
    else {
        return nullptr;
    }
}

void ReosMapItemDomain::setZValue(int z){
    mZValue=z;
    for (auto item:mItemsList)
    {
        item->setZValue(mZValue);
    }
}

void ReosMapItemDomain::clear()
{
    while (!mItemsList.isEmpty()) {
        delete mItemsList.at(0);
        mItemsList.removeAt(0);
    }

    for (auto sd:mSubDomains)
        sd->clear();
}

void ReosMapItemDomain::addSubDomain(ReosMapItemDomain *sub)
{
    mSubDomains.append(sub);
}

ReosMapItemPolyline::ReosMapItemPolyline(QgsMapCanvas *canvas, const QgsPolylineXY &polyline):ReosMapItem(canvas)
{
    setPolyline(polyline);
}

ReosMapItemPolyline::ReosMapItemPolyline(QgsMapCanvas *canvas, const QPolygonF &polyline):ReosMapItem(canvas),mMapPolyline(polyline)
{
}

void ReosMapItemPolyline::setPolyline(const QPolygonF &polyline)
{
    mViewPolyline=polyline;
}

void ReosMapItemPolyline::setPolyline(const QgsPolylineXY &poly)
{
    mMapPolyline.clear();
    for (auto point:poly)
    {
        mMapPolyline.append(point.toQPointF());
    }
    updatePosition();
}

void ReosMapItemPolyline::updatePosition()
{
    prepareGeometryChange();
    mViewPolyline.clear();
    if (mMapPolyline.count()<1)
        return;
    QPointF pview0=toCanvasCoordinates(QgsPoint(mMapPolyline.at(0)));

    for (auto p:mMapPolyline)
    {
        QPointF pview=toCanvasCoordinates(QgsPoint(p));
        mViewPolyline.append(QPointF(pview.x()-pview0.x(),pview.y()-pview0.y()));
    }
    setPos(pview0);
}

void ReosMapItemPolyline::setMapAndViewPoint(int i, const QPointF &mapPoint)
{
    prepareGeometryChange();
    mMapPolyline[i]=mapPoint;
    QPointF pview=toCanvasCoordinates(QgsPoint(mapPoint));
    pview=pview-pos();
    mViewPolyline[i]=pview;
}

void ReosMapItemPolyline::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    Q_UNUSED(option);
    Q_UNUSED(widget);

    paint(painter);
}

void ReosMapItemPolyline::paint(QPainter *painter)
{
    painter->save();
    painter->setPen(mPen);
    painter->drawPolyline(mViewPolyline);
    painter->restore();
}

void ReosMapItemPolyline::selectVertex(const QgsRectangle &rect)
{
    mSelectedVertex.clear();
    QRectF rectF=rect.toRectF();
    for (int i=0;i<mMapPolyline.count();++i)
    {
        if (rectF.contains(mMapPolyline.at(i)))
            mSelectedVertex.append(i);
    }
    update();
}

void ReosMapItemPolyline::deselectVertex()
{
    mSelectedVertex.clear();
    update();
}

bool ReosMapItemPolyline::isVertexSelected(int i) const
{
    return mSelectedVertex.contains(i);
}

const QList<int> &ReosMapItemPolyline::selectedVertices() const {
    return mSelectedVertex;
}

ReosMapRectangularItem::ReosMapRectangularItem(QgsMapCanvas *canvas, const QgsRectangle &rectMap):
    ReosMapItem(canvas),rectMap(rectMap)
{

}

ReosMapRectangularItem::~ReosMapRectangularItem() {}

void ReosMapRectangularItem::setPen(const QPen &pen)
{
    pen_=pen;
}

QgsRectangle ReosMapRectangularItem::getRectMap() const {return rectMap;}

QRectF ReosMapRectangularItem::boundingRect() const
{
    qreal penWidth=pen_.widthF();
    QRectF retour=rectView.adjusted(-penWidth,-penWidth,penWidth,penWidth);
    return retour;
}

void ReosMapRectangularItem::paint(QPainter *painter)
{
    painter->save();
    painter->setPen(pen_);
    painter->drawRect(rectView);
    painter->restore();
}

void ReosMapRectangularItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    Q_UNUSED(option);
    Q_UNUSED(widget);

    paint(painter);
}

void ReosMapRectangularItem::updatePosition()
{
    prepareGeometryChange();
    QgsPointXY topLeftMap(rectMap.xMinimum(),rectMap.yMaximum());
    QgsPointXY bottomRightMap(rectMap.xMaximum(),rectMap.yMinimum());
    QPointF topleft=toCanvasCoordinates(topLeftMap);
    QPointF bottomRight=toCanvasCoordinates(bottomRightMap);
    rectView=QRectF(topleft,bottomRight);
}

ReosMapItemNode::ReosMapItemNode(QgsPointXY center, QgsMapCanvas *canvas): ReosMapItem(canvas),mapPosition(center)
{
    updatePosition();
}

ReosMapItemNode::~ReosMapItemNode() {}

void ReosMapItemNode::setPosition(const QPointF &pt)
{
    mapPosition=pt;
    setPos(toCanvasCoordinates(pt));
}

void ReosMapItemNode::setPen(const QPen &pen){
    mPen=pen;
}

QPen ReosMapItemNode::pen(){return mPen;}

void ReosMapItemNode::setBrush(const QBrush &brush){
    mBrush=brush;
}

void ReosMapItemNode::setSize(int size){
    mSize=size;
}

void ReosMapItemNode::addSegment(ReosMapItemSegment *segment)
{
    if (!mSegments.contains(segment))
        mSegments.append(segment);
}

ReosMapItemSegment *ReosMapItemNode::segment(int i) const
{
    if (i<mSegments.count() && i>=0)
        return mSegments.at(i);
    else
        return nullptr;
}

ReosMapItemSegment *ReosMapItemNode::segment(ReosMapItemNode *otherNode) const
{
    bool found=false;
    int i=0;
    while (i<mSegments.count() && !found)
    {
        found=mSegments.at(i)->isNodeExtremity(otherNode);
        if (!found)
            i++;
    }

    return segment(i);
}

void ReosMapItemNode::removeSegment(ReosMapItemSegment *seg)
{
    mSegments.removeOne(seg);
}

void ReosMapItemNode::removeSegmentAt(int i)
{
    mSegments.removeAt(i);
}

void ReosMapItemNode::updatePosition(){
    prepareGeometryChange();
    setPos((toCanvasCoordinates(mapPosition)));
}

int ReosMapItemNode::segmentsCount() const {return mSegments.count();}

void ReosMapItemNode::paint(QPainter *painter)
{
    painter->save();
    painter->setBrush(mBrush);
    painter->setPen(mPen);
    painter->drawEllipse(-mSize/2,-mSize/2,mSize,mSize);
    painter->restore();
}
