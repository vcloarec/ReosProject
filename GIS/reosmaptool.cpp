/***************************************************************************
                      hdmaptool.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmaptool.h"
#include "reosmap.h"
#include <chrono>

ReosMapTool::~ReosMapTool()
{
    map_->unsetMapTool(this);
}

void ReosMapTool::keyPressEvent(QKeyEvent *e)
{
    if (e->key()==Qt::Key_Escape)
    {
        askForEscape();
        e->accept();
        return;
    }

   e->ignore();
}

HdMapToolRectangularSelection::HdMapToolRectangularSelection(ReosMap *map): ReosMapTool(map),
    rubberBand_(new QgsRubberBand(map->getMapCanvas(),QgsWkbTypes::LineGeometry))
{
    rubberBand_->setColor(Qt::red);
    rubberBand_->setLineStyle(Qt::DotLine);
    rubberBand_->setWidth(3);
    rubberBand_->setBrushStyle(Qt::NoBrush);
    setCursor(QCursor(QPixmap("://cursor/selectionRectangulaire.png"),3,3));
}

void HdMapToolRectangularSelection::reset()
{
    rubberBand_->reset(QgsWkbTypes::LineGeometry);
    inProgress_=false;
    topLeftPoint_=QgsPointXY();
    bottomRightPoint_=QgsPointXY();
}

void HdMapToolRectangularSelection::update()
{
    rubberBand_->reset(QgsWkbTypes::LineGeometry);
    rubberBand_->addPoint(QgsPointXY(topLeftPoint_.x(),topLeftPoint_.y()),false);
    rubberBand_->addPoint(QgsPointXY(bottomRightPoint_.x(),topLeftPoint_.y()),false);
    rubberBand_->addPoint(QgsPointXY(bottomRightPoint_.x(),bottomRightPoint_.y()),false);
    rubberBand_->addPoint(QgsPointXY(topLeftPoint_.x(),bottomRightPoint_.y()),false);
    rubberBand_->closePoints(true);
}

void HdMapToolRectangularSelection::canvasMoveEvent(QgsMapMouseEvent *e)
{
    if (inProgress_)
    {
        bottomRightPoint_=e->mapPoint();
        update();
    }

}

void HdMapToolRectangularSelection::canvasPressEvent(QgsMapMouseEvent *e)
{
    if (e->button()==Qt::LeftButton)
    {
        if (inProgress_)
        {
            bottomRightPoint_=e->mapPoint();
            rubberBand_->reset(QgsWkbTypes::LineGeometry);
            inProgress_=false;
            emit selectionDone(QgsRectangle(topLeftPoint_,bottomRightPoint_));
        }
        else
        {
            topLeftPoint_=e->mapPoint();
            inProgress_=true;
        }
    }

}


ReosMapTool::ReosMapTool(ReosMap *map):QgsMapTool(map->getMapCanvas()),inProgress_(false),map_(map)
{}

void ReosMapTool::deactivate()
{
    reset();
    QgsMapTool::deactivate();
}

void ReosMapTool::askForEscape()
{
    if (!inProgress_)
    {
        emit stop();
    }
    reset();
}

HdMapToolLinearSelection::HdMapToolLinearSelection(ReosMap *map):ReosMapTool(map),
    rubberBand_(new QgsRubberBand(map->getMapCanvas(),QgsWkbTypes::LineGeometry))
{
    rubberBand_->setColor(Qt::darkGreen);
    rubberBand_->setLineStyle(Qt::DotLine);
    rubberBand_->setWidth(3);
    setCursor(QCursor(QPixmap("://cursor/selectionLineaire.png"),3,3));
}

void HdMapToolLinearSelection::setColor(QColor color)
{
    rubberBand_->setColor(color);
}

void HdMapToolLinearSelection::reset()
{
    rubberBand_->reset(QgsWkbTypes::LineGeometry);
    inProgress_=false;
}

void HdMapToolLinearSelection::canvasMoveEvent(QgsMapMouseEvent *e)
{
    if (inProgress_)
    {
        rubberBand_->removeLastPoint();
        rubberBand_->addPoint(e->mapPoint());
    }
}

void HdMapToolLinearSelection::canvasPressEvent(QgsMapMouseEvent *e)
{
    if (e->button()==Qt::LeftButton)
    {
        if (!inProgress_)
        {
            rubberBand_->reset(QgsWkbTypes::LineGeometry);
            inProgress_=true;
        }
        rubberBand_->addPoint(e->mapPoint());
    }

    if (e->button()==Qt::RightButton)
    {
        if (rubberBand_->numberOfVertices()>1)
        {
            QgsPolylineXY poly;
            for (int i=0;i<rubberBand_->numberOfVertices();++i)
            {
                QgsPointXY point=(*rubberBand_->getPoint(0,i));
                poly.append(point);
            }
            rubberBand_->reset(QgsWkbTypes::LineGeometry);
            inProgress_=false;
            emit selectionDone(poly);
        }
        else
        {
            rubberBand_->reset(QgsWkbTypes::LineGeometry);
            inProgress_=false;
        }


    }

}


//HdFactoryMapTool::~HdFactoryMapTool() {}

//HdMapToolRectangularSelection *HdFactoryMapToolRectangularSelection::getTool() const
//{
//    return mapTool;
//}

//QgsMapTool *HdFactoryMapToolRectangularSelection::makeTool(QgsMapCanvas *canvas)
//{
//    mapTool=new HdMapToolRectangularSelection(canvas);
//    return mapTool;
//}

HdMapToolNeutral::HdMapToolNeutral(ReosMap *map):ReosMapTool(map)
{
    setCursor(Qt::ArrowCursor);
}

HdMapToolNeutral *HdMapToolNeutral::makeMapToolNeutral(ReosMap *map)
{
    HdMapToolNeutral *tool=new HdMapToolNeutral(map);
    return tool;
}

//HdMapToolLinearSelection *HdFactoryMapToolLinearSelection::getTool() const
//{
//    return mapTool;
//}

//QgsMapTool *HdFactoryMapToolLinearSelection::makeTool(QgsMapCanvas *canvas)
//{
//    mapTool=new HdMapToolLinearSelection(canvas);
//    return mapTool;
//}

HdMapToolItemSelection::HdMapToolItemSelection(ReosMap *map, int itemType):ReosMapTool(map),itemType(itemType)
{

}

void HdMapToolItemSelection::canvasPressEvent(QgsMapMouseEvent *e)
{
    QPointF p=e->localPos();
    QRectF rectf(p.x(),p.y(),5,5);
    QList<QGraphicsItem*> listItems=canvas()->scene()->items(rectf);

    QGraphicsItem *item=nullptr;

    int i=0;
    while(!(item) && i<listItems.count())
    {
        if (listItems.at(i)->type()==itemType)
            item=listItems.at(i);
        ++i;
    }

    if (item)
        emit foundItem(item);


}

//HdFactoryMapToolItemSelection::HdFactoryMapToolItemSelection(int itemType):itemType(itemType)
//{}

//HdMapToolItemSelection *HdFactoryMapToolItemSelection::getTool() const
//{
//    return mapTool;
//}

//QgsMapTool *HdFactoryMapToolItemSelection::makeTool(QgsMapCanvas *canvas)
//{
//    mapTool=new HdMapToolItemSelection(canvas,itemType);
//    return mapTool;
//}



HdMapToolLinearDrawing::HdMapToolLinearDrawing(ReosMap *map, QgsWkbTypes::GeometryType geometryType):
    ReosMapTool (map),
    rubberBand(new QgsRubberBand(map->getMapCanvas(),geometryType)),
    geometryType(geometryType)
{

}

QgsPolylineXY HdMapToolLinearDrawing::getPolyline()
{
    QgsPolylineXY poly;
    for (int i=0;i<rubberBand->numberOfVertices()-1;++i)
    {
        QgsPointXY point=(*rubberBand->getPoint(0,i));
        poly.append(point);
    }

    return poly;
}

void HdMapToolLinearDrawing::setStrokeColor(QColor col)
{
    rubberBand->setStrokeColor(col);
}

void HdMapToolLinearDrawing::setWidth(int width)
{
    rubberBand->setWidth(width);
}

void HdMapToolLinearDrawing::setLineStyle(Qt::PenStyle style)
{
    rubberBand->setLineStyle(style);
}

void HdMapToolLinearDrawing::setColor(QColor c)
{
    rubberBand->setColor(c);
}

void HdMapToolLinearDrawing::setFillColor(QColor col)
{
    rubberBand->setFillColor(col);
}

void HdMapToolLinearDrawing::setSecondaryStrokeColor(QColor col)
{
    rubberBand->setSecondaryStrokeColor(col);
}

bool HdMapToolLinearDrawing::control()
{
    QgsPolylineXY poly;
    int segmentToTst=0;
    if (rubberBand->asGeometry().type()==QgsWkbTypes::PolygonGeometry)
    {
        poly=rubberBand->asGeometry().asPolygon().at(0);
        if (!poly.isEmpty())
            poly.removeLast();
        if (!poly.isEmpty())
            poly.removeLast();

        if (poly.count()<3)
        {
            setInvalid(true);
            return false;
        }
        segmentToTst=poly.count();
    }

    if (rubberBand->asGeometry().type()==QgsWkbTypes::LineGeometry)
    {
        poly=rubberBand->asGeometry().asPolyline();
        if (!poly.isEmpty())
            poly.removeLast();

        if (poly.count()<2)
        {
            setInvalid(true);
            return false;
        }

        segmentToTst=poly.count()-1;
    }

    bool intersect=false;
    int i=0;
    while ((!intersect)&&(i<segmentToTst-1))
    {
        int j=i+1;
        while ((!intersect)&&(j<segmentToTst))
        {
            int pt1_1=i;
            int pt1_2=(i+1)%poly.count();
            int pt2_1=j;
            int pt2_2=(j+1)%poly.count();
            if ((abs(pt1_1-pt2_1)>1)&&(abs(pt1_1-pt2_1)!=poly.count()-1))
            {
                intersect2D intersection(poly.at(pt1_1).toQPointF(),poly.at(pt1_2).toQPointF(),
                                         poly.at(pt2_1).toQPointF(),poly.at(pt2_2).toQPointF());

                intersect=intersection.isIntersectSegmentExist();
            }
            ++j;

        }
        ++i;
    }

    setInvalid(intersect);

    return !intersect;

}

void HdMapToolLinearDrawing::finishEditing()
{
    if (control())
       emit selectionDone(getPolyline());
    reset();
}

void HdMapToolLinearDrawing::canvasMoveEvent(QgsMapMouseEvent *e)
{
    rubberBand->movePoint(e->mapPoint());
}

void HdMapToolLinearDrawing::canvasPressEvent(QgsMapMouseEvent *e)
{
    if (e->button()==Qt::LeftButton)
    {
        inProgress_=true;
        rubberBand->addPoint(e->mapPoint());
        control();
        undoPoint.clear();
    }
    if (e->button()==Qt::RightButton)
    {
        inProgress_=false;
        finishEditing();
    }

}

void HdMapToolLinearDrawing::keyPressEvent(QKeyEvent *e)
{

    if (e->matches(QKeySequence::Undo))
    {
        if (rubberBand->numberOfVertices()>1)
        {
            const QgsPointXY *lastPoint=rubberBand->getPoint(0,rubberBand->numberOfVertices()-2);
            if (lastPoint)
            {
                undoPoint.append((*lastPoint));
                rubberBand->removePoint(rubberBand->numberOfVertices()-2);
                control();
            }
        }
        else
        {
            reset();
            emit stop();
        }
        e->accept();
        return;
    }

    if (e->matches((QKeySequence::Redo)))
    {
        if (!undoPoint.empty())
        {
            QgsPointXY currentPoint=*rubberBand->getPoint(0,rubberBand->numberOfVertices()-1);
            rubberBand->removePoint(rubberBand->numberOfVertices()-1);
            rubberBand->addPoint(undoPoint.last(),false);
            undoPoint.removeLast();
            rubberBand->addPoint(currentPoint,true);
            control();

        }
        e->accept();
        return;
    }

    ReosMapTool::keyPressEvent(e);
}

void HdMapToolLinearDrawing::reset()
{
    rubberBand->reset(geometryType);
    undoPoint.clear();
    inProgress_=false;
}

void ReosMapToolClickPoint::canvasPressEvent(QgsMapMouseEvent *e)
{
    emit clickDone(e->mapPoint().toQPointF());
}
