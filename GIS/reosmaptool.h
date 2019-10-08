/***************************************************************************
                      reosmaptool.h
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

#ifndef REOSMAPTOOL_H
#define REOSMAPTOOL_H

#include <QObject>
#include <QLabel>
#include <QMenu>
#include <QAction>

#include <qgsmapcanvas.h>
#include <qgsrubberband.h>
#include <qgsmaptool.h>
#include <qgsmapmouseevent.h>

#include "../UtilsGeometry/utilsgeometry2d.h"



class ReosMap;

class ReosMapTool:public QgsMapTool
{
    Q_OBJECT
public:

    virtual ~ ReosMapTool() override;
    void deactivate() override;

    virtual bool isInProgress() const {return inProgress_;}

    ReosMap *map() const {return map_;}

    virtual void suspend();
    virtual void unsuspend();

signals:
    void undoCommandCreated(QUndoCommand* comm);
    void stop();

public slots:
    virtual void reset() {}
    virtual void askForEscape();


protected:
    ReosMapTool(ReosMap *map);

    bool inProgress_=false;
    virtual void keyPressEvent( QKeyEvent* e ) override;


private:
    ReosMap *map_;
    bool suspended_=false;



};


class HdMapToolNeutral:public ReosMapTool
{
public:
    HdMapToolNeutral(ReosMap *map);

    static HdMapToolNeutral* makeMapToolNeutral(ReosMap* map);
};

//class HdFactoryMapTool
//{
//public:
//    virtual ~HdFactoryMapTool();
//    virtual QgsMapTool* makeTool(QgsMapCanvas *canvas)=0;
//};


class ReosMapToolClickPoint: public ReosMapTool
{
    Q_OBJECT
public:
    ReosMapToolClickPoint(ReosMap *map):ReosMapTool(map){}

signals:
    void clickDone(const QPointF &pt);
    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

class ReosMapToolSelection: public ReosMapTool
{
    Q_OBJECT
public:
    ReosMapToolSelection(ReosMap* map):ReosMapTool(map)
    {

    }



signals:
    void zonalCanvasRect(const QRectF &rect);

protected:
    void canvasPressEvent(QgsMapMouseEvent *e)
    {
        QPointF canvasPoint=toCanvasCoordinates(e->mapPoint());
        emit zonalCanvasRect(selectedZone(canvasPoint));
    }

    QRectF selectedZone(QPointF selectedPoint) const
    {
        return  QRectF(selectedPoint,zoneSize);
    }

private:
    QSizeF zoneSize=QSizeF(8,8);
};

class HdMapToolRectangularSelection:public ReosMapTool
{
    Q_OBJECT
public:
    HdMapToolRectangularSelection(ReosMap *map);

public slots:
    void reset() override;

signals:

    void selectionDone(QgsRectangle rect);

private:
    QgsPointXY topLeftPoint_;
    QgsPointXY bottomRightPoint_;
    QgsRubberBand *rubberBand_;

    void update();



    // QgsMapTool interface
public:
    void canvasMoveEvent(QgsMapMouseEvent *e) override;
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

//class HdFactoryMapToolRectangularSelection:public HdFactoryMapTool
//{

//public:
//    HdMapToolRectangularSelection* getTool() const;

//private:
//    HdMapToolRectangularSelection *mapTool=nullptr;

//    // HdFactoryMapTool interface
//public:
//    QgsMapTool *makeTool(QgsMapCanvas *canvas) override;
//};

class HdMapToolLinearDrawing: public ReosMapTool
{
    Q_OBJECT
public:
    HdMapToolLinearDrawing(ReosMap *map,QgsWkbTypes::GeometryType geometryType);

    virtual ~HdMapToolLinearDrawing() override
    {

    }

    QgsPolylineXY getPolyline();

signals:

    void selectionDone(QgsPolylineXY line);

protected:
    void virtual setInvalid(bool b) {Q_UNUSED(b);}
    void setStrokeColor(QColor col);
    void setWidth(int width);

    void setLineStyle(Qt::PenStyle style);
    void setColor(QColor c);
    void setFillColor(QColor col);
    void setSecondaryStrokeColor(QColor col);
    bool control();

    virtual void finishEditing();

private:
    QgsRubberBand *rubberBand;
    QList<QgsPointXY> undoPoint;
    QgsWkbTypes::GeometryType geometryType;


    // QgsMapTool interface
public:
    void canvasMoveEvent(QgsMapMouseEvent *e) override;

    void canvasPressEvent(QgsMapMouseEvent *e) override;
    void keyPressEvent(QKeyEvent *e) override;

    // HdMapTool interface
public slots:
    void reset() override;
};

class HdMapToolLinearSelection:public ReosMapTool
{
    Q_OBJECT
public:
    HdMapToolLinearSelection(ReosMap *map);

    void setColor(QColor color);

public slots:
    void reset() override;

signals:

    void selectionDone(QgsPolylineXY line);
private:
    QgsRubberBand *rubberBand_;

    // QgsMapTool interface
public:
    void canvasMoveEvent(QgsMapMouseEvent *e) override;
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

//class HdFactoryMapToolLinearSelection:public HdFactoryMapTool
//{

//public:
//    HdMapToolLinearSelection *getTool() const;

//private:
//    HdMapToolLinearSelection *mapTool=nullptr;

//    // HdFactoryMapTool interface
//public:
//    QgsMapTool *makeTool(QgsMapCanvas *canvas) override;
//};

class HdMapToolItemSelection:public ReosMapTool
{
    Q_OBJECT
public:
    HdMapToolItemSelection(ReosMap *map,int itemType);

signals:
    void foundItem(QGraphicsItem *item);
private:
    int itemType;

    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e);
};



#endif // REOSMAPTOOL_H
