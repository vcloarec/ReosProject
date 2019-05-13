/***************************************************************************
                      hdmaptool.h
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

#ifndef HDMAPTOOL_H
#define HDMAPTOOL_H

#include <QObject>
#include <QLabel>
#include <QMenu>
#include <QAction>

#include <qgsmapcanvas.h>
#include <qgsrubberband.h>
#include <qgsmaptool.h>
#include <qgsmapmouseevent.h>

#include "../UtilsGeometry/utilsgeometry2d.h"



class HdMap;

class HdMapTool:public QgsMapTool
{
    Q_OBJECT
public:

    virtual ~ HdMapTool() override;
    void deactivate() override;


signals:
    void undoCommandCreated(QUndoCommand* comm);
    void arret();

public slots:
    virtual void reset() {}


protected:
    HdMapTool(HdMap *map);

    bool inProgress_=false;
    virtual void keyPressEvent( QKeyEvent* e ) override;


private:
    HdMap *map_;



};


class HdMapToolNeutral:public HdMapTool
{
public:
    HdMapToolNeutral(HdMap *map);

    static HdMapToolNeutral* makeMapToolNeutral(HdMap* map);
};

//class HdFactoryMapTool
//{
//public:
//    virtual ~HdFactoryMapTool();
//    virtual QgsMapTool* makeTool(QgsMapCanvas *canvas)=0;
//};


class HdMapToolClickPoint: public HdMapTool
{
    Q_OBJECT
public:
    HdMapToolClickPoint(HdMap *map):HdMapTool(map){}

signals:
    void clickDone(const QPointF &pt);
    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

class HdMapToolRectangularSelection:public HdMapTool
{
    Q_OBJECT
public:
    HdMapToolRectangularSelection(HdMap *map);

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

class HdMapToolLinearDrawing: public HdMapTool
{
    Q_OBJECT
public:
    HdMapToolLinearDrawing(HdMap *map,QgsWkbTypes::GeometryType geometryType);

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

class HdMapToolLinearSelection:public HdMapTool
{
    Q_OBJECT
public:
    HdMapToolLinearSelection(HdMap *map);

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

class HdMapToolItemSelection:public HdMapTool
{
    Q_OBJECT
public:
    HdMapToolItemSelection(HdMap *map,int itemType);

signals:
    void foundItem(QGraphicsItem *item);
private:
    int itemType;

    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e);
};



#endif // HDMAPTOOL_H
