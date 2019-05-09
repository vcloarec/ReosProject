/***************************************************************************
                      hdmap.cpp
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

#include "hdmap.h"

class mQgsMapCanvas:public QgsMapCanvas
{
public:
    mQgsMapCanvas(QWidget *parent=nullptr):QgsMapCanvas(parent) {}
    ~mQgsMapCanvas() {}

};

HdMap::HdMap(QObject *parent):ReosModule(parent),
    canvas_(new QgsMapCanvas()),
    cursorPosition(new HdCursorPosition(canvas_)),
    mapToolNeutral(new HdMapToolNeutral(this))
{

}

HdMap::~HdMap()
{

}

QgsMapCanvas *HdMap::getMapCanvas() const {return canvas_;}



void HdMap::setMapTool(HdMapTool *tool)
{
    if (currentMapTool)
        disconnect(currentMapTool,&HdMapTool::arret,this,&HdMap::askUnsetMapTool);
    currentMapTool=tool;
    canvas_->setMapTool(tool);
    connect(currentMapTool,&HdMapTool::arret,this,&HdMap::askUnsetMapTool);

}

QgsMapTool *HdMap::getMaptool() const
{
    return canvas_->mapTool();
}

QgsCoordinateReferenceSystem HdMap::getCoordinateReferenceSystem()
{
    if (canvas_)
        return canvas_->mapSettings().destinationCrs();
    else
        return QgsCoordinateReferenceSystem();
}

QWidget *HdMap::getCursorPosition() {return cursorPosition;}


void HdMap::unsetMapTool(HdMapTool *tool)
{
    if (currentMapTool==tool)
    {
        disconnect(currentMapTool,&HdMapTool::arret,this,&HdMap::askUnsetMapTool);
        currentMapTool=nullptr;
    }
}

void HdMap::unsetMapTool()
{
    if (currentMapTool)
        disconnect(currentMapTool,&HdMapTool::arret,this,&HdMap::askUnsetMapTool);
    currentMapTool=mapToolNeutral;
    canvas_->setMapTool(mapToolNeutral);
}

void HdMap::askUnsetMapTool()
{
    unsetMapTool();
}

void HdMap::refreshMap() {canvas_->refresh();}

void HdMap::crsChanged()
{
    canvas_->setDestinationCrs(QgsProject::instance()->crs());
}

QWidget *HdMap::getWidget() const
{
    return canvas_;
}



HdCursorPosition::HdCursorPosition(QgsMapCanvas *canvas):QLabel(canvas),canvas_(canvas)
{
    QRect rect(0,0,150,15);
    setGeometry(rect);
    connect(canvas,&QgsMapCanvas::xyCoordinates,this,&HdCursorPosition::actualisePosition);
}

void HdCursorPosition::actualisePosition(QgsPointXY p)
{
    QString position=tr("Coordonnées : ");
    position.append(QString::number(p.x(),'f',2));
    position.append(" : ");
    position.append(QString::number(p.y(),'f',2));
    position.append("  ");
    setText(position);
}
