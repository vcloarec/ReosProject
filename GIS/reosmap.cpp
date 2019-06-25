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

#include "reosmap.h"


ReosMap::ReosMap(QObject *parent):ReosModule(parent),
    canvas_(new QgsMapCanvas()),
    cursorPosition(new HdCursorPosition(canvas_)),
    mapToolNeutral(new HdMapToolNeutral(this))
{
    canvas_->setExtent(QgsRectangle(0,0,200,200));
    canvas_->setObjectName("map canvas");
}

ReosMap::~ReosMap()
{

}

QgsMapCanvas *ReosMap::getMapCanvas() const {return canvas_;}



void ReosMap::setMapTool(ReosMapTool *tool)
{
    if (currentMapTool)
        disconnect(currentMapTool,&ReosMapTool::stop,this,&ReosMap::askUnsetMapTool);
    currentMapTool=tool;
    canvas_->setMapTool(tool);
    connect(currentMapTool,&ReosMapTool::stop,this,&ReosMap::askUnsetMapTool);

}

ReosMapTool *ReosMap::getMaptool() const
{
    return currentMapTool;
}

QgsCoordinateReferenceSystem ReosMap::getCoordinateReferenceSystem()
{
    if (canvas_)
        return canvas_->mapSettings().destinationCrs();
    else
        return QgsCoordinateReferenceSystem();
}

QWidget *ReosMap::getCursorPosition() {return cursorPosition;}

void ReosMap::setMapExtent(QRectF extent) {
    canvas_->setExtent(QgsRectangle(extent));
}

void ReosMap::setMapSavedExtent(QRectF extent) {
    savedExtent=extent;
}

QByteArray ReosMap::encode() const
{
    ReosEncodedElement encodedMap(QStringLiteral("Map"));
    encodedMap.addData(QStringLiteral("Current extent"),canvas_->extent().toRectF());
    return encodedMap.encode();
}

void ReosMap::decode(QByteArray &byteArray)
{
    ReosEncodedElement encodedMap(byteArray);
    QRectF extent;
    if (encodedMap.getData(QStringLiteral("Current extent"),extent))
    {
        if (extent!=QRectF())
        {
            savedExtent=QgsRectangle(extent);
            canvas_->setExtent(extent);
        }
    }

}

void ReosMap::setToSaveExtent()
{
    canvas_->setExtent(savedExtent);
}

void ReosMap::saveMapExtent()
{
    savedExtent=canvas_->extent();
}


void ReosMap::unsetMapTool(ReosMapTool *tool)
{
    if (currentMapTool==tool)
    {
        disconnect(currentMapTool,&ReosMapTool::stop,this,&ReosMap::askUnsetMapTool);
        currentMapTool=nullptr;
    }
}

void ReosMap::unsetMapTool()
{
    if (currentMapTool)
        disconnect(currentMapTool,&ReosMapTool::stop,this,&ReosMap::askUnsetMapTool);
    currentMapTool=mapToolNeutral;
    canvas_->setMapTool(mapToolNeutral);
}

void ReosMap::askUnsetMapTool()
{
    unsetMapTool();
}

void ReosMap::refreshMap() {canvas_->refresh();}

void ReosMap::crsChanged()
{
    canvas_->setDestinationCrs(QgsProject::instance()->crs());
}

QWidget *ReosMap::getWidget() const
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
