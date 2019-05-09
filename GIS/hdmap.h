/***************************************************************************
                      hdmap.h
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

#ifndef HDMAP_H
#define HDMAP_H

#include <qgsproject.h>

#include "../Reos/reosmodule.h"
#include "../Reos/reosencodedelement.h"

#include "hdmaptool.h"

class MyMapCanvas:public QgsMapCanvas
{
public:
    MyMapCanvas():QgsMapCanvas() {}
    virtual ~MyMapCanvas()
    {
    }
};


class HdCursorPosition;

class HdMap: public ReosModule
{
    Q_OBJECT
public:
    HdMap(QObject *parent=nullptr);
    ~HdMap() override;

    QgsMapCanvas *getMapCanvas() const;
//    QgsMapTool *makeTool(HdFactoryMapTool *factory) const;
    void setMapTool(HdMapTool *tool);
    QgsMapTool *getMaptool() const;

    QgsCoordinateReferenceSystem getCoordinateReferenceSystem();

    QWidget* getCursorPosition();

    QRectF getMapExtent() const {return canvas_->extent().toRectF();}
    void setMapExtent(QRectF extent) {
        canvas_->setExtent(QgsRectangle(extent));
    }

    void setMapSavedExtent(QRectF extent) {
        savedExtent=extent;
    }

    QByteArray encode() const
    {
        ReosEncodedElement encodedMap(QStringLiteral("Map"));
        //encodedMap.addData(QStringLiteral("Current extent"),canvas_->extent().toRectF());
        return encodedMap.encode();
    }

    void decode(QByteArray &byteArray)
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

    void setToSaveExtent()
    {
        canvas_->setExtent(savedExtent);
    }

    void saveMapExtent()
    {
        savedExtent=canvas_->extent();
    }



public slots:
    void unsetMapTool(HdMapTool *tool);
    void unsetMapTool();
    void askUnsetMapTool();
    void refreshMap();
    void crsChanged();

private:
    QgsMapCanvas *canvas_;
    HdCursorPosition* cursorPosition;
    HdMapToolNeutral *mapToolNeutral;
    HdMapTool* currentMapTool=nullptr;
    QgsRectangle savedExtent;

public:

    // ReosModule interface
    QWidget *getWidget() const override;

};


class HdCursorPosition : public QLabel
{
    Q_OBJECT
public:
    HdCursorPosition(QgsMapCanvas* canvas);
    ~HdCursorPosition()
    {
    }

private:
    QgsMapCanvas *canvas_;

public slots:
    void actualisePosition(QgsPointXY p);

};



#endif // HDMAP_H
