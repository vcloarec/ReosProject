/***************************************************************************
                      reosmap.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSMAP_H
#define REOSMAP_H

#include <QLabel>
#include <QDomDocument>
#include <QPointer>
#include <QGraphicsView>

#include "reosmodule.h"
#include "reosmapitem.h"
//#include "../Reos/reosencodedelement.h"

class QgsMapCanvas;
class ReosMapCursorPosition;
class ReosGisEngine;

class ReosMap: public ReosModule
{
    Q_OBJECT
  public:
    ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget = nullptr );
    ~ReosMap();

//    //QgsMapCanvas *getMapCanvas() const;

//    void setMapTool( ReosMapTool *tool );
//    ReosMapTool *getMaptool() const;

//    QgsCoordinateReferenceSystem getCoordinateReferenceSystem();

//

//    QRectF getMapExtent() const {return canvas_->extent().toRectF();}
//    void setMapExtent( QRectF extent );

//    QByteArray encode() const;
//    void decode( QByteArray &byteArray );
//    void setToSaveExtent();
//    void saveMapExtent();
//    void setMapSavedExtent( QRectF extent );

    QWidget *mapCanvas() const;
    void refreshCanvas();

    ReosGisEngine *engine() const;

  public slots:
    //    void unsetMapTool( ReosMapTool *tool );
//    void unsetMapTool();
//    void stopMapTool()
//    {
//      if ( currentMapTool )
//      {
//        bool inProgress = currentMapTool->isInProgress();
//        currentMapTool->askForEscape();
//        if ( !inProgress )
//          unsetMapTool();
//      }

//    }
//    void askUnsetMapTool();
//    void refreshMap();
//    void crsChanged();

  signals:
    //! emitted when the mouse cursor moves on the map cavans.
    void cursorMoved( const QPointF &point );
    void readProject( const QDomDocument &doc );

  private:
    ReosGisEngine *mEngine;
    QPointer<QGraphicsView> mCanvas = nullptr;

//    HdCursorPosition *cursorPosition;
//    HdMapToolNeutral *mapToolNeutral;
//    ReosMapTool *currentMapTool = nullptr;
//    QgsRectangle savedExtent;

};


class ReosMapCursorPosition : public QLabel
{
    Q_OBJECT
  public:
    ReosMapCursorPosition( ReosMap *map, QWidget *parent = nullptr );
    ~ReosMapCursorPosition()
    {
    }

  private slots:
    void setPosition( const QPointF &p );
};



#endif // REOSMAP_H
