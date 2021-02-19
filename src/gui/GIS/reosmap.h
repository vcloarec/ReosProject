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

#include "reosgui.h"
#include "reosmodule.h"
#include "reosmapitem.h"
//#include "../Reos/reosencodedelement.h"

class QgsMapCanvas;
class ReosMapCursorPosition;
class ReosGisEngine;
class ReosMapTool;

class REOSGUI_EXPORT ReosMap: public ReosModule
{
    Q_OBJECT
  public:
    ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget = nullptr );
    ~ReosMap();

    QWidget *mapCanvas() const;
    void refreshCanvas();

    ReosGisEngine *engine() const;

    QString mapCrs() const;

    //! Sets the map tool to the default one
    void setDefaultMapTool();

  signals:
    //! emitted when the mouse cursor moves on the map cavans.
    void cursorMoved( const QPointF &point );
    void readProject( const QDomDocument &doc );

  private slots:
    void setCrs( const QString &crs );

  private:
    ReosGisEngine *mEngine;
    QPointer<QGraphicsView> mCanvas = nullptr;

    ReosMapTool *mDefaultMapTool = nullptr;

};


class REOSGUI_EXPORT ReosMapCursorPosition : public QLabel
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
