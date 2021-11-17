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
#include <QToolButton>

#include "reosgui.h"
#include "reosmodule.h"
#include "reosmapitem.h"
#include "reosmapextent.h"

class QDockWidget;
class QgsMapCanvas;
class ReosMapCursorPosition;
class ReosGisEngine;
class ReosMapTool;
class ReosMapToolDrawExtent;

class REOSGUI_EXPORT ReosMap: public ReosModule
{
    Q_OBJECT
  public:
    ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget );
    ~ReosMap();

    QWidget *mapCanvas() const;
    void refreshCanvas();

    ReosGisEngine *engine() const;
    QString mapCrs() const;

    //! Sets the map tool to the default one
    void setDefaultMapTool();

    void setExtent( const ReosMapExtent &extent );
    void setCenter( const QPointF &center );
    void setCenter( const ReosSpatialPosition &center );
    ReosMapExtent extent() const;

    QList<QAction *> mapToolActions();

    QDockWidget *temporalControllerDockWidget();


  signals:
    //! emitted when the mouse cursor moves on the map cavans.
    void cursorMoved( const QPointF &point );
    void readProject( const QDomDocument &doc );
    void crsChanged( const QString &crs );
    void extentChanged();

  private slots:
    void setCrs( const QString &crs );

  private:
    ReosGisEngine *mEngine;
    QPointer<QGraphicsView> mCanvas = nullptr;
    QDockWidget *mTemporalDockWidget = nullptr;

    QAction *mActionNeutral = nullptr;
    ReosMapTool *mDefaultMapTool = nullptr;

    QAction *mActionZoom = nullptr;
    ReosMapToolDrawExtent *mZoomMapTool = nullptr;

    QAction *mActionZoomIn = nullptr;
    QAction *mActionZoomOut = nullptr;
    QAction *mActionPreviousZoom = nullptr;
    QAction *mActionNextZoom = nullptr;
    QAction *mTemporalControllerAction = nullptr;
};


class REOSGUI_EXPORT ReosMapCursorPosition : public QWidget
{
    Q_OBJECT
  public:
    ReosMapCursorPosition( ReosMap *map, QWidget *parent = nullptr );
    ~ReosMapCursorPosition();

  private slots:
    void setPosition( const QPointF &p );
    void setCrs( const QString &crz );

  private:
    QLabel *mCoordinates;
    QLabel *mCrs;
};



#endif // REOSMAP_H
