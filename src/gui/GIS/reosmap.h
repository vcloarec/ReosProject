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
#include <QGraphicsView>
#include <QToolButton>
#include <QMutex>

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
class ReosGeometryStructure;
class ReosRenderedObject;
class ReosObjectRenderer;


class ReosRendererObjectHandler_p;
class ReosRendererObjectHandler : public QObject
{
    Q_OBJECT
  public:
    ReosRendererObjectHandler( QGraphicsView *view );
    ~ReosRendererObjectHandler();

    void init();

    void makeObsolete( ReosRenderedObject *renderedObject );

    void startRender( ReosRenderedObject *renderedObject );

    QImage image( ReosRenderedObject *renderedObject );

    void clearObject( ReosRenderedObject *renderedObject );

    void stopRendering( ReosRenderedObject *renderedObject );

  private slots:
    void onRendererFinished();
    void updateViewParameter();

  private:
    std::unique_ptr<ReosRendererObjectHandler_p> d;

    ReosRenderedObject *rendererToObject( ReosObjectRenderer *renderer ) const;
    ReosObjectRenderer *objectToRenderer( ReosRenderedObject *o ) const;
    QImage transformImage( ReosRenderedObject *renderedObject );

    //! Returns if a cache is present, event if not up to date
    bool hasCache( ReosRenderedObject *renderedObject );

    //! Returns if a updtodate cache is present
    bool hasUpToDateCache( ReosRenderedObject *renderedObject );
};



class REOSGUI_EXPORT ReosMap: public ReosModule
{
    Q_OBJECT
  public:
    ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget );
    ~ReosMap();

    QWidget *mapCanvas() const;

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

    void initialize();

    void addSnappableStructure( ReosGeometryStructure *structure );
    void removeSnappableStructure( ReosGeometryStructure *structure );

    void addExtraRenderedObject( ReosRenderedObject *obj );
    void removeExtraRenderedObject( ReosRenderedObject *obj );

  signals:
    //! emitted when the mouse cursor moves on the map cavans.
    void cursorMoved( const QPointF &point );
    void readProject( const QDomDocument &doc );
    void crsChanged( const QString &crs );
    void extentChanged();

  public slots:
    void refreshCanvas();

  private slots:
    void setCrs( const QString &crs );
    void prepareExtraRenderedObject();
    void drawExtraRendering( QPainter *painter );
    void onExtraObjectRenderedFinished();
    void onExtraObjectRequestRepaint();

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
    QAction *mEnableSnappingAction = nullptr;

    ReosRendererObjectHandler mExtraRenderedObjectHandler;
    QList<ReosRenderedObject *> mExtraRenderedObjects;
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
