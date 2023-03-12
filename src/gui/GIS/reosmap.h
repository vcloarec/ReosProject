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
class ReosMapToolSelectMapItem;
class ReosGeometryStructure;
class ReosRenderedObject;
class ReosObjectRenderer;
class ReosDuration;
class ReosTemporalController_p;
class ReosRendererObjectMapTimeStamp;
class ReosColorRampMapLegendItem;
class ReosColorShaderSettings;


class ReosRendererObjectHandler_p;

class ReosRendererObjectHandler : public QObject
{
    Q_OBJECT
  public:
    explicit ReosRendererObjectHandler( QGraphicsView *view );
    ~ReosRendererObjectHandler();

    void init();

    void makeRenderingObsolete( ReosRenderedObject *renderedObject );

    void startRender( ReosRenderedObject *renderedObject );

    QImage image( ReosRenderedObject *renderedObject );

    void clearObject( ReosRenderedObject *renderedObject );

  signals:
    void requestCanvasRefesh();

  private slots:
    void onRendererFinished();
    void updateViewParameter();

  private:
    std::unique_ptr<ReosRendererObjectHandler_p> d;
    QImage transformImage( ReosRenderedObject *renderedObject );

    //! Returns if a cache is present, event if not up to date
    bool hasCache( ReosRenderedObject *renderedObject );

    //! Returns if a updtodate cache is present
    bool hasUpToDateCache( ReosRenderedObject *renderedObject, ReosRendererObjectMapTimeStamp *mapTimeStamp = nullptr );

    void destroyRenderer( ReosObjectRenderer *renderer );
};



class REOSGUI_EXPORT ReosMap: public ReosModule
{
    Q_OBJECT
  public:
    explicit ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget = nullptr );
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
    void removeAllExtraRendererObjects();

    const QObject *temporalController() const;

    void setTimeStep( const ReosDuration &timeStep );
    void setTemporalRange( const QDateTime &startTime, const QDateTime &endTime );
    ReosDuration timeStep() const;
    QDateTime currentTime() const;

    void activateOpenStreetMap();

    void addSelectToolTarget( const QString &targetDescription );

    ReosMapToolSelectMapItem *defaultMapTool() const;

    void deactivateCurrentTool();

    static QString staticModuleName() {return QStringLiteral( "map" );}

  signals:
    //! emitted when the mouse cursor moves on the map cavans.
    void cursorMoved( const QPointF &point );
    void readProject( const QDomDocument &doc );
    void crsChanged( const QString &crs );
    void extentChanged();
    void timeChanged( const QDateTime &time );

    void mapItemFound( ReosMapItem *item, const QPointF &point );
    void mapItemFoundDoubleClick( ReosMapItem *item, const QPointF &point );

  public slots:
    void refreshCanvas();

  private slots:
    void setCrs( const QString &crs );
    void onMapStartRendering();
    void onMapRenderingFinish();
    void drawExtraRendering( QPainter *painter );
    void onExtraObjectRequestRepaint();
    void updateLegend() const;
    void resizeLegend() const;

  private:
    ReosGisEngine *mEngine;
    QPointer<QGraphicsView> mCanvas = nullptr;
    QDockWidget *mTemporalDockWidget = nullptr;
    ReosTemporalController_p *mTemporalControler = nullptr;

    QAction *mActionNeutral = nullptr;
    ReosMapToolSelectMapItem *mDefaultMapTool = nullptr;

    QAction *mActionZoom = nullptr;
    ReosMapToolDrawExtent *mZoomMapTool = nullptr;

    QAction *mActionPan = nullptr;
    QAction *mActionZoomIn = nullptr;
    QAction *mActionZoomOut = nullptr;
    QAction *mActionPreviousZoom = nullptr;
    QAction *mActionNextZoom = nullptr;
    QAction *mTemporalControllerAction = nullptr;
    QAction *mEnableSnappingAction = nullptr;
    QAction *mActionEnableLegend = nullptr;

    ReosRendererObjectHandler mExtraRenderedObjectHandler;
    QList<ReosRenderedObject *> mExtraRenderedObjects;
    QMap<QString, ReosColorRampMapLegendItem *> mColorRampLegendSettings;

    ReosColorRampMapLegendItem *mLegendItem;

    bool mMapIsRendering = false;
    bool mNeedOtherRefresh = false;

    void prepareExtraRenderedObject();
};


class REOSGUI_EXPORT ReosMapCursorPosition : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosMapCursorPosition( ReosMap *map, QWidget *parent = nullptr );
    ~ReosMapCursorPosition();

  private slots:
    void setPosition( const QPointF &p );
    void setCrs( const QString &crz );

  private:
    QLabel *mCoordinates;
    QLabel *mCrs;
};

class REOSGUI_EXPORT ReosDataVizMapWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosDataVizMapWidget( QWidget *parent = nullptr );
    ~ReosDataVizMapWidget();

    void addRenderedDataObject( ReosRenderedObject *object );
    void removeRenderedObject( ReosRenderedObject *object );
    void removeAllRenderedObjects();

    void setTimeExtent( const QDateTime &startTime, const QDateTime &endTime );
    void setTimeStep( const ReosDuration &timeStep );
    void setExtent( const ReosMapExtent &extent );

    void showExtentOnMap( const ReosMapExtent &extent );
    void hideExtentOnMap();

    ReosMap *map();

  private:
    ReosMap *mMap = nullptr;
    ReosMapPolygon mExtentOnMap;
};


#endif // REOSMAP_H
