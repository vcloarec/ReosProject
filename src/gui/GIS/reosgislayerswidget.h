/***************************************************************************
                      reosgislayerwidget.h
                     --------------------------------------
Date                 : 17-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSGISLAYERSWIDGET_H
#define REOSGISLAYERSWIDGET_H

#include <QWidget>
#include <QToolBar>

#include "reosgui.h"

class QgsLayerTreeView;
class QgsLayerTreeViewIndicator;
class ReosGisEngine;
class ReosMap;

class REOSGUI_EXPORT ReosGisLayersWidget: public QWidget
{
    Q_OBJECT
  public:
    ReosGisLayersWidget( ReosGisEngine *engine,
                         ReosMap *map,
                         QWidget *parent = nullptr );

    bool isLayerDigitalElevationModel( const QString &layerId );

  public slots:
    void registerCurrentLayerAsDigitalElevationModel();
    void unRegisterCurrentLayerAsDigitalElevationModel();

    void layerProperties();

  private slots:
    void onLoadQGISProject();
    void onLoadVectorLayer();
    void onLoadRasterLayer();
    void onLoadMeshLayer();
    void onTreeLayerDoubleClick();
    void onSetCrs();
    void updateLayerInsertionPoint() const;
    void onGISEngineUpdated();
    void updateIndicator();

  private:
    ReosGisEngine *mGisEngine;
    ReosMap *mMap;

    QgsLayerTreeView *mTreeView = nullptr;
    QgsLayerTreeViewIndicator *mDemIndicator = nullptr;

    QToolBar *mToolBar;

    QAction *mActionLoadQGISProject;
    QAction *mActionLoadVectorLayer;
    QAction *mActionLoadRasterLayer;
    QAction *mActionLoadMeshLayer;
    QAction *mActionSetProjectCrs;
};

#endif // REOSGISLAYERSWIDGET_H
