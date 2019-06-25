/***************************************************************************
                      hdgestionnairesig.h
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

#ifndef HDGISMANAGER_H
#define HDGISMANAGER_H

#include <memory>

#include <QObject>
#include <QDockWidget>
#include <QToolBar>
#include <QVBoxLayout>
#include <QMessageBox>
#include <QFileDialog>
#include <QMenu>
#include <gdal_frmts.h>

#include <qgsrubberband.h>
#include <qgsmapcanvas.h>
#include <qgsmaptool.h>
#include <qgsproviderregistry.h>
#include <qgslayertreeview.h>
#include <qgsproject.h>
#include <qgslayertreemapcanvasbridge.h>
#include <qgslayertreemodel.h>
#include <qgslayertree.h>
#include <qgslayertreeview.h>
#include <qgsvectorlayer.h>
#include <qgsrasterlayer.h>
#include <qgsmeshlayer.h>
#include <qgsgeometry.h>
#include <qgscolorschemeregistry.h>

#include <qgsmaplayercombobox.h>
#include <qgsvectorfilewriter.h>
#include <qgsprojectionselectiondialog.h>
#include <qgsprojectionselectionwidget.h>
#include <qgsrasterlayerproperties.h>
#include <qgsmeshlayerproperties.h>

//#include <qgisapp.h>



#include "QGis_app/qgis_app.h"
#include "reosmap.h"
#include "hdcrsdialogselection.h"
#include "hdvectorlayerpropertiesdialog.h"
#include "../Reos/reosdialogbox.h"
#include "../Reos/reossettings.h"
#include "../Reos/reosencodedelement.h"
#include "../Reos/reosmodule.h"


#if VERSION_INT<=30699
        #define LAYER_TYPE QgsMapLayer::LayerType
        #define RASTER_LAYER_TYPE QgsMapLayer::RasterLayer
        #define VECTOR_LAYER_TYPE QgsMapLayer::VectorLayer
        #define MESH_LAYER_TYPE QgsMapLayer::MeshLayer
#else
        #define LAYER_TYPE QgsMapLayerType
        #define RASTER_LAYER_TYPE QgsMapLayerType::RasterLayer
        #define VECTOR_LAYER_TYPE QgsMapLayerType::VectorLayer
        #define MESH_LAYER_TYPE QgsMapLayerType::MeshLayer
#endif



class HdTreeLayerSIGView:public QgsLayerTreeView
{
    // QWidget interface
protected:
    void dragEnterEvent(QDragEnterEvent *event) override;

};

class HdTreeLayerModel:public QgsLayerTreeModel
{
public:
    HdTreeLayerModel(QgsLayerTree *rootNode, QObject *parent = nullptr ):QgsLayerTreeModel(rootNode,parent) {}

protected:


    // QAbstractItemModel interface
public:
    QStringList mimeTypes() const override;

    bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) override;

    QVariant data(const QModelIndex &index, int role) const override;

};


class HdManagerSIG:public ReosModule
{
    Q_OBJECT
public:
    HdManagerSIG(ReosMap* map, ReosModule *parent=nullptr);
    ~HdManagerSIG() override {}

    QgsRasterLayer *getRasterLayer();
    QWidget* createCRSDisplay(QWidget *parent);

    QMenu *getContextMenu();

    void loadGISProject();
    void saveGISProject();

    QgsRectangle transformExtentFrom(const QgsRectangle &extent, const QgsCoordinateReferenceSystem crsSource);                                                                                                                                                                                                                            
    QgsRectangle transformExtentTo(const QgsRectangle &extent, const QgsCoordinateReferenceSystem crsDest);


    void transformTo(QgsAbstractGeometry *sourceGeometry,const QgsCoordinateReferenceSystem crsDest);
    void transformFrom(QgsAbstractGeometry *sourceGeometry,const QgsCoordinateReferenceSystem crsDest);

    ReosMap *getMap() const;

    QgsCoordinateReferenceSystem getLayerCRS(QString name,QString URI="");
    QgsMapLayer *getLayer(QString name,QString URI="");

    QString getGISFileName() const;
    void setGISFileName(const QString &value);

    QByteArray encode();

    void decode(QByteArray &ba);

    void clear();

    bool addLayer(QgsMapLayer *layer);

signals:
    void currentLayerChanged(QgsMapLayer *layer);
    void layerHasToBeRemoved(QgsMapLayer *layer);
    void layerHasToBeUpdated(QgsMapLayer *layer);

public slots:
    void openProjectSIG();
    void newProjectSIG();
    void loadVectorielLayer();
    void loadRasterLayer();
    void removeLayer(); /*!< remove the curren layer*/
    void removeSelectedLayers(); /*!< remove all the selected layer*/
    void zoomExtentToLayer();
    void CRSSelection();
    void setCRS(const QgsCoordinateReferenceSystem &newCrs);
    void setExtentAfterLoading();

private:
    ReosMap *map_;
    QgsCoordinateReferenceSystem crs;
    QgsCoordinateReferenceSystem deFaultCrs;
    HdTreeLayerSIGView *treeLayerView_;
    QgsLayerTreeMapCanvasBridge* bridgeTreeMap_;
    HdTreeLayerModel *treemodel_;
    QString pluginPath;
    QString GISFileName;

    QWidget *controlPannel;
    QWidget *crsDisplay;

    QAction *actionNewProjectSIG;
    QAction *actionOpenProjectSIG;
    QAction *actionLoadVectorielLayer;
    QAction *actionLoadRasterLayer;
    QAction *actionRemoveLayer;
    QAction *actionZoomLayerExtent;
    QAction *actionLayerProperties;
    QAction *actionCRSSelection;
    QAction *actionCRSSelectionWithText;


    QMenu* getMenuForOneRasterLayer();
    QMenu* getMenuForOneVectorLayer();
    QMenu *getMenuForSeveralLayers();


    QList<QgsRasterLayer*> getAllRasterLayers();

    void controlLayerCRS(QgsMapLayer *layer);
    void setTextActionCRS();
    void callPropertiesLayer(QgsMapLayer *layer);

private slots:
    void layerProperties();
    void layerPropertiesByIndex(QModelIndex index);



    // ReosModule interface
public:
    QWidget *getWidget() const override;


};



class HdSigTreeViewContextMenuProvider: public QgsLayerTreeViewMenuProvider
{
public:
    HdSigTreeViewContextMenuProvider(HdManagerSIG *manager):manager(manager){}
  
    // QgsLayerTreeViewMenuProvider interface
public:
    QMenu *createContextMenu() override;

private:
    HdManagerSIG *manager;
};

#endif // HDGISMANAGER_H
