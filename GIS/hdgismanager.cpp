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

#include "hdgismanager.h"


HdManagerSIG::HdManagerSIG(HdMap *map, ReosModule *parent): ReosModule(parent),map_(map),
    treeLayerView_(new HdTreeLayerSIGView()),
#ifdef _DEBUG
    pluginPath("pluginsQGISDebug"),
#else
    pluginPath("pluginsQGIS"),
#endif
    controlPannel(new QWidget()),
    actionNewProjectSIG(new QAction(QPixmap("://toolbar/NouveauDoc.png"),tr("Nouveau projet SIG"),this)),
    actionOpenProjectSIG(new QAction(QPixmap("://toolbar/ouvrirProjetQGIS.png"),tr("Ouvrir projet QGIS"),this)),
    actionLoadVectorielLayer(new QAction(QPixmap("://toolbar/ajouteVectoriel.png"),tr("Ajoute couche vectoriel"),this)),
    actionLoadRasterLayer(new QAction(QPixmap("://toolbar/ajouteRaster.png"),tr("Ajoute couche raster"),this)),
    actionRemoveLayer(new QAction(QPixmap("://toolbar/supprime.png"),tr("Enleve couche"),this)),
    actionZoomLayerExtent(new QAction(QPixmap("://toolbar/etendu.png"),tr("Zoom étendu sur couche"),this)),
    actionLayerProperties(new QAction(tr("Propriété de la couche"),this)),
    actionCRSSelection(new QAction(QPixmap("://Qgis/mActionSetProjection.png"),tr("Sélection du système de coordonnée"),this)),
    actionCRSSelectionWithText(new QAction(this))
{
    QgsProviderRegistry::instance(pluginPath);
    QgsApplication::setPkgDataPath("./");


    deFaultCrs=QgsCoordinateReferenceSystem(QStringLiteral( "EPSG:4326" ));
    setCRS(deFaultCrs);

    toolBar=new QToolBar(tr("Gestion couche QGIS"));


    treemodel_=new HdTreeLayerModel(QgsProject::instance()->layerTreeRoot());
    treemodel_->setFlags(QgsLayerTreeModel::ShowLegendAsTree|
                         QgsLayerTreeModel::ShowLegend|
                         QgsLayerTreeModel::AllowNodeRename|
                         QgsLayerTreeModel::AllowNodeChangeVisibility|
                         QgsLayerTreeModel::AllowNodeReorder);

    bridgeTreeMap_=new QgsLayerTreeMapCanvasBridge(QgsProject::instance()->layerTreeRoot(),map->getMapCanvas(),this);
    treeLayerView_->setModel(treemodel_);
    treeLayerView_->setDragDropMode(QAbstractItemView::DragDrop);
    treeLayerView_->setAcceptDrops(true);
    treeLayerView_->setDragEnabled(true);
    treeLayerView_->setMenuProvider(new HdSigTreeViewContextMenuProvider(this));

    toolBar->addAction(actionNewProjectSIG);
    toolBar->addAction(actionOpenProjectSIG);
    toolBar->addAction(actionLoadVectorielLayer);
    toolBar->addAction(actionLoadRasterLayer);
    toolBar->addAction(actionRemoveLayer);
    toolBar->addAction(actionZoomLayerExtent);
    toolBar->addAction(actionCRSSelection);


    QVBoxLayout *layoutControl=new QVBoxLayout();
    layoutControl->addWidget(toolBar);
    layoutControl->addWidget(treeLayerView_);
    controlPannel->setLayout(layoutControl);




    connect(actionNewProjectSIG,&QAction::triggered,this,&HdManagerSIG::newProjectSIG);
    connect(actionOpenProjectSIG,&QAction::triggered,this,&HdManagerSIG::openProjectSIG);
    connect(actionLoadVectorielLayer,&QAction::triggered,this,&HdManagerSIG::loadVectorielLayer);
    connect(actionLoadRasterLayer,&QAction::triggered,this,&HdManagerSIG::loadRasterLayer);
    connect(actionRemoveLayer,&QAction::triggered,this,&HdManagerSIG::removeSelectedLayers);
    connect(actionZoomLayerExtent,&QAction::triggered,this,&HdManagerSIG::zoomExtentToLayer);
    connect(actionCRSSelection,&QAction::triggered,this,&HdManagerSIG::CRSSelection);
    connect(actionCRSSelectionWithText,&QAction::triggered,this,&HdManagerSIG::CRSSelection);
    connect(actionLayerProperties,&QAction::triggered,this,&HdManagerSIG::layerProperties);
    connect(treeLayerView_,&QAbstractItemView::doubleClicked,this,&HdManagerSIG::layerPropertiesByIndex);
    connect(treeLayerView_,&QgsLayerTreeView::currentLayerChanged,this,&HdManagerSIG::currentLayerChanged);

    connect(QgsProject::instance(),&QgsProject::crsChanged,map_,&HdMap::crsChanged);

    setTextActionCRS();

    QgsApplication::colorSchemeRegistry()->addDefaultSchemes();

}



QgsRasterLayer *HdManagerSIG::getRasterLayer()
{
    QgsMapLayerComboBox *comboBox=new QgsMapLayerComboBox;
    comboBox->setFilters(QgsMapLayerProxyModel::RasterLayer);
    ReosDialogBox dial(comboBox);

    dial.exec();

    return static_cast<QgsRasterLayer*>(comboBox->currentLayer());
}

QWidget *HdManagerSIG::createCRSDisplay(QWidget *parent)
{
    crsDisplay=new QWidget(parent);
    crsDisplay->setLayout(new QHBoxLayout);
    QToolBar *toolBar=new QToolBar(crsDisplay);
    toolBar->addAction(actionCRSSelectionWithText);
    crsDisplay->layout()->addWidget(new QLabel(tr("Système de coordonnées : ")));
    crsDisplay->layout()->addWidget(toolBar);
    return crsDisplay;
}

QMenu *HdManagerSIG::getContextMenu()
{
    if (treeLayerView_->selectedLayers().count()==0)
        return nullptr;

    if (treeLayerView_->selectedLayers().count()==1)
    {
        if (treeLayerView_->currentLayer()->type()==QgsMapLayerType::RasterLayer)
            return getMenuForOneRasterLayer();

        if (treeLayerView_->currentLayer()->type()==QgsMapLayerType::VectorLayer)
            return getMenuForOneVectorLayer();

        return nullptr;

    }

    return getMenuForSeveralLayers();

}

void HdManagerSIG::openProjectSIG()
{

    if (treemodel_->rowCount()!=0)
    {
        QMessageBox dia(QMessageBox::Question,tr("Ouverture projet SIG"),tr("Cela enlèvera les couches actuelles. Voulez vous continuer ?"),
                        QMessageBox::Ok|QMessageBox::Cancel);
        dia.exec();
        if(dia.result()!=QMessageBox::Ok)
            return;
    }

    ReosSettings settings;
    QString path=settings.value(QStringLiteral("/Path/Project")).toString();
    QString nomFichierProjet = QFileDialog::getOpenFileName(controlPannel,tr("Ouverture d'une fichier de projet QGis"),path,"*.qgs *.qgz");

    if (nomFichierProjet=="")
        return;
    QFileInfo fileInfo(nomFichierProjet);
    settings.setValue(QStringLiteral("/Path/Project"),fileInfo.path());

    GISFileName=nomFichierProjet;
    map_->saveMapExtent();
    loadGISProject();

}

void HdManagerSIG::newProjectSIG()
{
    QMessageBox dia(QMessageBox::Question,tr("Nouveau projet SIG"),tr("Enlever les couches actuelles ?")
                    ,QMessageBox::Ok|QMessageBox::Cancel);

    if(dia.exec())
        clear();

}

bool HdManagerSIG::addLayer(QgsMapLayer *layer)
{
    if (layer)
    {
        if (layer->isValid())
        {
            controlLayerCRS(layer);
            QgsProject::instance()->addMapLayer(layer,true,true);
            bridgeTreeMap_->mapCanvas()->refresh();
            return true;
        }
    }

    return false;
}



void HdManagerSIG::loadVectorielLayer()
{
    ReosSettings settings;
    QString path=settings.value(QStringLiteral("/Path/SIGLayer")).toString();
    QString myLayerPath= QFileDialog::getOpenFileName(controlPannel,tr("Ajouter une couche vectoriel"),path,"*.*");
    if (myLayerPath=="")
        return;



    QFileInfo fileInfo(myLayerPath);
    settings.setValue(QStringLiteral("/Path/SIGLayer"),fileInfo.path());

    QString myLayerBaseName= fileInfo.fileName();
    QString myProviderName= "ogr";

    QgsVectorLayer *layer=new QgsVectorLayer(myLayerPath, myLayerBaseName, myProviderName);


    if (!addLayer(layer))
    {
        QMessageBox::warning(controlPannel,tr("Erreur de chargement du fichier"),tr("Fichier de couche vectoriel non valide !"));
    }



}

void HdManagerSIG::loadRasterLayer()
{
    ReosSettings settings;
    QString path=settings.value(QStringLiteral("/Path/SIGLayer")).toString();
    QString myLayerPath= QFileDialog::getOpenFileName(controlPannel,tr("Ajouter une couche raster"),path,"*.*");
    if (myLayerPath=="")
        return;
    QFileInfo fileInfo(myLayerPath);
    settings.setValue(QStringLiteral("/Path/SIGLayer"),fileInfo.path());

    QgsRasterLayer *layer=new QgsRasterLayer(fileInfo.filePath(),fileInfo.fileName());

    if (!addLayer(layer))
    {
        QMessageBox::warning(controlPannel,tr("Erreur de chargement du fichier"),tr("Fichier de couche raster non valide !"));
    }



}

void HdManagerSIG::removeLayer()
{
    QgsMapLayer *layer=treeLayerView_->currentLayer();


    if (layer)
    {
        ///TODO : implemanter un moyen de vérifier si la couche st uutilisé autre part (la couche ne peut être obtenue ailleurs que par getLayer(...)
        /*if (couchesUtilisee.contains(layer->id()))
        {
            QMessageBox dia(QMessageBox::Question,tr("Enlever couche"),tr("La couche ne peut être supprimée car elle est utilisée dans le projet"),QMessageBox::Ok);
            dia.exec();
            return;
        }
        else*/
        {
            QMessageBox dia(QMessageBox::Question,tr("Enlever couche"),tr("Supprimer la couche du projet ?"),QMessageBox::Ok|QMessageBox::Cancel,controlPannel);
            if(dia.exec()==QMessageBox::Ok)
                QgsProject::instance()->layerTreeRoot()->removeLayer(layer);
            else
                return;
        }




    }
}

void HdManagerSIG::removeSelectedLayers()
{
    QList<QgsMapLayer*> listLayer=treeLayerView_->selectedLayers();

    if (listLayer.count()>0)
    {
        QString message;
        if (listLayer.count()==1)
            message=tr("Supprimer la couche du projet ?");
        else
            message=tr("Supprimer les couches du projet ?");

        QMessageBox dia(QMessageBox::Question,tr("Enlever couche"),message,QMessageBox::Ok|QMessageBox::Cancel,controlPannel);
        if (dia.exec()==QMessageBox::Ok)
        {
            for (auto layer:listLayer)
                QgsProject::instance()->removeMapLayer(layer);

            map_->getMapCanvas()->refresh();
        }

    }
}

void HdManagerSIG::zoomExtentToLayer()
{
    QgsMapLayer *layer=treeLayerView_->currentLayer();
    if (layer)
    {
        bridgeTreeMap_->mapCanvas()->setExtent(layer->extent());
        bridgeTreeMap_->mapCanvas()->refresh();
    }
}

void HdManagerSIG::CRSSelection()
{
    HdCRSDialogSelection dial;

    if (crs.isValid())
        dial.setCrs(crs);


    if(dial.exec())
        setCRS(dial.getCrs());

}

void HdManagerSIG::setCRS(const QgsCoordinateReferenceSystem &newCrs)
{
    if (!newCrs.isValid())
    {
        return;
    }

    crs=newCrs;
    QgsProject::instance()->setCrs(newCrs);
    map_->getMapCanvas()->setDestinationCrs(newCrs);
    setTextActionCRS();
}

void HdManagerSIG::setExtentAfterLoading()
{
    setCRS(crs);
    map_->setToSaveExtent();
    disconnect(bridgeTreeMap_,&QgsLayerTreeMapCanvasBridge::canvasLayersChanged,this,&HdManagerSIG::setExtentAfterLoading);
}

QString HdManagerSIG::getGISFileName() const
{
    return GISFileName;
}

void HdManagerSIG::setGISFileName(const QString &value)
{
    GISFileName = value;
}

QByteArray HdManagerSIG::encode()
{
    ReosEncodedElement encodedGISmanager(QStringLiteral("GIS Manager"));
    encodedGISmanager.addData(QStringLiteral("Map extent"),map_->getMapExtent());
    saveGISProject();

    return encodedGISmanager.encode();
}

void HdManagerSIG::decode(QByteArray &ba)
{
    ReosEncodedElement encodedGISmanager(ba);
    if (encodedGISmanager.selfDescription()!=QStringLiteral("GIS Manager"))
        return;

    QRectF extent;
    if(encodedGISmanager.getData(QStringLiteral("Map extent"),extent))
        map_->setMapSavedExtent(extent);

    loadGISProject();

}

void HdManagerSIG::clear()
{
    QgsProject::instance()->clear();
    setCRS(deFaultCrs);
}

QMenu *HdManagerSIG::getMenuForOneRasterLayer()
{
    QMenu *menu=new QMenu(controlPannel);
    menu->addAction(actionLayerProperties);
    menu->addAction(actionRemoveLayer);
    menu->addAction(actionZoomLayerExtent);

    return menu;
}

QMenu *HdManagerSIG::getMenuForOneVectorLayer()
{
    QMenu *menu=new QMenu(controlPannel);
    menu->addAction(actionLayerProperties);
    menu->addAction(actionRemoveLayer);
    menu->addAction(actionZoomLayerExtent);

    return menu;
}

QMenu *HdManagerSIG::getMenuForSeveralLayers()
{
    QMenu *menu=new QMenu(controlPannel);
    menu->addAction(actionRemoveLayer);
    return menu;
}


void HdManagerSIG::loadGISProject()
{
    connect(bridgeTreeMap_,&QgsLayerTreeMapCanvasBridge::canvasLayersChanged,this,&HdManagerSIG::setExtentAfterLoading);
    QgsProject::instance()->clear();
    QgsProject::instance()->read(GISFileName);

    setCRS(QgsProject::instance()->crs());

}

void HdManagerSIG::saveGISProject()
{
    QgsProject::instance()->write(GISFileName);
}

QgsRectangle HdManagerSIG::transformExtentFrom(const QgsRectangle &extent, const QgsCoordinateReferenceSystem crsSource)
{
    if (crs==crsSource)
        return extent;

    if (crs.isValid()&&crsSource.isValid())
    {
        QgsRectangle rectReturn;
        try {
            QgsCoordinateTransform transform(crsSource,crs,QgsProject::instance());
            rectReturn=transform.transform(extent);

        } catch (QgsCsException &e) {

            rectReturn=extent;
            error(e.what());
        }

        return rectReturn;
    }
    else
        return extent;
}

QgsRectangle HdManagerSIG::transformExtentTo(const QgsRectangle &extent, const QgsCoordinateReferenceSystem crsDest)
{
    if (crs==crsDest)
        return extent;

    if (crs.isValid()&&crsDest.isValid())
    {
        QgsRectangle rectReturn;
        try {
            QgsCoordinateTransform transform(crsDest,crs,QgsProject::instance());
            rectReturn=transform.transform(extent,QgsCoordinateTransform::ReverseTransform);

        } catch (QgsCsException &e) {
            Q_UNUSED(e);
            rectReturn=extent;
            error(e.what());
        }

        return rectReturn;
    }
    else
        return extent;
}

void HdManagerSIG::transformTo(QgsAbstractGeometry *sourceGeometry, const QgsCoordinateReferenceSystem crsDest)
{

    try {
        QgsCoordinateTransform transform(crsDest,crs,QgsProject::instance());
        sourceGeometry->transform(transform);

    } catch (QgsCsException &e) {
        Q_UNUSED(e);
        QString message=tr("erreur de transformation :");
        message.append(e.what());
        error(message);
    }

}

void HdManagerSIG::transformFrom(QgsAbstractGeometry *sourceGeometry, const QgsCoordinateReferenceSystem crsDest)
{

    try {
        QgsCoordinateTransform transform(crsDest,crs,QgsProject::instance());
        sourceGeometry->transform(transform,QgsCoordinateTransform::ReverseTransform);

    } catch (QgsCsException &e) {
        Q_UNUSED(e);
        QString message=tr("erreur de transformation :");
        message.append(e.what());
        error(message);
    }

}

HdMap *HdManagerSIG::getMap() const {return map_;}

QgsCoordinateReferenceSystem HdManagerSIG::getLayerCRS(QString name, QString URI)
{
    QgsMapLayer *layer=getLayer(name,URI);
    if (layer)
        return layer->crs();
    else
        return QgsCoordinateReferenceSystem();
}

QgsMapLayer *HdManagerSIG::getLayer(QString name, QString URI)
{
    QList<QgsLayerTreeNode*> nodeToExplore;
    nodeToExplore.append(treemodel_->rootGroup()->children());
    while ( nodeToExplore.count()>0)
    {
        QgsLayerTreeNode *current=nodeToExplore.at(0);
        nodeToExplore.removeAt(0);
        if (current->nodeType()==QgsLayerTreeNode::NodeLayer)
        {
            QgsLayerTreeLayer *nodeLayer=static_cast<QgsLayerTreeLayer*>(current);
            if (nodeLayer->layer()->name()==name && ((nodeLayer->layer()->source()==URI)||URI==""))
                return nodeLayer->layer();
        }
    }

    return nullptr;
}

QList<QgsRasterLayer *> HdManagerSIG::getAllRasterLayers()
{
    QList<QgsMapLayer *> list=treemodel_->rootGroup()->checkedLayers();

    QList<QgsRasterLayer*> listRasterLayers;

    for (auto layer:list)
    {
        if (layer->type()==QgsMapLayerType::RasterLayer)
            listRasterLayers.append(static_cast<QgsRasterLayer*>(layer));
    }

    return listRasterLayers;
}

void HdManagerSIG::controlLayerCRS(QgsMapLayer *layer)
{
    if (!layer->crs().isValid())
    {
        QMessageBox::warning(nullptr,tr("Système de coordonnées"),
                             tr("Le système de coordonnées de la couche est inexistant ou invalide,\n"
                                "le système de coordonnées du projet est assigné à la couche"));
        layer->setCrs(crs);
        return;
    }

    if (!crs.isValid())
        setCRS(layer->crs());
}

void HdManagerSIG::setTextActionCRS()
{
    QString txt;

    if (crs.isValid())
        txt.append(crs.description());
    else
        txt.append(tr("Invalide"));

    if(txt=="")
        txt=tr("Inconnu");

    actionCRSSelectionWithText->setText(txt);
}

void HdManagerSIG::callPropertiesLayer(QgsMapLayer *layer)
{
    if (!layer)
        return;

    QDialog *dial=nullptr;

    QgsVectorLayer *vl;
    QgsRasterLayer *rl;
    QgsMeshLayer *ml;



    switch (layer->type()) {
    case QgsMapLayerType::VectorLayer:
        vl=qobject_cast<QgsVectorLayer*>(layer);
        if (vl)
            dial =new HdVectorLayerPropertiesDialog(vl,map_->getMapCanvas());
        break;
    case QgsMapLayerType::RasterLayer:
        rl=qobject_cast<QgsRasterLayer*>(layer);
        if (rl)
            dial =new QgsRasterLayerProperties(rl,map_->getMapCanvas());
        break;
    case QgsMapLayerType::PluginLayer:
        break;
    case QgsMapLayerType::MeshLayer:
        ml=qobject_cast<QgsMeshLayer*>(layer);
        if (ml)
            dial= new QgsMeshLayerProperties(ml,map_->getMapCanvas());
        break;
    }

    if (dial)
        dial->exec();
}

void HdManagerSIG::layerProperties()
{
    QgsMapLayer *layer=treeLayerView_->currentLayer();
    callPropertiesLayer(layer);

}

void HdManagerSIG::layerPropertiesByIndex(QModelIndex index)
{
    QgsLayerTreeNode* node=treeLayerView_->layerTreeModel()->index2node(index);
    if (!node)
        return;
    if (node->nodeType()==QgsLayerTreeNode::NodeLayer)
    {
        QgsMapLayer * layer=static_cast<QgsLayerTreeLayer*>(node)->layer();
        callPropertiesLayer(layer);
    }
    else
        return;
}

QWidget *HdManagerSIG::getWidget() const
{
    return controlPannel;
}





void HdTreeLayerSIGView::dragEnterEvent(QDragEnterEvent *event)
{
    QgsLayerTreeView::dragEnterEvent(event);
}

QStringList HdTreeLayerModel::mimeTypes() const
{
    QStringList types=QgsLayerTreeModel::mimeTypes();
    types.append(QStringLiteral("application/reos.qgislayer"));
    return types;
}

bool HdTreeLayerModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
    if (data->hasFormat(QStringLiteral("application/reos.qgislayer")))
    {
        QByteArray dataByte=data->data(QStringLiteral("application/reos.qgislayer"));
        QDataStream stream(dataByte);
        int nbLayer;
        stream>>nbLayer;

        for (int i=0;i<nbLayer;++i)
        {
            int type;
            stream>>type;
            QString name;
            stream>>name;
            QString source;
            stream>>source;
            QgsMapLayer *layer=nullptr;

            QgsMapLayerType layerType=static_cast<QgsMapLayerType>(type);
            switch (layerType) {
            case QgsMapLayerType::VectorLayer:
                layer=new QgsVectorLayer(source,name);
                break;
            case QgsMapLayerType::RasterLayer:
                layer=new  QgsRasterLayer(source,name);
                break;
            case QgsMapLayerType::PluginLayer:
                break;
            case QgsMapLayerType::MeshLayer:
                layer=new  QgsMeshLayer(source,name);
                break;

            }

            if (layer)
                if (layer->isValid())
                    QgsProject::instance()->addMapLayer(layer);
        }

        return true;
    }

    return QgsLayerTreeModel::dropMimeData(data,action,row,column,parent);
}

QMenu *HdSigTreeViewContextMenuProvider::createContextMenu()
{
    return manager->getContextMenu();
}
