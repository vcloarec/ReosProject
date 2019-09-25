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


ReosGisManager::ReosGisManager(ReosMap *map, ReosModule *parent): ReosModule(parent),mMap(map),
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




    connect(actionNewProjectSIG,&QAction::triggered,this,&ReosGisManager::newProjectSIG);
    connect(actionOpenProjectSIG,&QAction::triggered,this,&ReosGisManager::openProjectSIG);
    connect(actionLoadVectorielLayer,&QAction::triggered,this,&ReosGisManager::loadVectorielLayer);
    connect(actionLoadRasterLayer,&QAction::triggered,this,&ReosGisManager::loadRasterLayer);
    connect(actionRemoveLayer,&QAction::triggered,this,&ReosGisManager::removeSelectedLayers);
    connect(actionZoomLayerExtent,&QAction::triggered,this,&ReosGisManager::zoomExtentToLayer);
    connect(actionCRSSelection,&QAction::triggered,this,&ReosGisManager::CRSSelection);
    connect(actionCRSSelectionWithText,&QAction::triggered,this,&ReosGisManager::CRSSelection);
    connect(actionLayerProperties,&QAction::triggered,this,&ReosGisManager::layerProperties);
    connect(treeLayerView_,&QAbstractItemView::doubleClicked,this,&ReosGisManager::layerPropertiesByIndex);
    connect(treeLayerView_,&QgsLayerTreeView::currentLayerChanged,this,&ReosGisManager::currentLayerChanged);

    connect(QgsProject::instance(),&QgsProject::crsChanged,mMap,&ReosMap::crsChanged);

    setTextActionCRS();

    QgsApplication::colorSchemeRegistry()->addDefaultSchemes();

}



QgsRasterLayer *ReosGisManager::getRasterLayer()
{
    QgsMapLayerComboBox *comboBox=new QgsMapLayerComboBox;
    comboBox->setFilters(QgsMapLayerProxyModel::RasterLayer);
    ReosDialogBox dial(comboBox);

    dial.exec();

    return static_cast<QgsRasterLayer*>(comboBox->currentLayer());
}

QWidget *ReosGisManager::createCRSDisplay(QWidget *parent)
{
    crsDisplay=new QWidget(parent);
    crsDisplay->setLayout(new QHBoxLayout);
    QToolBar *toolBar=new QToolBar(crsDisplay);
    toolBar->addAction(actionCRSSelectionWithText);
    crsDisplay->layout()->addWidget(new QLabel(tr("Système de coordonnées : ")));
    crsDisplay->layout()->addWidget(toolBar);
    return crsDisplay;
}

QMenu *ReosGisManager::getContextMenu()
{
    if (treeLayerView_->selectedLayers().count()==0)
        return nullptr;

    if (treeLayerView_->selectedLayers().count()==1)
    {
        if (treeLayerView_->currentLayer()->type()==RASTER_LAYER_TYPE)
            return getMenuForOneRasterLayer();


        if (treeLayerView_->currentLayer()->type()==VECTOR_LAYER_TYPE)
            return getMenuForOneVectorLayer();

        return nullptr;

    }

    return getMenuForSeveralLayers();

}

void ReosGisManager::openProjectSIG()
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
    mMap->saveMapExtent();
    loadGISProject();

}

void ReosGisManager::newProjectSIG()
{
    QMessageBox dia(QMessageBox::Question,tr("Nouveau projet SIG"),tr("Enlever les couches actuelles ?")
                    ,QMessageBox::Ok|QMessageBox::Cancel);

    if(dia.exec())
        clear();

}

bool ReosGisManager::addLayer(QgsMapLayer *layer)
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



void ReosGisManager::loadVectorielLayer()
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

void ReosGisManager::loadRasterLayer()
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

void ReosGisManager::removeLayer()
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

void ReosGisManager::removeSelectedLayers()
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

            mMap->getMapCanvas()->refresh();
        }

        QgsMapLayer* currentLayer=nullptr;
        if (treeLayerView_->selectedLayers().count()>0)
            currentLayer=treeLayerView_->selectedLayers().at(0);
        emit currentLayerChanged(currentLayer);

    }
}

void ReosGisManager::zoomExtentToLayer()
{
    QgsMapLayer *layer=treeLayerView_->currentLayer();
    if (layer)
    {
        bridgeTreeMap_->mapCanvas()->setExtent(layer->extent());
        bridgeTreeMap_->mapCanvas()->refresh();
    }
}

void ReosGisManager::CRSSelection()
{
    HdCRSDialogSelection dial;

    if (mCrs.isValid())
        dial.setCrs(mCrs);


    if(dial.exec())
        setCRS(dial.getCrs());

}

void ReosGisManager::setCRS(const QgsCoordinateReferenceSystem &newCrs)
{
    if (!newCrs.isValid())
    {
        return;
    }

    mCrs=newCrs;
    QgsProject::instance()->setCrs(newCrs);
    mMap->getMapCanvas()->setDestinationCrs(newCrs);
    setTextActionCRS();
    emit mapCrsChanged(mCrs);
}

void ReosGisManager::setExtentAfterLoading()
{
    setCRS(mCrs);
    mMap->setToSaveExtent();
    disconnect(bridgeTreeMap_,&QgsLayerTreeMapCanvasBridge::canvasLayersChanged,this,&ReosGisManager::setExtentAfterLoading);
}

QString ReosGisManager::getGISFileName() const
{
    return GISFileName;
}

void ReosGisManager::setGISFileName(const QString &value)
{
    GISFileName = value;
}

QByteArray ReosGisManager::encode()
{
    ReosEncodedElement encodedGISmanager(QStringLiteral("GIS Manager"));
    encodedGISmanager.addData(QStringLiteral("Map extent"),mMap->getMapExtent());
    saveGISProject();

    return encodedGISmanager.encode();
}

void ReosGisManager::decode(QByteArray &ba)
{
    ReosEncodedElement encodedGISmanager(ba);
    if (encodedGISmanager.selfDescription()!=QStringLiteral("GIS Manager"))
        return;

    QRectF extent;
    if(encodedGISmanager.getData(QStringLiteral("Map extent"),extent))
        mMap->setMapSavedExtent(extent);

    loadGISProject();

}

void ReosGisManager::clear()
{
    QgsProject::instance()->clear();
    setCRS(deFaultCrs);
}

QMenu *ReosGisManager::getMenuForOneRasterLayer()
{
    QMenu *menu=new QMenu(controlPannel);
    menu->addAction(actionLayerProperties);
    menu->addAction(actionRemoveLayer);
    menu->addAction(actionZoomLayerExtent);

    return menu;
}

QMenu *ReosGisManager::getMenuForOneVectorLayer()
{
    QMenu *menu=new QMenu(controlPannel);
    menu->addAction(actionLayerProperties);
    menu->addAction(actionRemoveLayer);
    menu->addAction(actionZoomLayerExtent);

    return menu;
}

QMenu *ReosGisManager::getMenuForSeveralLayers()
{
    QMenu *menu=new QMenu(controlPannel);
    menu->addAction(actionRemoveLayer);
    return menu;
}


void ReosGisManager::loadGISProject()
{
    connect(bridgeTreeMap_,&QgsLayerTreeMapCanvasBridge::canvasLayersChanged,this,&ReosGisManager::setExtentAfterLoading);
    QgsProject::instance()->clear();
    QgsProject::instance()->read(GISFileName);

    setCRS(QgsProject::instance()->crs());

}

void ReosGisManager::saveGISProject()
{
    QgsProject::instance()->write(GISFileName);
}

QgsRectangle ReosGisManager::transformExtentFrom(const QgsRectangle &extent, const QgsCoordinateReferenceSystem crsSource)
{
    if (mCrs==crsSource)
        return extent;

    if (mCrs.isValid()&&crsSource.isValid())
    {
        QgsRectangle rectReturn;
        try {
            QgsCoordinateTransform transform(crsSource,mCrs,QgsProject::instance());
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

QgsRectangle ReosGisManager::transformExtentTo(const QgsRectangle &extent, const QgsCoordinateReferenceSystem crsDest)
{
    if (mCrs==crsDest)
        return extent;

    if (mCrs.isValid()&&crsDest.isValid())
    {
        QgsRectangle rectReturn;
        try {
            QgsCoordinateTransform transform(crsDest,mCrs,QgsProject::instance());
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

void ReosGisManager::transformTo(QgsAbstractGeometry *sourceGeometry, const QgsCoordinateReferenceSystem crsDest)
{

    try {
        QgsCoordinateTransform transform(crsDest,mCrs,QgsProject::instance());
        sourceGeometry->transform(transform);

    } catch (QgsCsException &e) {
        Q_UNUSED(e);
        QString message=tr("erreur de transformation :");
        message.append(e.what());
        error(message);
    }

}

void ReosGisManager::transformFrom(QgsAbstractGeometry *sourceGeometry, const QgsCoordinateReferenceSystem crsDest)
{

    try {
        QgsCoordinateTransform transform(crsDest,mCrs,QgsProject::instance());
        sourceGeometry->transform(transform,QgsCoordinateTransform::ReverseTransform);

    } catch (QgsCsException &e) {
        Q_UNUSED(e);
        QString message=tr("erreur de transformation :");
        message.append(e.what());
        error(message);
    }

}

ReosMap *ReosGisManager::getMap() const {return mMap;}

QgsCoordinateReferenceSystem ReosGisManager::getLayerCRS(QString name, QString URI)
{
    QgsMapLayer *layer=getLayer(name,URI);
    if (layer)
        return layer->crs();
    else
        return QgsCoordinateReferenceSystem();
}

QgsMapLayer *ReosGisManager::getLayer(QString name, QString URI)
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

QList<QgsRasterLayer *> ReosGisManager::getAllRasterLayers()
{
    QList<QgsMapLayer *> list=treemodel_->rootGroup()->checkedLayers();

    QList<QgsRasterLayer*> listRasterLayers;

    for (auto layer:list)
    {
        if (layer->type()==RASTER_LAYER_TYPE)
            listRasterLayers.append(static_cast<QgsRasterLayer*>(layer));
    }

    return listRasterLayers;
}

void ReosGisManager::controlLayerCRS(QgsMapLayer *layer)
{
    if (!layer->crs().isValid())
    {
        QMessageBox::warning(nullptr,tr("Système de coordonnées"),
                             tr("Le système de coordonnées de la couche est inexistant ou invalide,\n"
                                "le système de coordonnées du projet est assigné à la couche"));
        layer->setCrs(mCrs);
        return;
    }

    if (!mCrs.isValid())
        setCRS(layer->crs());
}

void ReosGisManager::setTextActionCRS()
{
    QString txt;

    if (mCrs.isValid())
        txt.append(mCrs.description());
    else
        txt.append(tr("Invalide"));

    if(txt=="")
        txt=tr("Inconnu");

    actionCRSSelectionWithText->setText(txt);
}

void ReosGisManager::callPropertiesLayer(QgsMapLayer *layer)
{
    if (!layer)
        return;

    QDialog *dial=nullptr;

    QgsVectorLayer *vl;
    QgsRasterLayer *rl;
    QgsMeshLayer *ml;



    switch (layer->type()) {
    case VECTOR_LAYER_TYPE:
        vl=qobject_cast<QgsVectorLayer*>(layer);
        if (vl)
            dial =new HdVectorLayerPropertiesDialog(vl,mMap->getMapCanvas());
        break;
    case RASTER_LAYER_TYPE:
        rl=qobject_cast<QgsRasterLayer*>(layer);
        if (rl)
            dial =new QgsRasterLayerProperties(rl,mMap->getMapCanvas());
        break;
    case MESH_LAYER_TYPE:
        ml=qobject_cast<QgsMeshLayer*>(layer);
        if (ml)
            dial= new QgsMeshLayerProperties(ml,mMap->getMapCanvas());
        break;
    default:
        break;
    }

    if (dial)
        dial->exec();

    emit layerHasToBeUpdated(layer);
}

void ReosGisManager::layerProperties()
{
    QgsMapLayer *layer=treeLayerView_->currentLayer();
    callPropertiesLayer(layer);

}

void ReosGisManager::layerPropertiesByIndex(QModelIndex index)
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

QWidget *ReosGisManager::getWidget() const
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

            LAYER_TYPE layerType=static_cast<LAYER_TYPE>(type);
            switch (layerType) {
            case VECTOR_LAYER_TYPE:
                layer=new QgsVectorLayer(source,name);
                break;
            case RASTER_LAYER_TYPE:
                layer=new  QgsRasterLayer(source,name);
                break;
            case MESH_LAYER_TYPE:
                layer=new  QgsMeshLayer(source,name);
                break;
            default:
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

QVariant HdTreeLayerModel::data(const QModelIndex &index, int role) const
{
    QgsLayerTreeNode *node = index2node( index );

    if ( role == Qt::DecorationRole && index.column() == 0 )
    {
        if (QgsLayerTree::isLayer(node))
        {
            QgsMapLayer *layer=QgsLayerTree::toLayer(node)->layer();
            if (layer->dataProvider()->name()=="TIN")
                return QPixmap("://toolbar/MeshTinIcon.png");
        }
    }

    return QgsLayerTreeModel::data(index,role);

}

QMenu *HdSigTreeViewContextMenuProvider::createContextMenu()
{
    return manager->getContextMenu();
}
