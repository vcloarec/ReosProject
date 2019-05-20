/***************************************************************************
                      hdtineditorgraphic.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "hdtineditorgraphic.h"


HdTinMapToolNewVertex::HdTinMapToolNewVertex(HdMap *map):HdMapTool(map){}

void HdTinMapToolNewVertex::canvasPressEvent(QgsMapMouseEvent *e)
{
    emit newVertex(e->mapPoint().toQPointF());
}



HdTinEditorUi::HdTinEditorUi(HdManagerSIG *gismanager, QObject *parent):ReosModule(parent),
    mDomain(new HdMapMeshEditorItemDomain(gismanager->getMap()->getMapCanvas())),
    mGisManager(gismanager),
    mMap(gismanager->getMap()),
    actionNewTinLayer(new QAction(QPixmap("://toolbar/MeshNewTIN.png"),tr("Nouveau TIN"),this)),
    actionNewVertex(new QAction(QPixmap("://toolbar/MeshTINNewVertex.png"),tr("Nouveau point"),this)),
    mapToolNewVertex(new HdTinMapToolNewVertex(gismanager->getMap())),
    actionTriangulateTIN(new QAction(QPixmap("://toolbar/MeshTINTriangulation.png"),tr("Triangulation"),this))
{
    actionNewVertex->setCheckable(true);
    mapToolNewVertex->setAction(actionNewVertex);

    groupAction->addAction(actionNewTinLayer);
    groupAction->addAction(actionNewVertex);
    groupAction->addAction(actionTriangulateTIN);
    actionEditList.append(actionNewVertex);
    actionEditList.append(actionTriangulateTIN);
    enableEditAction(false);

    uiDialog=new HdTinEditorUiDialog(mMap->getMapCanvas());
    uiDialog->setActions(getActions());

    connect(actionNewTinLayer,&QAction::triggered,this,&HdTinEditorUi::newTinLayer);

    connect(actionNewVertex,&QAction::triggered,this,&HdTinEditorUi::startNewVertex);
    connect(mapToolNewVertex,&HdTinMapToolNewVertex::newVertex,this,&HdTinEditorUi::newVertex);
    connect(mapToolNewVertex,&HdTinMapToolNewVertex::arret,this,&HdTinEditorUi::stopNewVertex);
    connect(actionTriangulateTIN,&QAction::triggered,this,&HdTinEditorUi::triangulateTIN);
    connect(gismanager,&HdManagerSIG::currentLayerChanged,this,&HdTinEditorUi::currentLayerChanged);
    connect(uiDialog,&HdTinEditorUiDialog::closed,this,&HdTinEditorUi::widgetClosed);
}

void HdTinEditorUi::setMeshLayer(QgsMeshLayer *meshLayer)
{
    if (meshLayer==mMeshLayer)
        return;

    mMeshLayer=meshLayer;

    if(mMeshLayer==nullptr)
        mEditor=nullptr;
    else
    {
        if(mMeshLayer->dataProvider()->name()==QStringLiteral("TIN"))
        {
            mEditor=static_cast<TINProvider*>(mMeshLayer->dataProvider())->editor();
        }
        else {
            mEditor=nullptr;
        }
    }
    enableEditAction(mEditor != nullptr);

    populateDomain();

}

VertexPointer HdTinEditorUi::newVertex(const QPointF &p)
{
    uiDialog->setLineEditFocus();
    if (mEditor)
    {
        VertexPointer vert=mEditor->addVertex(p.x(),p.y());

        if (mEditor->verticesCount()>mDomain->verticesCount())
        {
            addVertexToDomain(p);
            vert->setZValue(zValue(p));
        }

        return vert;
    }

    return VertexPointer();
}

void HdTinEditorUi::populateDomain()
{
    mDomain->clear();
    if (mEditor)
    {
        auto tinReader=mEditor->getTinReader();
        while (!tinReader->allVerticesReaden())
        {
            double vert[3];
            tinReader->readOnlyVertex(vert);
            mDomain->addVertex(QPointF(vert[0],vert[1]));
        }
    }

}

void HdTinEditorUi::currentLayerChanged(QgsMapLayer *layer)
{
    if (!layer)
        return;

    if (layer->type()==QgsMapLayerType::MeshLayer)
        setMeshLayer(static_cast<QgsMeshLayer*>(layer));
    else
        setMeshLayer(nullptr);
}

void HdTinEditorUi::startNewVertex()
{
    mMap->setMapTool(mapToolNewVertex);
    setLevelMode();
    uiDialog->setLineEditFocus();

}


void HdTinEditorUi::triangulateTIN()
{
    if (mMeshLayer)
        mMeshLayer->reload();

    mMap->refreshMap();
}

void HdTinEditorUi::enableEditAction(bool enable)
{
    for (auto a:actionEditList)
        a->setEnabled(enable);
}

void HdTinEditorUi::newTinLayer()
{
    auto dial=new HdTinEditorNewDialog(mMap->getMapCanvas());
    if (dial->exec())
    {
        auto layer=new QgsMeshLayer(dial->fileName(),dial->name(),"TIN");
        layer->setCrs(dial->crs());
        mGisManager->addLayer(new QgsMeshLayer(dial->fileName(),dial->name(),"TIN"));
    }

}
