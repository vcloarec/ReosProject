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

#include "reostineditorgraphic.h"

ReosTinEditorUi::ReosTinEditorUi(HdManagerSIG *gismanager, QObject *parent):ReosModule(parent),
    mDomain(new ReosMapMeshEditorItemDomain(this,gismanager->getMap()->getMapCanvas())),
    mGisManager(gismanager),
    mMap(gismanager->getMap()),
    actionNewTinLayer(new QAction(QPixmap("://toolbar/MeshNewTIN.png"),tr("New TIN"),this)),
    actionNewVertex(new QAction(QPixmap("://toolbar/MeshTINNewVertex.png"),tr("Draw vertex"),this)),
    mapToolNewVertex(new ReosMapToolClickPoint(gismanager->getMap())),
    actionRemoveVertex(new QAction(QPixmap("://toolbar/MeshTINRemoveVertex.png"),tr("Remove vertex"),this)),
    mapToolRemoveVertex(new ReosMapToolSelection(gismanager->getMap())),
    actionNewHardLineSegment(new QAction(QPixmap("://toolbar/MeshTINNewSegment.png"),tr("Draw hard line"),this)),
    mapToolHardLineSegment(new ReosTinMapToolHardLineSegement(gismanager->getMap(),this)),
    actionRemoveSegment(new QAction(QPixmap("://toolbar/MeshTINRemoveSegment.png"),tr("Remove hard line"),this)),
    mapToolRemoveSegment(new ReosMapToolSelection(gismanager->getMap())),
    actionTriangulateTIN(new QAction(QPixmap("://toolbar/MeshTINTriangulation.png"),tr("update mesh"),this))
{
    actionNewVertex->setCheckable(true);
    mapToolNewVertex->setAction(actionNewVertex);
    mapToolHardLineSegment->setAction(actionNewHardLineSegment);
    actionNewHardLineSegment->setCheckable(true);
    mapToolRemoveVertex->setCursor(QCursor(QPixmap("://supprimeElement.png"),0,0));
    mapToolRemoveVertex->setAction(actionRemoveVertex);
    actionRemoveVertex->setCheckable(true);
    mapToolRemoveSegment->setCursor(QCursor(QPixmap("://supprimeElement.png"),0,0));
    mapToolRemoveSegment->setAction(actionRemoveSegment);
    actionRemoveSegment->setCheckable(true);


    groupAction->addAction(actionNewTinLayer);
    groupAction->addAction(actionNewVertex);
    groupAction->addAction(actionRemoveVertex);
    groupAction->addAction(actionNewHardLineSegment);
    groupAction->addAction(actionRemoveSegment);
    groupAction->addAction(actionTriangulateTIN);
    actionEditList.append(actionNewVertex);
    actionEditList.append(actionNewHardLineSegment);
    actionEditList.append(actionRemoveVertex);
    actionEditList.append(actionTriangulateTIN);
    actionEditList.append(actionRemoveSegment);
    enableEditAction(false);

    uiDialog=new HdTinEditorUiDialog(mMap->getMapCanvas());
    uiDialog->setActions(getActions());

    connect(actionNewTinLayer,&QAction::triggered,this,&ReosTinEditorUi::newTinLayer);

    connect(actionNewVertex,&QAction::triggered,this,&ReosTinEditorUi::startNewVertex);
    connect(mapToolNewVertex,&ReosMapToolClickPoint::clickDone,this,&ReosTinEditorUi::newVertex);
    connect(mapToolNewVertex,&ReosMapTool::stop,this,&ReosTinEditorUi::stopNewVertex);

    connect(actionRemoveVertex,&QAction::triggered,this,&ReosTinEditorUi::startRemoveVertex);
    connect(mapToolRemoveVertex,&ReosMapToolSelection::zonalCanvasRect,this,&ReosTinEditorUi::removeVertexFromRect);

    connect(actionNewHardLineSegment,&QAction::triggered,this,&ReosTinEditorUi::startNewHardLineSegment);
    connect(mapToolHardLineSegment,&ReosTinMapToolHardLineSegement::stop,this,&ReosTinEditorUi::stopNewHardLineSegment);
    connect(actionTriangulateTIN,&QAction::triggered,this,&ReosTinEditorUi::updateMesh);

    connect(actionRemoveSegment,&QAction::triggered,this,&ReosTinEditorUi::startRemoveSegment);
    connect(mapToolRemoveSegment,&ReosMapToolSelection::zonalCanvasRect,this,&ReosTinEditorUi::removeSegmentFromRect);

    connect(gismanager,&HdManagerSIG::currentLayerChanged,this,&ReosTinEditorUi::currentLayerChanged);
    connect(gismanager,&HdManagerSIG::layerHasToBeUpdated,this,&ReosTinEditorUi::layerHasToBeUpdated);
    connect(gismanager,&HdManagerSIG::layerHasToBeRemoved,this,&ReosTinEditorUi::layerHasToBeRemoved);

    connect(uiDialog,&HdTinEditorUiDialog::closed,this,&ReosTinEditorUi::widgetClosed);
    connect(uiDialog,&HdTinEditorUiDialog::escapePressed,mMap,&ReosMap::stopMapTool);
}

void ReosTinEditorUi::setMeshLayer(QgsMeshLayer *meshLayer)
{
    if (meshLayer==mMeshLayer)
        return;

    mMeshLayer=meshLayer;

    updateMeshLayer();

    enableEditAction(mEditor != nullptr);
    populateDomain();

}

ReosMeshItemVertex *ReosTinEditorUi::mapVertex(const QPointF &mapPoint) const {

    VertexPointer rwv=realWorldVertex(mapPoint);
    if (rwv)
        return static_cast<ReosMeshItemVertex*>(rwv->graphicPointer());
    else
        return nullptr;
}

VertexPointer ReosTinEditorUi::realWorldVertex(const QPointF &mapPoint) const
{
    if (!mEditor)
        return nullptr;

    QPointF meshPoint=meshCoordinates(mapPoint);
    return mEditor->vertex(meshPoint.x(),meshPoint.y());
}

VertexPointer ReosTinEditorUi::addRealWorldVertex(const QPointF &mapPoint, double z)
{
    VertexPointer vert=nullptr;

    if (mEditor)
    {
        QPointF p=meshCoordinates(mapPoint);
        vert=mEditor->addVertex(p.x(),p.y());
        vert->setZValue(z);
        vert->setZUserDefined();
        addMapVertex(mapPoint,vert);

    }

    return vert;
}

VertexPointer ReosTinEditorUi::addRealWorldVertex(const QPointF &mapPoint)
{
    return addRealWorldVertex(mapPoint,zValue(mapPoint));
}


void ReosTinEditorUi::removeVertex(VertexPointer vertex)
{
    if (!vertex)
        return;
    auto neighbours=mEditor->hardNeighbours(vertex);
    mEditor->removeVertex(vertex);
    domain()->removeVertex(static_cast<ReosMeshItemVertex*>(vertex->graphicPointer()));

    for (auto n:neighbours)
    {
        updateGraphics(n);
    }
}




void ReosTinEditorUi::doCommand(ReosTinUndoCommandNewVertex *command)
{
    VertexPointer realWordlVertex=addRealWorldVertex(command->mMapPoint,command->mZValue);
    if (realWordlVertex)
    {
        realWordlVertex->setZUserDefined();
    }
}

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandNewVertex *command)
{
    VertexPointer rwv=realWorldVertex(command->mMapPoint);
    removeVertex(rwv);
}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandNewSegmentWithNewSecondVertex *command)
{
    VertexPointer firstVertex=realWorldVertex(command->mMapPointFirst);
    if (!firstVertex)
        return;

    //add the second realWorld vertex
    VertexPointer secondVertex=addRealWorldVertex(command->mMapPointSecond);
    addSegment(firstVertex,secondVertex,command->mVerticesPositionAndStructureMemory);


}

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandNewSegmentWithNewSecondVertex *command)
{

    VertexPointer finalVertex=realWorldVertex(command->mMapPointSecond);

    //remove the segments
    QList<PointAndNeighbours> &verticesPositions=command->mVerticesPositionAndStructureMemory;
    for (int i=0;i<verticesPositions.count()-1;++i)
    {
        VertexPointer v1=realWorldVertex(verticesPositions.at(i).point);
        VertexPointer v2=realWorldVertex(verticesPositions.at(i+1).point);

    }

//    if(command->intersectionVertex.count()>0)
//    {
//        //remove segments added
//        for (int i=0;i<command->intersectionVertex.count();++i)
//        {
//            PointAndNeighbours point=command->intersectionVertex.at(i);
//            VertexPointer nextVertex=realWorldVertex(point.point);

//            removeSegment(currentVertex,nextVertex);

//            currentVertex=nextVertex;
//        }

//        //remove intersection vertices
//        for (int i=0;i<command->intersectionVertex.count();++i)
//        {
//            PointAndNeighbours point=command->intersectionVertex.at(i);
//            removeVertex(realWorldVertex(point.point));
//        }
//    }
//    else
//    {
//        removeSegment(currentVertex,finalVertex);
//    }

    //remove the secondVertex
    removeVertex(realWorldVertex(command->mMapPointSecond));


}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandNewSegmentWithExistingSecondVertex *command)
{
    VertexPointer firstVertex=realWorldVertex(command->mMapPointFirst);
    VertexPointer secondVertex=realWorldVertex(command->mMapPointSecond);

    if (!firstVertex || !secondVertex)
        return;

    addSegment(firstVertex,secondVertex,command->mVerticesPositionAndStructureMemory);

}

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandNewSegmentWithExistingSecondVertex *command)
{
    //remove the segments
    QList<PointAndNeighbours> &verticesPositions=command->mVerticesPositionAndStructureMemory;
    for (int i=0;i<verticesPositions.count()-1;++i)
    {
        VertexPointer v1=realWorldVertex(verticesPositions.at(i).point);
        VertexPointer v2=realWorldVertex(verticesPositions.at(i+1).point);
    }
}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandRemoveVertex *command)
{
    VertexPointer vertex=realWorldVertex(command->mMapPoint);

    if (vertex)
    {
        auto neighboursVertices=mEditor->hardNeighbours(vertex);
        auto graphicVertex=static_cast<ReosMeshItemVertex*>(vertex->graphicPointer());
        mEditor->removeVertex(vertex);
        mDomain->removeVertex(graphicVertex);
        for (auto n:neighboursVertices)
            updateGraphics(n);
    }

}

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandRemoveVertex *command)
{

}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandRemoveHardLine *command)
{
    VertexPointer vertex1=realWorldVertex(command->mMapPointForVertex1);
    VertexPointer vertex2=realWorldVertex(command->mMapPointForVertex2);

    auto verticesList=mEditor->removeHardLine(vertex1,vertex2);

    for (auto v:verticesList)
        updateGraphics(v);
};


void ReosTinEditorUi::updateMeshLayer()
{
    mTransform.reset(nullptr);
    if(mMeshLayer==nullptr)
        mEditor=nullptr;
    else
    {
        if(mMeshLayer->dataProvider()->name()==QStringLiteral("TIN"))
        {
            mEditor=static_cast<TINProvider*>(mMeshLayer->dataProvider())->editor();
            mTransform.reset(new QgsCoordinateTransform(mMeshLayer->crs(),mMap->getCoordinateReferenceSystem(),QgsProject::instance()));
            mUndoStack=mUndoStacks.value(mEditor,nullptr);

            if (mUndoStack==nullptr)
            {
                mUndoStack=new QUndoStack(this);
                mUndoStacks[mEditor]=mUndoStack;
            }

            emit activeUndoStack(mUndoStack);
        }
        else {
            mEditor=nullptr;
        }
    }
}



void ReosTinEditorUi::newVertex(const QPointF &mapPoint)
{
    uiDialog->setLineEditFocus();


    if (!mEditor || realWorldVertex(mapPoint))
        return;

    double z=zValue(mapPoint);

    auto command=new ReosTinUndoCommandNewVertex(this,mapPoint,z);
    newCommand(command);
}

void ReosTinEditorUi::stopNewVertex()
{
    setNoneMode();
}

void ReosTinEditorUi::startRemoveVertex()
{
    mMap->setMapTool(mapToolRemoveVertex);
}

void ReosTinEditorUi::removeVertexFromRect(const QRectF &selectionZone)
{
    ReosMeshItemVertex *vert=mDomain->vertex(selectionZone);
    if (!vert)
        return;
    VertexPointer realWorldVertex=vert->realWorldVertex();
    if (realWorldVertex)
    {
        if(mEditor->isVertexOnHarLine(realWorldVertex))
        {
            QMessageBox::warning(uiDialog,tr("Suppression d'un sommet surune ligne d'arrête"),tr("Il est nécessaire de supprimer d'abord la ou les lignes d'arrêtes"));
        }
        else
        {
            QPointF mapPoint=mapCoordinates(QPointF(realWorldVertex->x(),realWorldVertex->y()));
            ReosTinUndoCommandRemoveVertex* command=new ReosTinUndoCommandRemoveVertex(this,mapPoint);
            newCommand(command);
        }

    }
}

void ReosTinEditorUi::removeSegmentFromRect(const QRectF &selectionZone)
{
    ReosMeshItemSegment *seg=mDomain->segment(selectionZone);
    if (seg)
    {
        VertexPointer v1=seg->vertex1()->realWorldVertex();
        VertexPointer v2=seg->vertex2()->realWorldVertex();

        QPointF mapPoint1=mapCoordinates(QPointF(v1->x(),v1->y()));
        QPointF mapPoint2=mapCoordinates(QPointF(v2->x(),v2->y()));

        auto command= new ReosTinUndoCommandRemoveHardLine(this,mapPoint1,mapPoint2);
        newCommand(command);
    }
}

void ReosTinEditorUi::startNewHardLineSegment()
{
    mMap->setMapTool(mapToolHardLineSegment);
    setLevelMode();
    uiDialog->setLineEditFocus();
}

void ReosTinEditorUi::newSegment(ReosMeshItemVertex *firstVertex, const QPointF &secondMapPoint)
{
    if (!mEditor)
        return;

    QPointF mapPointFirst=mapCoordinates(QPointF(firstVertex->realWorldVertex()->x(),firstVertex->realWorldVertex()->y()));
    auto command=new ReosTinUndoCommandNewSegmentWithNewSecondVertex(this,mapPointFirst,secondMapPoint,zValue(secondMapPoint));
    newCommand(command);
}

void ReosTinEditorUi::newSegment(ReosMeshItemVertex *firstVertex, ReosMeshItemVertex *secondVertex)
{
    if (!mEditor)
        return;

    if (firstVertex==secondVertex)
        return;

    auto command=new ReosTinUndoCommandNewSegmentWithExistingSecondVertex(this,firstVertex->position(),secondVertex->position());
    newCommand(command);
}

void ReosTinEditorUi::stopNewHardLineSegment()
{
    setNoneMode();
}

void ReosTinEditorUi::startRemoveSegment()
{
    mMap->setMapTool(mapToolRemoveSegment);
}

//QList<PointAndNeighbours> ReosTinEditorUi::addSegment(ReosMeshItemVertex *v1, ReosMeshItemVertex *v2)
//{
//    std::list<VertexPointer> intersectVertex=mEditor->addSegment(v1->realWorldVertex(),v2->realWorldVertex());


//    ReosMeshItemVertex *vertexToJoinWithLastVertex=nullptr;
//    QList<ReosMeshItemVertex*> newGraphicVertices;
//    for (auto v:intersectVertex)
//    {
//        ReosMeshItemVertex *graphicVertex=nullptr;
//        if (v->graphicPointer()==nullptr)
//        {
//            graphicVertex=addVertexToDomain(v);
//            newGraphicVertices.append(graphicVertex);
//            graphicVertex->setStatus(ReosMeshItemVertex::newIntersection);
//            v->setZValue(zValue(graphicVertex->position()));
//        }
//        else {
//            graphicVertex=static_cast<ReosMeshItemVertex*>(v->graphicPointer());
//        }

//        if (graphicVertex && vertexToJoinWithLastVertex)
//        {
//            if (!graphicVertex->segment(vertexToJoinWithLastVertex))
//                domain()->addSegmentHardLine(vertexToJoinWithLastVertex,graphicVertex);
//        }

//        vertexToJoinWithLastVertex=graphicVertex;
//    }

//    QList<PointAndNeighbours> neighbours;

//    for (auto v:newGraphicVertices)
//    {
//        //ReosMeshItemVertexAndNeighbours n=updateNeighbourVertices(v);
//        //neighbours.append(PointAndNeighbours(n));
//    }


//    return neighbours;
//}

ReosMeshItemVertex *ReosTinEditorUi::addMapVertex_2(VertexPointer realWorldVertex)
{
    QPointF realWordlPoint(realWorldVertex->x(),realWorldVertex->y());
    QPointF mapPoint=mapCoordinates(realWordlPoint);

    return addMapVertex(realWorldVertex);
//    ReosMeshItemVertex *mapVertex=mDomain->addVertex(mapPoint);
//    mapVertex->setRealWorldVertex(realWorldVertex);
//    realWorldVertex->setGraphicPointer(mapVertex);
//    return mapVertex;
}



void ReosTinEditorUi::addSegment(VertexPointer v1, VertexPointer v2, QList<PointAndNeighbours> &oldNeigboursStructure)
{
    //add real world segment
    std::list<VertexPointer> hardLineVertices=mEditor->addSegment(v1,v2);

    //set Z value for next
    for (auto vert:hardLineVertices)
    {
        if(vert->graphicPointer()==nullptr)
        {
            QPointF mapPoint=mapCoordinates(QPointF(vert->x(),vert->y()));
            vert->setZValue(zValue(mapPoint));
        }
    }


    //store the old structure arround the new vertices from the graphics item
    oldNeigboursStructure.clear();
    for (auto vert:hardLineVertices)
    {
        PointAndNeighbours currentStructure;
        currentStructure.point=QPointF(vert->x(),vert->y());
        auto realWorldNeighbours=mEditor->hardNeighbours(vert);
        for (auto rwn:realWorldNeighbours)
        {
            ReosMeshItemVertex *graphicVertex=static_cast<ReosMeshItemVertex*>(rwn->graphicPointer());
            if (graphicVertex)
            {
                PointAndNeighbours currentSubStructure(saveStructure(graphicVertex));
                currentStructure.neighbours.append(currentSubStructure);
            }
        }
        oldNeigboursStructure.append(currentStructure);
    }

    //update graphics vertices and their neighbourgh : synchronize the real world with the graphic world
    for (auto vert:hardLineVertices)
    {
        updateGraphics(vert);
    }
    for (auto vert:hardLineVertices)
    {
        auto neighbours=mEditor->hardNeighbours(vert);
        for (auto nv:neighbours)
            updateGraphics(nv);
    }
}

void ReosTinEditorUi::populateDomain()
{
    mDomain->clear();
    if (mEditor)
    {
        auto tinReader=mEditor->getTinReader();
        while (!tinReader->allVerticesReaden())
        {
            VertexPointer vert=tinReader->readVertexPointer();
            addMapVertex(vert);
        }

        while(!tinReader->allSegmentsReaden())
        {
            int s[2];
            tinReader->readSegment(s);
            mDomain->addSegmentHardLine(mDomain->vertex(s[0]),mDomain->vertex(s[1]));
        }
    }

}

void ReosTinEditorUi::currentLayerChanged(QgsMapLayer *layer)
{
    if (!layer)
    {
        setMeshLayer(nullptr);
        return;
    }

    if (layer->type()==QgsMapLayerType::MeshLayer)
        setMeshLayer(static_cast<QgsMeshLayer*>(layer));
    else
        setMeshLayer(nullptr);
}

void ReosTinEditorUi::layerHasToBeUpdated(QgsMapLayer *layer)
{
    if (layer && layer==mMeshLayer)
    {
        updateMeshLayer();
    }
}

void ReosTinEditorUi::layerHasToBeRemoved(QgsMapLayer *layer)
{
    if (!(layer->type()==QgsMapLayerType::MeshLayer))
        return;

    QgsMeshLayer *meshLayer=static_cast<QgsMeshLayer*>(layer);
    if(meshLayer->dataProvider()->name()==QStringLiteral("TIN"))
    {
        TINEditor* layerEditor=static_cast<TINProvider*>(mMeshLayer->dataProvider())->editor();

        if (layerEditor->isDirty())
        {
            if (QMessageBox::warning(uiDialog,tr("Sauvegarde du maillage"),
                                     tr("Le maillage %1 a été modifé. Voulez-vous le sauvegarder ?").arg(meshLayer->name())
                                     ,QMessageBox::Yes|QMessageBox::No,QMessageBox::Yes)==QMessageBox::Yes)
            {
                ///TODO : need to be completed
            }
        }


        QUndoStack *us=mUndoStacks.value(layerEditor,nullptr);
        if (us)
            us->deleteLater();
        mUndoStacks.remove(layerEditor);

    }
}

double ReosTinEditorUi::zValue(const QPointF &p)
{
    Q_UNUSED(p);

    if (zValueMode==HdTinEditorUiDialog::level)
    {
        return uiDialog->lineEditText().toDouble();
    }

    return 0;
}

void ReosTinEditorUi::startNewVertex()
{
    mMap->setMapTool(mapToolNewVertex);
    setLevelMode();
    uiDialog->setLineEditFocus();

}


void ReosTinEditorUi::updateMesh()
{
    if (mMeshLayer)
        mMeshLayer->reload();

    mMap->refreshMap();
}

void ReosTinEditorUi::enableEditAction(bool enable)
{
    for (auto a:actionEditList)
        a->setEnabled(enable);
}

void ReosTinEditorUi::newTinLayer()
{
    auto dial=new HdTinEditorNewDialog(mMap->getMapCanvas());
    if (dial->exec())
    {
        auto layer=new QgsMeshLayer(dial->fileName(),dial->name(),"TIN");
        layer->setCrs(dial->crs());
        mGisManager->addLayer(new QgsMeshLayer(dial->fileName(),dial->name(),"TIN"));
    }

}

void ReosTinEditorUi::widgetClosed()
{
    emit widgetVisibility(false);
}

ReosMeshItemVertex *ReosTinEditorUi::addMapVertex(VertexPointer vert)
{
    QPointF mapPoint;
    if (mTransform->isValid())
        mapPoint=mTransform->transform(vert->x(),vert->y(),QgsCoordinateTransform::ForwardTransform).toQPointF();
    else {
        mapPoint=QPointF(vert->x(),vert->y());
    }
    return addMapVertex(mapPoint,vert);
}

ReosMeshItemVertex *ReosTinEditorUi::addMapVertex(const QPointF &mapPoint, VertexPointer vert)
{
    ReosMeshItemVertex *vertexGraphic=mDomain->addVertex(mapPoint);
    vertexGraphic->setRealWorldVertex(vert);
    if (vert->isZUserDefined())
        vertexGraphic->setStatus(ReosMeshItemVertex::none);
    else
        (vertexGraphic->setStatus(ReosMeshItemVertex::ZToDefined));
    vert->setGraphicPointer(vertexGraphic);
    return vertexGraphic;
}

void ReosTinEditorUi::setLevelMode()
{
    zValueMode=HdTinEditorUiDialog::level;
    uiDialog->setZValueMode(zValueMode);
}

void ReosTinEditorUi::setNoneMode()
{
    zValueMode=HdTinEditorUiDialog::none;
    uiDialog->setZValueMode(zValueMode);
}

QPointF ReosTinEditorUi::mapCoordinates(const QPointF &meshCoordinate) const
{
    if (mTransform)
        return mTransform->transform(meshCoordinate.x(),meshCoordinate.y(),QgsCoordinateTransform::ForwardTransform).toQPointF();
    else
        return meshCoordinate;
}

QPointF ReosTinEditorUi::meshCoordinates(const QPointF &mapCoordinate) const
{
    if (mTransform)
        return mTransform->transform(mapCoordinate.x(),mapCoordinate.y(),QgsCoordinateTransform::ReverseTransform).toQPointF();
    else
        return mapCoordinate;
}

ReosMeshItemVertexAndNeighbours ReosTinEditorUi::saveStructure(ReosMeshItemVertex *vert) const
{
    auto neighboursRealWorldVertex=mEditor->hardNeighbours(vert->realWorldVertex());

    ReosMeshItemVertexAndNeighbours returnMemory;
    returnMemory.vertex=vert;

    for (int i=0;i<vert->segmentsCount();++i)
    {
        ReosMeshItemVertex *neighbour=vert->segment(i)->otherVertex(vert);
        ReosMeshItemVertexAndNeighbours localMemory;
        localMemory.vertex=neighbour;
        returnMemory.neighbours.append(localMemory);
    }

    return returnMemory;
}

void ReosTinEditorUi::restoreStructure(VertexPointer realWorldVertex, const PointAndNeighbours &structure) const
{

}

void ReosTinEditorUi::updateGraphics(VertexPointer realWorldVertex)
{
    if (!realWorldVertex)
        return;

    if (realWorldVertex->graphicPointer()==nullptr)
        addMapVertex(realWorldVertex);

    auto neighboursVertex=mEditor->hardNeighbours(realWorldVertex);
    auto currentGraphicVertex=static_cast<ReosMeshItemVertex*>(realWorldVertex->graphicPointer());
    if (!currentGraphicVertex)
        return;

    QVector<bool> graphicsNeighboursAssociated(currentGraphicVertex->segmentsCount(),false); //table containing if the graphic vertex is associated with a real world vertex

    for (auto nb:neighboursVertex)
    {
        //look for other extremtiy graphic vertex that corresponds with graphic vertex associated with the neighbour real world vertex
        //if yes, store it in associatedGrapicVertex and flag true graphicsNeighboursAssociated table to not remove it after
        //if not the graphicsNeighboursAssociated table is still flag false to remove it after
        ReosMeshItemVertex* associatedGraphicVertex=nullptr;
        for (int i=0;i<currentGraphicVertex->segmentsCount();++i)
        {
            auto neighbourGraphicVertex=currentGraphicVertex->segment(i)->otherVertex(currentGraphicVertex);
            if (neighbourGraphicVertex->realWorldVertex()==nb)
            {
                graphicsNeighboursAssociated[i]=true;
                associatedGraphicVertex=neighbourGraphicVertex;
            }
        }
        //if the neighbourg real world vertex doesn't have a graphic verter with the currnt graphic vertex, create the link
        if (!associatedGraphicVertex)
        {
            ReosMeshItemVertex *vertexToAssociate=static_cast<ReosMeshItemVertex*>(nb->graphicPointer());
            if (!vertexToAssociate)
                vertexToAssociate=addMapVertex(nb);

            mDomain->addSegmentHardLine(currentGraphicVertex,vertexToAssociate);
            graphicsNeighboursAssociated.append(true);
        }

    }

    int i=0;
    while (i<currentGraphicVertex->segmentsCount())
    {
        if (!graphicsNeighboursAssociated[i])
        {
            mDomain->removeSegment(currentGraphicVertex->segment(i));
            graphicsNeighboursAssociated.removeAt(i);
        }
        else
            ++i;
    }

};

void ReosTinMapToolHardLineSegement::canvasPressEvent(QgsMapMouseEvent *e)
{
    if (firstVertex==nullptr && firstPoint==QPointF())
    {
        rubberBand->reset();
        firstPoint=e->mapPoint().toQPointF();
        firstVertex=mUiEditor->mapVertex(firstPoint);

        if(!firstVertex)
        {
            mUiEditor->newVertex(firstPoint);
            firstVertex=mUiEditor->mapVertex(firstPoint);
        }

        rubberBand->addPoint(e->mapPoint());
    }
    else{
        QPointF secondPoint=e->mapPoint().toQPointF();
        auto secondVertex=mUiEditor->mapVertex(secondPoint);
        if (secondVertex)
        {
            mUiEditor->newSegment(firstVertex,secondVertex);
        }else
        {
            mUiEditor->newSegment(firstVertex,secondPoint);
        }

        rubberBand->reset();
        rubberBand->addPoint(e->mapPoint());
        firstPoint=secondPoint;
        firstVertex=mUiEditor->mapVertex(firstPoint);
    }
}
