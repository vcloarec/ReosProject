/***************************************************************************
                      hdtineditorgraphic.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
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

ReosTinEditorUi::ReosTinEditorUi(ReosGisManager *gismanager, QObject *parent):ReosModule(parent),
    mDomain(new ReosMapMeshEditorItemDomain(this,gismanager->getMap()->getMapCanvas())),
    mGisManager(gismanager),
    mMap(gismanager->getMap()),
    actionNewTinLayer(new QAction(QPixmap("://toolbar/MeshNewTIN.png"),tr("New TIN"),this)),
    actionOpenTinLayer(new QAction(tr("Open"),this)),
    actionSaveTinLayer(new QAction(tr("Save"),this)),
    actionNewVertex(new QAction(QPixmap("://toolbar/MeshTINNewVertex.png"),tr("Draw vertex"),this)),
    mapToolNewVertex(new ReosMapToolClickPoint(gismanager->getMap())),
    actionRemoveVertex(new QAction(QPixmap("://toolbar/MeshTINRemoveVertex.png"),tr("Remove vertex"),this)),
    mapToolRemoveVertex(new ReosMapToolSelection(gismanager->getMap())),
    actionNewHardLineSegment(new QAction(QPixmap("://toolbar/MeshTINNewSegment.png"),tr("Draw hard line"),this)),
    mapToolHardLineSegment(new ReosTinMapToolHardLineSegement(gismanager->getMap(),this)),
    actionRemoveSegment(new QAction(QPixmap("://toolbar/MeshTINRemoveSegment.png"),tr("Remove hard line"),this)),
    mapToolRemoveSegment(new ReosMapToolSelection(gismanager->getMap())),
    actionFlipFaces(new QAction(QPixmap("://toolbar/MeshFlipFaces.png"),tr("Flip faces"),this)),
    mapToolFlipFaces(new ReosTinMapToolFlipFaces(gismanager->getMap(),this)),
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

    mapToolFlipFaces->setAction(actionFlipFaces);
    actionFlipFaces->setCheckable(true);



    groupAction->addAction(actionNewTinLayer);
    groupAction->addAction(actionOpenTinLayer);
    groupAction->addAction(actionSaveTinLayer);
    groupAction->addAction(actionNewVertex);
    groupAction->addAction(actionRemoveVertex);
    groupAction->addAction(actionNewHardLineSegment);
    groupAction->addAction(actionRemoveSegment);
    groupAction->addAction(actionFlipFaces);
    groupAction->addAction(actionTriangulateTIN);

    actionEditList.append(actionNewVertex);
    actionEditList.append(actionNewHardLineSegment);
    actionEditList.append(actionRemoveVertex);
    actionEditList.append(actionTriangulateTIN);
    actionEditList.append(actionRemoveSegment);
    actionEditList.append(actionFlipFaces);
    enableEditAction(false);

    uiDialog=new HdTinEditorUiDialog(mMap->getMapCanvas());
    uiDialog->setActions(getActions());
    mZSpecifierWidget= new ReosVertexZSpecifierWidget(mMap,mDomain,uiDialog);
    uiDialog->setZSpecifierWidet(mZSpecifierWidget);
    mZSpecifierWidget->hide();

    connect(mGisManager,&ReosGisManager::mapCrsChanged,this,&ReosTinEditorUi::mapCrsChanged);

    connect(actionNewTinLayer,&QAction::triggered,this,&ReosTinEditorUi::newTinLayer);
    connect(actionOpenTinLayer,&QAction::triggered,this,&ReosTinEditorUi::openTin);
    connect(actionSaveTinLayer,&QAction::triggered,this,&ReosTinEditorUi::saveTin);

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

    connect(actionFlipFaces,&QAction::triggered,this,&ReosTinEditorUi::startFlipFaces);

    connect(gismanager,&ReosGisManager::currentLayerChanged,this,&ReosTinEditorUi::currentLayerChanged);
    connect(gismanager,&ReosGisManager::layerHasToBeUpdated,this,&ReosTinEditorUi::layerHasToBeUpdated);
    connect(gismanager,&ReosGisManager::layerHasToBeRemoved,this,&ReosTinEditorUi::layerHasToBeRemoved);

    connect(uiDialog,&HdTinEditorUiDialog::closed,this,&ReosTinEditorUi::widgetClosed);
    connect(uiDialog,&HdTinEditorUiDialog::escapePressed,mMap,&ReosMap::stopMapTool);

    gismanager->setCRS(QgsCoordinateReferenceSystem());
}

void ReosTinEditorUi::setMeshLayer(QgsMeshLayer *meshLayer)
{
    if (meshLayer==mMeshLayer)
        return;

    mMeshLayer=meshLayer;
    updateMeshLayer();
    enableEditAction(mTIN != nullptr);
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
    if (!mTIN)
        return nullptr;

    bool ok;
    QPointF meshPoint=meshCoordinates(mapPoint,ok);
    if (!ok)
        return nullptr;
    return mTIN->vertex(meshPoint.x(),meshPoint.y());
}

QList<QPointF> ReosTinEditorUi::mapFace(const QPointF &mapPoint) const
{
    QList<QPointF> vertices;
    FacePointer face=realWorldFace(mapPoint);

    if (!face)
        return vertices;

    for (int i=0;i<face->verticesCount();++i)
    {
        VertexPointer v=face->vertexPointer(i);
        bool ok;
        QPointF mp=mapCoordinates(QPointF(v->x(),v->y()),ok);
        if (ok)
            vertices.append(mp);
    }

    return vertices;
}

FacePointer ReosTinEditorUi::realWorldFace(const QPointF &mapPoint) const
{
    if (!mTIN)
        return nullptr;

    bool ok;
    QPointF meshPoint=meshCoordinates(mapPoint,ok);
    if (!ok)
        return nullptr;
    return mTIN->face(meshPoint.x(),meshPoint.y());
}

bool ReosTinEditorUi::isFlipable(FacePointer f1, FacePointer f2) const
{
    return mTIN->isFlipable(f1,f2);
}

VertexPointer ReosTinEditorUi::addRealWorldVertex(const QPointF &mapPoint, double z)
{
    VertexPointer vert=nullptr;

    if (mTIN)
    {
        bool ok;
        QPointF p=meshCoordinates(mapPoint,ok);
        if (!ok)
            return nullptr;
        vert=mTIN->addVertex(p.x(),p.y());
        vert->setZValue(z);
        addMapVertex(mapPoint,vert);

    }

    return vert;
}

VertexPointer ReosTinEditorUi::addRealWorldVertex(const QPointF &mapPoint)
{
    VertexPointer vert=nullptr;

    if (mTIN)
    {
        bool ok;
        QPointF p=meshCoordinates(mapPoint,ok);
        if (!ok)
            return nullptr;
        vert=mTIN->addVertex(p.x(),p.y());
        mZSpecifierWidget->assignZSpecifier(vert);
        addMapVertex(mapPoint,vert);

    }

    return vert;

}


void ReosTinEditorUi::removeVertex(VertexPointer vert)
{
    if (!vert)
        return;
    auto neighbours=mTIN->hardNeighbours(vert);

    domain()->removeVertex(static_cast<ReosMeshItemVertex*>(vert->graphicPointer()));
    vert->setGraphicPointer(nullptr);
    mTIN->removeVertex(vert);

    for (auto n:neighbours)
    {
        updateGraphics(n);
    }

}


void ReosTinEditorUi::doCommand(ReosTinUndoCommandNewVertex *command)
{
    addRealWorldVertex(command->mMapPoint);
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

//    VertexPointer finalVertex=realWorldVertex(command->mMapPointSecond);

//    //remove the segments
//    QList<PointAndNeighbours> &verticesPositions=command->mVerticesPositionAndStructureMemory;
//    for (int i=0;i<verticesPositions.count()-1;++i)
//    {
//        VertexPointer v1=realWorldVertex(verticesPositions.at(i).point);
//        VertexPointer v2=realWorldVertex(verticesPositions.at(i+1).point);

//    }

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

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandNewSegmentWithExistingSecondVertex*)
{
    //remove the segments
//    QList<PointAndNeighbours> &verticesPositions=command->mVerticesPositionAndStructureMemory;
//    for (int i=0;i<verticesPositions.count()-1;++i)
//    {
//        VertexPointer v1=realWorldVertex(verticesPositions.at(i).point);
//        VertexPointer v2=realWorldVertex(verticesPositions.at(i+1).point);
//    }
}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandRemoveVertex *command)
{
    VertexPointer vertex=realWorldVertex(command->mMapPoint);

    if (vertex)
    {
        auto neighboursVertices=mTIN->hardNeighbours(vertex);
        auto graphicVertex=static_cast<ReosMeshItemVertex*>(vertex->graphicPointer());
        mTIN->removeVertex(vertex);
        mDomain->removeVertex(graphicVertex);
        for (auto n:neighboursVertices)
            updateGraphics(n);
    }

}

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandRemoveVertex *)
{

}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandRemoveHardLine *command)
{
    VertexPointer vertex1=realWorldVertex(command->mMapPointForVertex1);
    VertexPointer vertex2=realWorldVertex(command->mMapPointForVertex2);

    removeHardLine(vertex1,vertex2);
}

void ReosTinEditorUi::doCommand(ReosTinUndoCommandFlipFaces *command)
{
    auto face_1=realWorldFace(command->mFaceCentroidToFlip_1);
    auto face_2=realWorldFace(command->mFaceCentroidToFlip_2);

    auto newFaces=mTIN->flipFaces(face_1,face_2);
    command->setFacesCentroid(newFaces[0],newFaces[1]);
}

void ReosTinEditorUi::undoCommand(ReosTinUndoCommandFlipFaces *command)
{
    auto face_1=realWorldFace(command->mFaceCentroidToFlip_1);
    auto face_2=realWorldFace(command->mFaceCentroidToFlip_2);

    auto newFaces=mTIN->flipFaces(face_1,face_2);
    command->setFacesCentroid(newFaces[0],newFaces[1]);
};


void ReosTinEditorUi::updateMeshLayer()
{
    mTransform.reset(nullptr);
    if(mMeshLayer==nullptr)
        mTIN=nullptr;
    else
    {
        if(mMeshLayer->dataProvider()->name()==QStringLiteral("TIN"))
        {
            mTIN=static_cast<TINProvider*>(mMeshLayer->dataProvider())->tin();
            mTransform.reset(new QgsCoordinateTransform(mMeshLayer->crs(),mMap->getCoordinateReferenceSystem(),QgsProject::instance()));
            mUndoStack=mUndoStacks.value(mTIN,nullptr);

            if (mUndoStack==nullptr)
            {
                mUndoStack=new QUndoStack(this);
                mUndoStacks[mTIN]=mUndoStack;
            }

            emit activeUndoStack(mUndoStack);
        }
        else {
            mTIN=nullptr;
        }
    }
}

bool ReosTinEditorUi::writeToFile(QString fileName)
{
    if (mTIN)
        return mTIN->writeUGRIDFormat(fileName.toStdString());
    else
        return false;
}

void ReosTinEditorUi::newCommand(QUndoCommand *command)
{
    //override for disable the sending of this command to the undo stck because, undo not works because of potential CGAL issue
    command->redo();

    if (uiDialog->autoUpdate())
        updateMesh();
}



void ReosTinEditorUi::newVertex(const QPointF &mapPoint)
{
    uiDialog->setFocus();

    if (!mTIN || realWorldVertex(mapPoint))
        return;

    auto command=new ReosTinUndoCommandNewVertex(this,mapPoint);
    newCommand(command);
}

void ReosTinEditorUi::stopNewVertex()
{
    stopVertexEntry();
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
        if(mTIN->isOnHardLine(realWorldVertex))
        {
            QMessageBox::warning(uiDialog,tr("Suppression d'un sommet sur une ligne d'arrête"),tr("Il est nécessaire de supprimer d'abord la ou les lignes d'arrêtes"));
        }
        else
        {
            bool ok;
            QPointF mapPoint=mapCoordinates(QPointF(realWorldVertex->x(),realWorldVertex->y()),ok);
            if (!ok)
                return;
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

        bool ok1,ok2;
        QPointF mapPoint1=mapCoordinates(QPointF(v1->x(),v1->y()),ok1);
        QPointF mapPoint2=mapCoordinates(QPointF(v2->x(),v2->y()),ok2);

        if (!ok1 || !ok2)
            return;

        auto command= new ReosTinUndoCommandRemoveHardLine(this,mapPoint1,mapPoint2);
        newCommand(command);
    }
}

void ReosTinEditorUi::startFlipFaces()
{
    mMap->setMapTool(mapToolFlipFaces);
}

void ReosTinEditorUi::flipFaces(FacePointer f1, FacePointer f2)
{
    auto command=new ReosTinUndoCommandFlipFaces(f1,f2,this);
    newCommand(command);
}

void ReosTinEditorUi::startNewHardLineSegment()
{
    mMap->setMapTool(mapToolHardLineSegment);
    startVertexEntry();
}

void ReosTinEditorUi::newSegment(ReosMeshItemVertex *firstVertex, const QPointF &secondMapPoint)
{
    if (!mTIN)
        return;

    bool ok;
    QPointF mapPointFirst=mapCoordinates(QPointF(firstVertex->realWorldVertex()->x(),firstVertex->realWorldVertex()->y()),ok);
    if (!ok)
        return;
    auto command=new ReosTinUndoCommandNewSegmentWithNewSecondVertex(this,mapPointFirst,secondMapPoint);
    newCommand(command);
}

void ReosTinEditorUi::newSegment(ReosMeshItemVertex *firstVertex, ReosMeshItemVertex *secondVertex)
{
    if (!mTIN)
        return;

    if (firstVertex==secondVertex)
        return;

    auto command=new ReosTinUndoCommandNewSegmentWithExistingSecondVertex(this,firstVertex->position(),secondVertex->position());
    newCommand(command);
}

void ReosTinEditorUi::stopNewHardLineSegment()
{
    stopVertexEntry();
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
    //QPointF mapPoint=mapCoordinates(realWordlPoint);

    return addMapVertex(realWorldVertex);
//    ReosMeshItemVertex *mapVertex=mDomain->addVertex(mapPoint);
//    mapVertex->setRealWorldVertex(realWorldVertex);
//    realWorldVertex->setGraphicPointer(mapVertex);
//    return mapVertex;
}



void ReosTinEditorUi::addSegment(VertexPointer v1, VertexPointer v2, QList<PointAndNeighbours> &oldNeigboursStructure)
{
    //add real world segment
    std::list<VertexPointer> hardLineVertices=mTIN->addHardLine(v1,v2);

    //set Z value for next
    for (auto vert:hardLineVertices)
    {
        if(vert->graphicPointer()==nullptr)
        {
            mZSpecifierWidget->assignZSpecifier(vert);
        }
    }


    //store the old structure arround the new vertices from the graphics item
    oldNeigboursStructure.clear();
    for (auto vert:hardLineVertices)
    {
        PointAndNeighbours currentStructure;
        currentStructure.point=QPointF(vert->x(),vert->y());
        auto realWorldNeighbours=mTIN->hardNeighbours(vert);
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
        auto neighbours=mTIN->hardNeighbours(vert);
        for (auto nv:neighbours)
            updateGraphics(nv);
    }
}

void ReosTinEditorUi::removeHardLine(VertexPointer v1, VertexPointer v2)
{
    auto verticesList=mTIN->removeHardLine(v1,v2);

    for (auto v:verticesList)
        updateGraphics(v);
}

void ReosTinEditorUi::populateDomain()
{
    mDomain->clear();
    if (mTIN)
    {
        auto tinReader=mTIN->getReader();
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
        ReosTin* tin=static_cast<TINProvider*>(mMeshLayer->dataProvider())->tin();

        if (tin->isDirty())
        {
            if (QMessageBox::warning(uiDialog,tr("Sauvegarde du maillage"),
                                     tr("Le maillage %1 a été modifé. Voulez-vous le sauvegarder ?").arg(meshLayer->name())
                                     ,QMessageBox::Yes|QMessageBox::No,QMessageBox::Yes)==QMessageBox::Yes)
            {
                ///TODO : need to be completed
            }
        }


        QUndoStack *us=mUndoStacks.value(tin,nullptr);
        if (us)
            us->deleteLater();
        mUndoStacks.remove(tin);

    }
}

void ReosTinEditorUi::mapCrsChanged(const QgsCoordinateReferenceSystem &crs)
{
    if (mMeshLayer)
        mTransform.reset(new QgsCoordinateTransform(mMeshLayer->dataProvider()->crs(),crs,QgsProject::instance()));
}


void ReosTinEditorUi::startNewVertex()
{
    mMap->setMapTool(mapToolNewVertex);
    startVertexEntry();
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
        newTin(dial->fileName(),dial->name(),dial->crs());
    }

}

void ReosTinEditorUi::newTin(const QString &fileName, const QString &name,const QgsCoordinateReferenceSystem &crs)
{
    QFileInfo fileInfo(fileName);
    if (fileInfo.exists())
        QFile::remove(fileName);

    TINProvider::createEmptyTIN(fileName,crs);

    auto layer=new QgsMeshLayer(fileName,name,"TIN");

    if (layer->isValid())
    {
        mGisManager->addLayer(layer);
        setMeshLayer(layer);
    }
}

bool ReosTinEditorUi::openTin()
{
    QString fileName=QFileDialog::getOpenFileName();
    return openTinWithFileName(fileName);
}

bool ReosTinEditorUi::openTinWithFileName(QString fileName)
{
    if (fileName!="")
    {
        auto layer=new QgsMeshLayer(fileName,fileName,"TIN");
        mGisManager->addLayer(layer);
        setMeshLayer(layer);
        return true;
    }

    return false;
}



void ReosTinEditorUi::widgetClosed()
{
    emit widgetVisibility(false);
}

void ReosTinEditorUi::startVertexEntry()
{
    uiDialog->setFocus();
    mZSpecifierWidget->start();
}

void ReosTinEditorUi::stopVertexEntry()
{
    mZSpecifierWidget->stop();
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
    vert->setGraphicPointer(vertexGraphic);
    return vertexGraphic;
}


QPointF ReosTinEditorUi::mapCoordinates(const QPointF &meshCoordinate, bool &ok) const
{
    ok=true;
    if (mTransform)
    {
        try {
            return mTransform->transform(meshCoordinate.x(),meshCoordinate.y(),QgsCoordinateTransform::ForwardTransform).toQPointF();
        } catch (QgsCsException &except) {
            QString message=tr("Error when transforming coordinate");
            message.append("/n");
            message.append(except.what());
            QMessageBox::critical(mGisManager->getMap()->getMapCanvas(),tr("Coordinate transform"),tr("Error when transforming coordinate"));
            ok=false;
            return QPointF();
        }
    }
    else
        return meshCoordinate;
}

QPointF ReosTinEditorUi::meshCoordinates(const QPointF &mapCoordinate, bool &ok) const
{
    ok=true;
    if (mTransform)
    {
         try {
             return mTransform->transform(mapCoordinate.x(),mapCoordinate.y(),QgsCoordinateTransform::ReverseTransform).toQPointF();
        } catch (QgsCsException &except) {
            QString message=tr("Error when transforming coordinate");
            message.append("/n");
            message.append(except.what());
            QMessageBox::critical(mGisManager->getMap()->getMapCanvas(),tr("Coordinate transform"),tr("Error when transforming coordinate"));
            ok=false;
            return QPointF();
        }
    }
    else
        return mapCoordinate;
}

ReosMeshItemVertexAndNeighbours ReosTinEditorUi::saveStructure(ReosMeshItemVertex *vert) const
{
    auto neighboursRealWorldVertex=mTIN->hardNeighbours(vert->realWorldVertex());

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

void ReosTinEditorUi::restoreStructure(VertexPointer, const PointAndNeighbours) const
{

}

void ReosTinEditorUi::updateGraphics(VertexPointer realWorldVertex)
{
    if (!realWorldVertex)
        return;

    if (realWorldVertex->graphicPointer()==nullptr)
        addMapVertex(realWorldVertex);

    auto neighboursVertex=mTIN->hardNeighbours(realWorldVertex);
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

void ReosTinMapToolHardLineSegement::suspend()
{
    rubberBand->hide();
    ReosMapTool::suspend();
    showWhenMoving=false;
}

void ReosTinMapToolHardLineSegement::unsuspend()
{
    ReosMapTool::unsuspend();
    showWhenMoving=true;
}

void ReosTinMapToolHardLineSegement::canvasPressEvent(QgsMapMouseEvent *e)
{
    if (e->button()==Qt::RightButton)
    {
        reset();
        return;
    }

    if (e->button()!=Qt::LeftButton)
    {
        return;
    }

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
        inProgress_=true;
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

void ReosTinMapToolHardLineSegement::canvasMoveEvent(QgsMapMouseEvent *e)
{
    if(showWhenMoving)
    {
        rubberBand->show();
        showWhenMoving=false;
    }
    rubberBand->movePoint(e->mapPoint());
}


void ReosTinMapToolFlipFaces::canvasPressEvent(QgsMapMouseEvent *e)
{
    if (currentFaceItem)
        delete currentFaceItem;
    currentFaceItem=nullptr;

    FacePointer face=mUiEditor->realWorldFace(e->mapPoint().toQPointF());

    if (!face)
        return;

    if(!firstFace)
    {
        firstFace=face;
        QList<QPointF> facePoints=selectedFace(e->mapPoint().toQPointF());
        firstFaceItem=new ReosMeshItemFace(facePoints,canvas());
        firstFaceItem->setBrushColor(firstFaceColor);
    }
    else
    {
        FacePointer secondFace=mUiEditor->realWorldFace(e->mapPoint().toQPointF());
        if (faceCoupleIsValid(firstFace,secondFace))
        {
            mUiEditor->flipFaces(firstFace,secondFace);
            reset();
        }

    }




}

void ReosTinMapToolFlipFaces::canvasMoveEvent(QgsMapMouseEvent *e)
{
    if (currentFaceItem)
        delete currentFaceItem;

    QList<QPointF> face=selectedFace(e->mapPoint().toQPointF());
    if (face.count()==0)
    {
        currentFaceItem=nullptr;
        return;
    }

    currentFaceItem=new ReosMeshItemFace(face,canvas());
    if (!firstFace)
    {
        currentFaceItem->setBrushColor(currentFaceColor);
    }
    else
    {
        auto secondFace=mUiEditor->realWorldFace(e->mapPoint().toQPointF());

        if(mUiEditor->isFlipable(firstFace,secondFace))
        {
            currentFaceItem->setBrushColor(currentFaceColor);
        }
        else
        {
            currentFaceItem->setBrushColor(wrongFaceColor);
        }
    }

}

QList<QPointF> ReosTinMapToolFlipFaces::selectedFace(QPointF mapPoint)
{
    return mUiEditor->mapFace(mapPoint);
}

bool ReosTinMapToolFlipFaces::faceCoupleIsValid(FacePointer f1, FacePointer f2)
{
    if (!(f1&&f2))
        return false;

    if (!mUiEditor->isFlipable(f1,f2))
        return false;

    return true;
}
