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


HdTINEditorZEntryWidget::HdTINEditorZEntryWidget(QWidget *parent):QDialog(parent)
{
    QVBoxLayout *lay=new QVBoxLayout;
    setLayout(lay);

    QHBoxLayout *hLay=new QHBoxLayout;
    ZLineEdit=new QLineEdit;
    hLay->addWidget(new QLabel("Z :"));
    hLay->addWidget(ZLineEdit);

    lay->addLayout(hLay);
    QDialogButtonBox *buttonBox=new QDialogButtonBox(QDialogButtonBox::Ok);
    lay->addWidget(buttonBox);

    ZLineEdit->setFocus();
    ZLineEdit->setText("0");
    setModal(false);

    connect(buttonBox,&QDialogButtonBox::accepted,this,&QDialog::accept);
}

double HdTINEditorZEntryWidget::getZValue()
{
    return ZLineEdit->text().toDouble();
}

void HdTinMapToolNewVertex::canvasPressEvent(QgsMapMouseEvent *e)
{
    emit newVertex(e->mapPoint().toQPointF());
}

HdTinEditorUI::HdTinEditorUI(HdManagerSIG *gismanager, QObject *parent):ReosModule(parent),
    domain(new HdMapMeshEditorItemDomain(gismanager->getMap()->getMapCanvas())),
    gisManager(gismanager),
    mCanvas(gismanager->getMap()->getMapCanvas()),
    actionNewTinLayer(new QAction(QPixmap("://toolbar/MeshNewTIN.png"),tr("Nouveau TIN"),this)),
    actionNewVertex(new QAction(QPixmap("://toolbar/MeshTINNewVertex.png"),tr("Nouveau point"),this)),
    mapToolNewVertex(new HdTinMapToolNewVertex(gismanager->getMap()->getMapCanvas())),
    actionTriangulateTIN(new QAction(QPixmap("://toolbar/MeshTINTriangulation.png"),tr("Triangulation"),this)),
    zEntryWidget(new HdTINEditorZEntryWidget(gismanager->getMap()->getMapCanvas()))
{
    mapToolNewVertex->setAction(actionNewVertex);

    groupAction->addAction(actionNewTinLayer);
    groupAction->addAction(actionNewVertex);
    groupAction->addAction(actionTriangulateTIN);
    actionEditList.append(actionNewVertex);
    actionEditList.append(actionTriangulateTIN);
    enableEditAction(false);

    connect(actionNewTinLayer,&QAction::triggered,this,&HdTinEditorUI::newTinLayer);

    connect(actionNewVertex,&QAction::triggered,this,&HdTinEditorUI::startNewVertex);
    connect(mapToolNewVertex,&HdTinMapToolNewVertex::newVertex,this,&HdTinEditorUI::newVertex);
    connect(actionTriangulateTIN,&QAction::triggered,this,&HdTinEditorUI::triangulateTIN);
    connect(zEntryWidget,&QDialog::accepted,this,&HdTinEditorUI::setZValue);

    connect(gismanager,&HdManagerSIG::currentLayerChanged,this,&HdTinEditorUI::currentLayerChanged);
}

void HdTinEditorUI::setMeshLayer(QgsMeshLayer *meshLayer)
{
    if (meshLayer==mMeshLayer)
        return;

    mMeshLayer=meshLayer;

    if(mMeshLayer==nullptr)
        editor=nullptr;
    else {
        if(mMeshLayer->dataProvider()->name()==QStringLiteral("TIN"))
        {
            editor=static_cast<TINProvider*>(mMeshLayer->dataProvider())->editor();
        }
        else {
            editor=nullptr;
        }
    }
    enableEditAction(editor != nullptr);

    domain->setTINEditor(editor);

}

void HdTinEditorUI::newVertex(const QPointF &p)
{
    if (zEntryWidget->isVisible())
        setZValue();

    domain->addVertex(p);
    if (zEntryWidget->isHidden())
        zEntryWidget->show();
}

void HdTinEditorUI::currentLayerChanged(QgsMapLayer *layer)
{
    if (!layer)
        return;

    if (layer->type()==QgsMapLayerType::MeshLayer)
        setMeshLayer(static_cast<QgsMeshLayer*>(layer));
    else
        setMeshLayer(nullptr);
}

void HdTinEditorUI::startNewVertex()
{
    mCanvas->setMapTool(mapToolNewVertex);
}

void HdTinEditorUI::setZValue()
{
    if( editor->verticesCount()==0)
        return;
    editor->setZValue(editor->verticesCount()-1,zEntryWidget->getZValue());
}

void HdTinEditorUI::triangulateTIN()
{
    editor->generateMesh();
    if (mMeshLayer)
        mMeshLayer->reload();

    mCanvas->refresh();
}

void HdTinEditorUI::enableEditAction(bool enable)
{
    for (auto a:actionEditList)
        a->setEnabled(enable);
}

void HdTinEditorUI::newTinLayer()
{
    gisManager->addLayer(new QgsMeshLayer("-","TIN","TIN"));
}
