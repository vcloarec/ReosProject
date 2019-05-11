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

HdTinMapToolNewVertex::HdTinMapToolNewVertex(QgsMapCanvas *canvas):QgsMapTool(canvas){}

void HdTinMapToolNewVertex::canvasPressEvent(QgsMapMouseEvent *e)
{
    emit newVertex(e->mapPoint().toQPointF());
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

    mDomain->setTINEditor(mEditor);

}

void HdTinEditorUi::newVertex(const QPointF &p)
{
    if (zEntryWidget->isVisible())
        setZValue();

    mDomain->addVertex(p);
    if (zEntryWidget->isHidden())
        zEntryWidget->show();
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
    mCanvas->setMapTool(mapToolNewVertex);
}

void HdTinEditorUi::setZValue()
{
    if( mEditor->verticesCount()==0)
        return;
    mEditor->setZValue(mEditor->verticesCount()-1,zEntryWidget->getZValue());
}

void HdTinEditorUi::triangulateTIN()
{
    mEditor->generateMesh();
    if (mMeshLayer)
        mMeshLayer->reload();

    mCanvas->refresh();
}

void HdTinEditorUi::enableEditAction(bool enable)
{
    for (auto a:actionEditList)
        a->setEnabled(enable);
}

void HdTinEditorUi::newTinLayer()
{
    auto dial=new HdTinEditorNewDialog(mCanvas);
    if (dial->exec())
    {
        auto layer=new QgsMeshLayer(dial->fileName(),dial->name(),"TIN");
        layer->setCrs(dial->crs());
        mGisManager->addLayer(new QgsMeshLayer(dial->fileName(),dial->name(),"TIN"));
    }

}
