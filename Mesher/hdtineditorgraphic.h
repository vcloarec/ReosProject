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

#ifndef HDTINEDITORGRAPHIC_H
#define HDTINEDITORGRAPHIC_H

#include <QObject>
#include <QAction>
#include <QDialog>
#include <QDialogButtonBox>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLineEdit>
#include <QLabel>
#include <QPixmap>

#include <qgsmapcanvas.h>
#include "qgsmeshlayer.h"
#include <qgsmaptool.h>
#include <qgsmapmouseevent.h>

#include "../GIS/hdgismanager.h"
#include "hdmapmeshitem.h"
#include "meshdataprovider.h"
#include "../Reos/reosmodule.h"


class HdTINEditorZEntryWidget: public QDialog
{
public :
    HdTINEditorZEntryWidget(QWidget *parent):QDialog(parent)
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

    double getZValue()
    {
        return ZLineEdit->text().toDouble();
    }

private:
    QLineEdit *ZLineEdit;
};


class HdTINMapToolNewVertex:public QgsMapTool
{
    Q_OBJECT
public:
    HdTINMapToolNewVertex(QgsMapCanvas* canvas):QgsMapTool(canvas){}

signals:

    void newVertex(const QPointF &p);
    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e) override
    {
        emit newVertex(e->mapPoint().toQPointF());
    }
};

class HdTINEditorUI : public ReosModule
{
    Q_OBJECT
public:
    explicit HdTINEditorUI(HdManagerSIG *gismanager,QObject *parent = nullptr):ReosModule(parent),
        domain(new HdMapMeshEditorItemDomain(gismanager->getMap()->getMapCanvas())),
        gisManager(gismanager),
        mCanvas(gismanager->getMap()->getMapCanvas()),
        actionNewTinLayer(new QAction(QPixmap("://toolbar/MeshNewTIN.png"),tr("Nouveau TIN"),this)),
        actionNewVertex(new QAction(QPixmap("://toolbar/MeshTINNewVertex.png"),tr("Nouveau point"),this)),
        mapToolNewVertex(new HdTINMapToolNewVertex(gismanager->getMap()->getMapCanvas())),
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

        connect(actionNewTinLayer,&QAction::triggered,this,&HdTINEditorUI::newTinLayer);

        connect(actionNewVertex,&QAction::triggered,this,&HdTINEditorUI::startNewVertex);
        connect(mapToolNewVertex,&HdTINMapToolNewVertex::newVertex,this,&HdTINEditorUI::newVertex);
        connect(actionTriangulateTIN,&QAction::triggered,this,&HdTINEditorUI::triangulateTIN);
        connect(zEntryWidget,&QDialog::accepted,this,&HdTINEditorUI::setZValue);

        connect(gismanager,&HdManagerSIG::currentLayerChanged,this,&HdTINEditorUI::currentLayerChanged);
    }

    void setMeshLayer(QgsMeshLayer *meshLayer)
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

public slots:
    void newVertex(const QPointF &p)
    {
        if (zEntryWidget->isVisible())
            setZValue();

        domain->addVertex(p);
        if (zEntryWidget->isHidden())
            zEntryWidget->show();
    }


private slots:
    void currentLayerChanged(QgsMapLayer *layer)
    {
        if (!layer)
            return;

        if (layer->type()==QgsMapLayerType::MeshLayer)
            setMeshLayer(static_cast<QgsMeshLayer*>(layer));
        else
            setMeshLayer(nullptr);
    }

    void startNewVertex()
    {
        mCanvas->setMapTool(mapToolNewVertex);
    }

    void setZValue()
    {
        editor->setZValue(editor->verticesCount()-1,zEntryWidget->getZValue());
    }

    void triangulateTIN()
    {
        editor->generateMesh();
//        if (mMeshLayer)
//            mMeshLayer->reload();
        QgsProject::instance()->reloadAllLayers();
        mCanvas->refresh();
    }

    void enableEditAction(bool enable)
    {
        for (auto a:actionEditList)
            a->setEnabled(enable);
    }

    void newTinLayer()
    {
        gisManager->addLayer(new QgsMeshLayer("-","TIN","TIN"));
    }

private:
    HdMapMeshEditorItemDomain *domain;
    HdManagerSIG *gisManager;
    QgsMapCanvas *mCanvas;

    QgsMeshLayer *mMeshLayer=nullptr;
    TINEditor *editor=nullptr;

    QAction *actionNewTinLayer;

    QList<QAction*> actionEditList;
    QAction *actionNewVertex;
    HdTINMapToolNewVertex *mapToolNewVertex;

    QAction *actionTriangulateTIN;

    HdTINEditorZEntryWidget* zEntryWidget;
};

#endif // HDTINEDITORGRAPHIC_H
