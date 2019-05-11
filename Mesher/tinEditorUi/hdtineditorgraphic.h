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

#include "../../GIS/hdgismanager.h"
#include "../../Reos/reosmodule.h"

#include "../provider/meshdataprovider.h"
#include "hdmapmeshitem.h"
#include "hdtineditoruidialog.h"
#include "hdtineditornewdialog.h"




class HdTINEditorZEntryWidget: public QDialog
{
public :
    HdTINEditorZEntryWidget(QWidget *parent);

    double getZValue();

private:
    QLineEdit *ZLineEdit;
};


class HdTinMapToolNewVertex:public QgsMapTool
{
    Q_OBJECT
public:
    HdTinMapToolNewVertex(QgsMapCanvas* canvas);

signals:

    void newVertex(const QPointF &p);
    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

class HdTinEditorUi : public ReosModule
{
    Q_OBJECT
public:
    explicit HdTinEditorUi(HdManagerSIG *gismanager,QObject *parent = nullptr):ReosModule(parent),
        mDomain(new HdMapMeshEditorItemDomain(gismanager->getMap()->getMapCanvas())),
        mGisManager(gismanager),
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

        uiDialog=new HdTinEditorUiDialog(mCanvas);
        uiDialog->setActions(getActions());

        connect(actionNewTinLayer,&QAction::triggered,this,&HdTinEditorUi::newTinLayer);

        connect(actionNewVertex,&QAction::triggered,this,&HdTinEditorUi::startNewVertex);
        connect(mapToolNewVertex,&HdTinMapToolNewVertex::newVertex,this,&HdTinEditorUi::newVertex);
        connect(actionTriangulateTIN,&QAction::triggered,this,&HdTinEditorUi::triangulateTIN);
        connect(zEntryWidget,&QDialog::accepted,this,&HdTinEditorUi::setZValue);

        connect(gismanager,&HdManagerSIG::currentLayerChanged,this,&HdTinEditorUi::currentLayerChanged);

        connect(uiDialog,&HdTinEditorUiDialog::closed,this,&HdTinEditorUi::widgetClosed);
    }

    void setMeshLayer(QgsMeshLayer *meshLayer);

public slots:
    void showWidget() override
    {
        uiDialog->show();
    }
    void newVertex(const QPointF &p);

private slots:
    void currentLayerChanged(QgsMapLayer *layer);
    void startNewVertex();
    void setZValue();
    void triangulateTIN();
    void enableEditAction(bool enable);
    void newTinLayer();

    void widgetClosed()
    {
        emit widgetVisibility(false);
    }

private:
    HdMapMeshEditorItemDomain *mDomain;
    HdManagerSIG *mGisManager;
    QgsMapCanvas *mCanvas;
    HdTinEditorUiDialog *uiDialog;

    QgsMeshLayer *mMeshLayer=nullptr;
    TINEditor *mEditor=nullptr;

    QAction *actionNewTinLayer;

    QList<QAction*> actionEditList;
    QAction *actionNewVertex;
    HdTinMapToolNewVertex *mapToolNewVertex;

    QAction *actionTriangulateTIN;

    HdTINEditorZEntryWidget* zEntryWidget;


};

#endif // HDTINEDITORGRAPHIC_H
