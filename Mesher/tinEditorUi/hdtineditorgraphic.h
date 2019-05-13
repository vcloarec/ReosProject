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




class HdTinMapToolNewVertex:public HdMapTool
{
    Q_OBJECT
public:
    HdTinMapToolNewVertex(HdMap *map);

signals:
    void newVertex(const QPointF &p);

public:
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

class HdTinEditorUi : public ReosModule
{
    Q_OBJECT
public:

    explicit HdTinEditorUi(HdManagerSIG *gismanager,QObject *parent = nullptr);

    void setMeshLayer(QgsMeshLayer *meshLayer);
    HdMapMeshEditorItemDomain *domain() const {return mDomain;}

public slots:
    void showWidget() override
    {
        uiDialog->show();
    }
    void startNewVertex();
    VertexPointer newVertex(const QPointF &p);
    void stopNewVertex()
    {
        setNoneMode();
    }

    void populateDomain();

private:
    void currentLayerChanged(QgsMapLayer *layer);
    double zValue(const QPointF &p)
    {
        Q_UNUSED(p);

        if (zValueMode==HdTinEditorUiDialog::level)
        {
            return uiDialog->lineEditText().toDouble();
        }

        return 0;
    }
    void triangulateTIN();
    void enableEditAction(bool enable);
    void newTinLayer();

    void widgetClosed()
    {
        emit widgetVisibility(false);
    }

    void addVertexToDomain(const QPointF& p)
    {
        mDomain->addVertex(p);
    }

    void setLevelMode()
    {
        zValueMode=HdTinEditorUiDialog::level;
        uiDialog->setZValueMode(zValueMode);
    }

    void setNoneMode()
    {
        zValueMode=HdTinEditorUiDialog::none;
        uiDialog->setZValueMode(zValueMode);
    }



private:
    HdMapMeshEditorItemDomain *mDomain;
    HdManagerSIG *mGisManager;
    HdMap *mMap;
    HdTinEditorUiDialog *uiDialog;
    HdTinEditorUiDialog::ZValueMode zValueMode=HdTinEditorUiDialog::none;

    QgsMeshLayer *mMeshLayer=nullptr;
    TINEditor *mEditor=nullptr;

    QAction *actionNewTinLayer;

    QList<QAction*> actionEditList;
    QAction *actionNewVertex;
    HdTinMapToolNewVertex *mapToolNewVertex;

    QAction *actionTriangulateTIN;

};

#endif // HDTINEDITORGRAPHIC_H
