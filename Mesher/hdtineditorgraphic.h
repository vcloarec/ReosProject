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
    HdTINEditorZEntryWidget(QWidget *parent);

    double getZValue();

private:
    QLineEdit *ZLineEdit;
};


class HdTinMapToolNewVertex:public QgsMapTool
{
    Q_OBJECT
public:
    HdTinMapToolNewVertex(QgsMapCanvas* canvas):QgsMapTool(canvas){}

signals:

    void newVertex(const QPointF &p);
    // QgsMapTool interface
public:
    void canvasPressEvent(QgsMapMouseEvent *e) override;
};

class HdTinEditorUI : public ReosModule
{
    Q_OBJECT
public:
    explicit HdTinEditorUI(HdManagerSIG *gismanager,QObject *parent = nullptr);

    void setMeshLayer(QgsMeshLayer *meshLayer);

public slots:
    void newVertex(const QPointF &p);


private slots:
    void currentLayerChanged(QgsMapLayer *layer);
    void startNewVertex();
    void setZValue();
    void triangulateTIN();
    void enableEditAction(bool enable);
    void newTinLayer();

private:
    HdMapMeshEditorItemDomain *domain;
    HdManagerSIG *gisManager;
    QgsMapCanvas *mCanvas;

    QgsMeshLayer *mMeshLayer=nullptr;
    TINEditor *editor=nullptr;

    QAction *actionNewTinLayer;

    QList<QAction*> actionEditList;
    QAction *actionNewVertex;
    HdTinMapToolNewVertex *mapToolNewVertex;

    QAction *actionTriangulateTIN;

    HdTINEditorZEntryWidget* zEntryWidget;
};

#endif // HDTINEDITORGRAPHIC_H
