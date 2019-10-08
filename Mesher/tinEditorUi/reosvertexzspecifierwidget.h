/***************************************************************************
                      reosvertexzspecifierwidget.h
                     --------------------------------------
Date                 : 01-10-2019
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

#ifndef REOSVERTEXZSPECIFIERWIDGET_H
#define REOSVERTEXZSPECIFIERWIDGET_H

#include <QWidget>
#include <QLineEdit>

#include "../ReosMesh/reosvertexzspecifier.h"
#include "../../Reos/Form/reosform.h"
#include "../../GIS/reosmaptool.h"
#include "../ReosMesh/reosmeshgenerator.h"
#include "reosmapmeshitem.h"
#include "../../GIS/reosmap.h"

class ReosVertexZSpecifierEntryWidgetModel;


namespace Ui {
class ReosVertexZSpecifierWidget;
}

class ReosVertexZSpecifierEntryWidget: public QWidget
{
public:
    ReosVertexZSpecifierEntryWidget(QWidget *parent);
    virtual ~ReosVertexZSpecifierEntryWidget();

    virtual QIcon icon() const=0;

    virtual ReosVertexZSpecifierFactory& factory() =0;

    virtual void start();
    virtual void stop();


};

class ReosVertexZSpecifierSimpleValueWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
public:
    ReosVertexZSpecifierSimpleValueWidget(QWidget *parent);

    QIcon icon() const;

    ReosVertexZSpecifierFactory& factory();

private slots:
    void ZValueHasBeenEdited();
private:
    ReosVertexZSpecifierSimpleFactory mFactory;
    ReosForm *valueForm;
    ReosFormParameterSimpleDouble *zValueParameterForm;
};

class ReosVertexZSpecifierSelectReferenceMapTool:public ReosMapToolSelection
{
    Q_OBJECT
public:
    ReosVertexZSpecifierSelectReferenceMapTool(ReosMap *map,ReosMapMeshEditorItemDomain *domain);

    void start();

    void returnToPreviousMapTool();

    // QgsMapTool interface
public:
    void canvasMoveEvent(QgsMapMouseEvent *e) override;

private:
    ReosMapMeshEditorItemDomain *mDomain;
    ReosMapTool *mPreviousCurrentMapTool=nullptr;

    // ReosMapTool interface
public slots:
    void askForEscape() override;
};

class ReosVertexZSpecifierDependentOtherVertexWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
public:
    ReosVertexZSpecifierDependentOtherVertexWidget(ReosMap* map,ReosMapMeshEditorItemDomain *domain,QWidget *parent);

    void init();

    ReosVertexZSpecifierFactory& factory();

    void start();

    void stop();

private slots:
    void startSelectReference();

    void updateReferenceText();

    void zoneHasBeenSelected(const QRectF &rect);

    virtual void valueHasBeenEdited();

private:

    virtual void setValueInFactory(double value)=0;
    virtual ReosFormParameterSimpleDouble* makeValueParameterForm(ReosForm *parentForm)=0;

    virtual ReosVertexZSpecifierDependOnOtherVertexFactory & dependentVertexFactory()=0;
    ReosForm *mValueForm;
    ReosFormAction* mSelectReferenceAction;
    ReosFormText* mReferenceText;
    ReosFormParameterSimpleDouble *mValueParameterForm;
    ReosFormParameterSimpleBool *mTakeNewVertexAsReference;

    ReosMap *mMap;
    ReosMapMeshEditorItemDomain *mDomain;

    ReosVertexZSpecifierSelectReferenceMapTool* selectReferenceMapTool=nullptr;


};

class ReosVertexZSpecifierSlopeWidget: public ReosVertexZSpecifierDependentOtherVertexWidget
{

public:
    ReosVertexZSpecifierSlopeWidget(ReosMap* map,ReosMapMeshEditorItemDomain *domain,QWidget *parent);

    QIcon icon() const override;

private:

    ReosVertexZSpecifierDependOnOtherVertexFactory & dependentVertexFactory() override {return mFactory;}
    virtual ReosFormParameterSimpleDouble* makeValueParameterForm(ReosForm *parentForm) override;
    virtual void setValueInFactory(double value) override;

    ReosVertexZSpecifierOtherVertexAndSlopeFactory mFactory;

};

class ReosVertexZSpecifierGapWidget: public ReosVertexZSpecifierDependentOtherVertexWidget
{

public:
    ReosVertexZSpecifierGapWidget(ReosMap* map,ReosMapMeshEditorItemDomain *domain,QWidget *parent);

    QIcon icon() const override;

private:

    ReosVertexZSpecifierDependOnOtherVertexFactory & dependentVertexFactory() override;
    virtual ReosFormParameterSimpleDouble* makeValueParameterForm(ReosForm *parentForm) override;
    virtual void setValueInFactory(double value) override;

    ReosVertexZSpecifierOtherVertexAndGapFactory mFactory;

};



class ReosVertexZSpecifierWidget : public QWidget
{
    Q_OBJECT

public:
    explicit ReosVertexZSpecifierWidget(ReosMap *map,ReosMapMeshEditorItemDomain* domain,QWidget *parent = nullptr);
    ~ReosVertexZSpecifierWidget();

    void assignZSpecifier(VertexPointer vert);

    void start();

    void stop();

private slots:
    void listViewClicked(QModelIndex index);

private:

    void setCurrentZSpecifier(int i);
    void addEntry(ReosVertexZSpecifierEntryWidget* entry);

    Ui::ReosVertexZSpecifierWidget *ui;
    QList<ReosVertexZSpecifierEntryWidget*> mEntryWidgets;
    ReosVertexZSpecifierEntryWidgetModel *mEntriesModel;
    ReosVertexZSpecifierEntryWidget *mCurrentEntryWidget=nullptr;
};



class ReosVertexZSpecifierEntryWidgetModel:public QAbstractListModel
{
public:
    ReosVertexZSpecifierEntryWidgetModel(QList<ReosVertexZSpecifierEntryWidget*> &entriesList, QObject *parent=nullptr);

    // QAbstractItemModel interface
public:
    int rowCount(const QModelIndex &parent) const override;
    QVariant data(const QModelIndex &index, int role) const override;

private:
    QList<ReosVertexZSpecifierEntryWidget*> &mEntriesList;

};

#endif // REOSVERTEXZSPECIFIERWIDGET_H
