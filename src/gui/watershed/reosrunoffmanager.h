/***************************************************************************
  reosrunoffmanager.h - ReosRunoffManager

 ---------------------
 begin                : 21.2.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSRUNOFFMANAGER_H
#define REOSRUNOFFMANAGER_H

#include <QWidget>

#include "reosactionwidget.h"
#include "reosformwidget.h"

class ReosRunoffModelModel;
class ReosFormWidget;
class ReosRunoffModel;
class ReosDataObject;

namespace Ui
{
  class ReosRunoffManager;
}

class REOSGUI_EXPORT ReosRunoffManager : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosRunoffManager( ReosRunoffModelModel *model, QWidget *parent = nullptr );
    ~ReosRunoffManager();

    void loadDataFile();

  public slots:
    void save();

  private slots:
    void onSaveAs();
    void onOpenFile();
    void onAddNewModel( const QString &type );
    void onRemoveRunoffModel( ReosRunoffModel *runoffModel );
    void onCurrentTreeIndexChanged();
    void onTreeViewContextMenu( const QPoint &pos );

  private:
    Ui::ReosRunoffManager *ui;
    ReosRunoffModelModel *mRunoffModelModel;
    ReosFormWidget *mCurrentForm = nullptr;

    bool saveOn( const QString &fileName );

    void selectRunoffModel( ReosRunoffModel *runoffModel );
};

class ReosFormRunoffConstantCoefficientWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const;
};

class ReosFormRunoffGreenAmptWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const;
};

class ReosFormRunofCurveNumberWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const;
};

#endif // REOSRUNOFFMANAGER_H
