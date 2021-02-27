/***************************************************************************
  reosmeteorologicmodelwidget.h - ReosMeteorologicModelWidget

 ---------------------
 begin                : 16.2.2021
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
#ifndef REOSMETEOROLOGICMODELWIDGET_H
#define REOSMETEOROLOGICMODELWIDGET_H

#include <QWidget>

#include "reosactionwidget.h"

class ReosMeteorologicItemModel;
class ReosMeteorologicModel;
class ReosWatershedItemModel;
class ReosMeteorologicModelsCollection;

namespace Ui
{
  class ReosMeteorologicModelWidget;
}

class ReosMeteorologicModelWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosMeteorologicModelWidget( ReosWatershedItemModel *watershedModel,
                                          ReosMeteorologicModelsCollection *meteoModelsCollection,
                                          QWidget *parent = nullptr );

    void setCurrentMeteorologicalModel( int index );

    ~ReosMeteorologicModelWidget();

  signals:
    void currentModelChanged( int index );

  private slots:
    void onAddMeteoModel();
    void onDuplicateMeteoModel();
    void onRemoveMeteoModel();
    void onRenameMeteoModel();
    void onCurrentModelChanged();
    void onMeteoTreeViewContextMenu( const QPoint &pos );


  private:
    ReosMeteorologicItemModel *mMeteorologicItemModel = nullptr;
    ReosMeteorologicModelsCollection *mModelsCollections = nullptr;
    Ui::ReosMeteorologicModelWidget *ui;

    QAction *mActionAddMeteoModel = nullptr;
    QAction *mActionDuplicateMeteoModel = nullptr;
    QAction *mActionRemoveMeteoModel = nullptr;
    QAction *mActionRenameMeteoModel = nullptr;

    ReosMeteorologicModel *currentModel() const;
};

#endif // REOSMETEOROLOGICMODELWIDGET_H
