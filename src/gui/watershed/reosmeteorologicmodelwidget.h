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
#include "reosguicontext.h"

class ReosMeteorologicItemModel;
class ReosMeteorologicModel;
class ReosWatershedItemModel;
class ReosMeteorologicModelsCollection;
class ReosTimeWindow;
class ReosDuration;
class ReosRenderedObject;
class ReosMap;

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
                                          const ReosGuiContext &guiContext );

    void setCurrentMeteorologicalModel( int index );

    ~ReosMeteorologicModelWidget();

    ReosTimeWindow timeWindow() const;
    ReosDuration mapTimeStep() const;

  signals:
    void currentModelChanged( int index );
    void timeWindowChanged();
    void mapTimeStepChanged();

  private slots:
    void onAddMeteoModel();
    void onDuplicateMeteoModel();
    void onRemoveMeteoModel();
    void onRenameMeteoModel();
    void onCurrentModelChanged();
    void onMeteoTreeViewContextMenu( const QPoint &pos );
    void handleRenderedObject();

  private:
    ReosMeteorologicItemModel *mMeteorologicItemModel = nullptr;
    ReosMeteorologicModel *mCurrentModel = nullptr;
    ReosMeteorologicModelsCollection *mModelsCollections = nullptr;
    ReosMap *mMap = nullptr;
    Ui::ReosMeteorologicModelWidget *ui;

    QAction *mActionAddMeteoModel = nullptr;
    QAction *mActionDuplicateMeteoModel = nullptr;
    QAction *mActionRemoveMeteoModel = nullptr;
    QAction *mActionRenameMeteoModel = nullptr;

    QHash<QString, ReosRenderedObject *> mActiveRenderedObject;

    ReosMeteorologicModel *currentModel() const;
};

#endif // REOSMETEOROLOGICMODELWIDGET_H
