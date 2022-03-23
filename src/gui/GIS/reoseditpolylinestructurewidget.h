/***************************************************************************
  reoseditpolylinestructurewidget.h - ReosEditPolylineStructureWidget

 ---------------------
 begin                : 13.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSEDITPOLYLINESTRUCTUREWIDGET_H
#define REOSEDITPOLYLINESTRUCTUREWIDGET_H

#include "reosactionwidget.h"
#include "reosmaptool.h"
#include "reosguicontext.h"

class ReosPolylinesStructure;

class QUndoStack;
class QToolBar;
class ReosMapToolEditPolylineStructure;
class ReosPolylineStructureClassModelList;

namespace Ui
{
  class ReosEditPolylineStructureWidget;
}

class ReosEditPolylineStructureWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosEditPolylineStructureWidget( ReosPolylinesStructure *structure, const ReosGuiContext &context = ReosGuiContext() );
    ~ReosEditPolylineStructureWidget();

    void addToolBarActions( const QList<QAction *> actions );

    void setSettingsWidget( QWidget *widget );
    void setInformationWidget( QWidget *widget );

  signals:
    void boundaryConditionSelectionChanged();

  protected:
    void hideEvent( QHideEvent *e );
    void showEvent( QShowEvent *e );

  private slots:
    void onRenameBoundary();
    void onRemoveBoundary();
    void onZoomOnBoundaryCondition();

  private:
    Ui::ReosEditPolylineStructureWidget *ui;
    QAction *mActionEditLine = nullptr;
    QPointer<ReosPolylinesStructure> mStructure;
    ReosMapToolEditPolylineStructure *mMapToolEditLine = nullptr;
    QToolBar *mToolBar = nullptr;
    QAction *mActionRemoveBoundary = nullptr;
    QAction *mActionZoomOnBoundary = nullptr;
    QAction *mActionRenameBoundary = nullptr;
    ReosPolylineStructureClassModelList *mBoundaryModel = nullptr;
    ReosGuiContext mGuiContext;
};

#endif // REOSEDITPOLYLINESTRUCTUREWIDGET_H
