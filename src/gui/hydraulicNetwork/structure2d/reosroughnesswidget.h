/***************************************************************************
  reosroughnesswidget.h - ReosRoughnessWidget

 ---------------------
 begin                : 17.3.2022
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
#ifndef REOSROUGHNESSWIDGET_H
#define REOSROUGHNESSWIDGET_H

#include <QWidget>

#include "reosmapitem.h"

namespace Ui
{
  class ReosRoughnessWidget;
}

class QToolBar;
class ReosGuiContext;
class ReosPolygonsClassified;
class ReosMap;
class ReosPolygonStructureClassModelList;
class ReosMapToolEditPolygonsClassified;
class ReosRoughnessStructure;
class ReosHydraulicStructure2D;

class ReosRoughnessWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosRoughnessWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &guiContext );
    ~ReosRoughnessWidget();

  protected:
    void hideEvent( QHideEvent *e );
    void showEvent( QShowEvent *e );

  private slots:
    void addRougness();
    void removeCurrentRoughness();

  private:
    Ui::ReosRoughnessWidget *ui;
    ReosMap *mMap = nullptr;
    QPointer<ReosRoughnessStructure> mStructure;
    ReosPolygonStructureClassModelList *mModel = nullptr;

    QAction *mActionEditRoughnessPolygons = nullptr;
    ReosMapToolEditPolygonsClassified *mMapToolEditResolutionPolygon = nullptr;
    ReosMapPolygonStructure mMapStructureItem;
    QToolBar *mToolBar = nullptr;

    QString mCurrentClass;
};

#endif // REOSROUGHNESSWIDGET_H
