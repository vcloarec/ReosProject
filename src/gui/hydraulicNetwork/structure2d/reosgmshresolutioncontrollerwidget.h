/***************************************************************************
  reosgmshresolutioncontroller.h - ReosGmshResolutionController

 ---------------------
 begin                : 31.1.2022
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
#ifndef REOSGMSHRESOLUTIONCONTROLLERWIDGET_H
#define REOSGMSHRESOLUTIONCONTROLLERWIDGET_H

#include <QWidget>

#include "reosgmshgenerator.h"
#include "reosmapitem.h"

class ReosMapToolEditPolygonStructure;
class ReosGuiContext;
class ReosMap;
class ReosHydraulicStructure2D;

namespace Ui
{
  class ReosGmshResolutionControllerWidget;
}

class ReosGmshResolutionControllerWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosGmshResolutionControllerWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &guiContext );
    ~ReosGmshResolutionControllerWidget();

    void addToolBarActions( const QList<QAction *> actions );

  protected:
    void hideEvent( QHideEvent *e );
    void showEvent( QShowEvent *e );

  private slots:
    void addClass();

  private:
    Ui::ReosGmshResolutionControllerWidget *ui;
    ReosMap *mMap = nullptr;
    QPointer<ReosMeshResolutionController> mController;

    QAction *mActionEditResolutionPolygons = nullptr;
    ReosMapToolEditPolygonStructure *mMapToolEditResolutionPolygon = nullptr;
    ReosMapPolygonStructure mMapStructureItem;
    QToolBar *mToolBar = nullptr;
};

#endif // REOSGMSHRESOLUTIONCONTROLLERWIDGET_H
