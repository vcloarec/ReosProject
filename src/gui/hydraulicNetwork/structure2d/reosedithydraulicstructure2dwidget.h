/***************************************************************************
  reosedithydraulicstructure2dwidget.h - ReosEditHydraulicStructure2DWidget

 ---------------------
 begin                : 10.1.2022
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
#ifndef REOSEDITSTRUCTURE2DWIDGET_H
#define REOSEDITSTRUCTURE2DWIDGET_H

#include <QWidget>

#include "reosactionwidget.h"
#include "reosguicontext.h"
#include "reosmapitem.h"
#include "reosparameterwidget.h"

class QUndoStack;
class ReosMapToolEditPolylineStructure;
class ReosHydraulicStructure2D;

namespace Ui
{
  class ReosEditStructure2DWidget;
  class ReosEditStructureGeometry2DWidget;
}

class ReosEditHydraulicStructure2DWidget : public ReosStackedPageWidget
{
    Q_OBJECT
  public:
    ReosEditHydraulicStructure2DWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context );
    ~ReosEditHydraulicStructure2DWidget();

  private slots:
    void onMeshOptionListChanged( int row );

  private:
    Ui::ReosEditStructure2DWidget *ui;
    ReosMap *mMap = nullptr;
    ReosHydraulicStructure2D *mStructure2D = nullptr;
    ReosMapPolylineStructure mMapStructureItem;
    ReosMapItem *mInitialMapStructureItem = nullptr;
};



#endif // REOSEDITSTRUCTURE2DWIDGET_H
