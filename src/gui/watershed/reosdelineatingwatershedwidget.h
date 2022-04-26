/***************************************************************************
                      reosdelineatingwatershedwidget.h
                     --------------------------------------
Date                 : October-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSDELINEATINGWATERSHEDWIDGET_H
#define REOSDELINEATINGWATERSHEDWIDGET_H

#include <QWidget>

#include "reoswatersheddelineating.h"
#include "reosmapitem.h"
#include "reosactionwidget.h"

class ReosWatershedDelineating;
class ReosWatershedModule;
class ReosMapToolEditMapPolygon;
class ReosMapToolMoveMapItem;
class ReosMapToolDrawPolyline;
class ReosMapToolDrawPolygon;
class ReosMapToolDrawExtent;
class ReosMapToolSelectMapItem;
class ReosMapToolDrawPoint;
class ReosMap;
class ReosGuiContext;

namespace Ui
{
  class ReosDelineatingWatershedWidget;
}

class ReosDelineatingWatershedWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosDelineatingWatershedWidget( ReosWatershedModule *watershedModule, const ReosGuiContext &context );

    ~ReosDelineatingWatershedWidget();

    void setEditingDelineatingMapTool( ReosMapToolEditMapPolygon *mapTool );
    void setMoveOutletPointMapTool( ReosMapToolMoveMapItem *mapTool );

  private slots:
    void onDownstreamLineDrawn( const QPolygonF &downstreamLine );
    void onPredefinedExtentDrawn( const QRectF &extent );
    void onBurningLineDrawn( const QPolygonF &burningLine );
    void onBurningLineRemoved( ReosMapItem *item );
    void onDemComboboxChanged();
    void onLoadRasterDem();
    void onDelineateAsked();
    void onAutomaticValidateAsked();
    void onManualWatershedDrawn( const QPolygonF &polygon );
    void onManualOutletDrawn( const QPointF &point );
    void onManualValidateAsked();
    void onMethodChange();
    void onModuleReset();

    void closingWidget() override;


  private:
    Ui::ReosDelineatingWatershedWidget *ui;
    QAction *mAction = nullptr;
    ReosWatershedModule *mModule = nullptr;
    ReosMap *mMap = nullptr;
    ReosMapTool *mCurrentAutomaticMapTool = nullptr;
    ReosMapTool *mCurrentManualMapTool = nullptr;

    QToolBar *mAutomaticToolBar = nullptr;
    QToolBar *mManualToolBar = nullptr;

    ReosMapToolDrawPolyline *mMapToolDrawDownStreamLine = nullptr;
    ReosMapToolDrawExtent *mMapToolDrawPredefinedExtent = nullptr;
    ReosMapToolDrawPolyline *mMapToolDrawBurningLine = nullptr;
    ReosMapToolSelectMapItem *mMapToolRemoveBurningLine = nullptr;
    QAction *mActionDrawDownstreamLine = nullptr;
    QAction *mActionDrawPredefinedExtent = nullptr;
    QAction *mActionDrawAddBurningLine = nullptr;
    QAction *mActionRemoveBurningLine = nullptr;

    ReosMapPolyline mDownstreamLine;
    ReosMapPolygon mWatershedExtent;
    QVector<std::shared_ptr<ReosMapPolyline>> mBurningLines;

    ReosMapToolDrawPolygon *mMapToolDrawWatershed = nullptr;
    ReosMapToolDrawPoint *mMapToolDrawOutletPoint = nullptr;
    QAction *mActionDrawWatershed = nullptr;
    QAction *mActionEditWatershed = nullptr;
    QAction *mActionMoveOutletPoint = nullptr;

    ReosMapPolygon mTemporaryAutomaticWatershed;
    ReosMapPolyline mTemporaryAutomaticStreamLine;

    void showAutomaticDelineating( bool shown );
    void showManualDelineating( bool shown );

    void updateAutomaticTool();
    void updateBurningLines();

    ReosMapPolygon mTemporaryManualWatershed;
    ReosMapMarkerFilledCircle mTemporaryManualOutletPoint;

    ReosMapPolylineFormater burningLineFormater;

    void updateManualMapTool();
};

#endif // REOSDELINEATINGWATERSHEDWIDGET_H
