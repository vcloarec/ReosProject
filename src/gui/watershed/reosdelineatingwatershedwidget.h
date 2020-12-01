/***************************************************************************
                      reosdelineatingwatershedfromdemwidget.h
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
#include "reosmaptool.h"
#include "reosmapitem.h"

class ReosWatershedDelineating;
class ReosMap;

namespace Ui
{
  class ReosDelineatingWatershedWidget;
}

class ReosDelineatingWatershedWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosDelineatingWatershedWidget(
      ReosWatershedDelineating *watershedDelineatingModule,
      ReosMap *map,
      QWidget *parent = nullptr );
    ~ReosDelineatingWatershedWidget();

  signals:
    void closed();

  protected:
    void closeEvent( QCloseEvent *event );

  private slots:
    void onDownstreamLineDrawn( const QPolygonF &downstreamLine );
    void onPredefinedExtentDrawn( const QRectF &extent );
    void onBurningLineDrawn( const QPolygonF &burningLine );
    void onBurningLineRemoved( ReosMapItem *item );
    void onDemComboboxChanged();
    void onDelineateAsked();
    void onValidateAsked();
    void onMethodChange();
    void storeGeometry();
    void restore();

  private:
    Ui::ReosDelineatingWatershedWidget *ui;
    ReosWatershedDelineating *mModule = nullptr;
    ReosMap *mMap = nullptr;

    QToolBar *mAutomaticToolBar = nullptr;

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
    std::vector<std::unique_ptr<ReosMapPolyline>> mBurningLines;

    ReosMapPolygon mTemporaryWatershed = nullptr;
    ReosMapPolyline mTemporaryStreamLine = nullptr;

    void updateTool();
};

#endif // REOSDELINEATINGWATERSHEDWIDGET_H
