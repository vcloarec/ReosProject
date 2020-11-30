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
      ReosGisEngine *gisEngine,
      ReosMap *map,
      QWidget *parent = nullptr );
    ~ReosDelineatingWatershedWidget();

  private slots:
    void onDownstreamLineDrawn( const QPolygonF &downstreamLine );
    void onPredefinedExtentDrawn( const QRectF &extent );
    void onDemComboboxChanged();
    void onDelineateAsked();
    void onValidateAsked();

    void onMethodChange();

  private:
    Ui::ReosDelineatingWatershedWidget *ui;
    ReosWatershedDelineating *mModule = nullptr;
    ReosMap *mMap = nullptr;

    QToolBar *mAutomaticToolBar = nullptr;

    ReosMapToolDrawPolyline *mMapToolDrawDownStreamLine = nullptr;
    ReosMapToolDrawExtent *mMapToolDrawPredefinedExtent = nullptr;
    QAction *mActionDrawDownstreamLine = nullptr;
    QAction *mActionDrawPredefinedExtent = nullptr;

    ReosMapPolyline mDownstreamLine;
    ReosMapPolygon mWatershedExtent;

    ReosMapPolygon mTemporaryWatershed = nullptr;
    ReosMapPolyline mTemporaryStreamLine = nullptr;

    void updateTool();
};

#endif // REOSDELINEATINGWATERSHEDWIDGET_H
