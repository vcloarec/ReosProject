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

#ifndef REOSDELINEATINGWATERSHEDFROMDEMWIDGET_H
#define REOSDELINEATINGWATERSHEDFROMDEMWIDGET_H

#include <QWidget>

#include "reoswatersheddelineating.h"
#include "reosmaptool.h"

class ReosWatershedDelineating;
class ReosMap;

namespace Ui
{
  class ReosDelineatingWatershedFromDemWidget;
}

class ReosDelineatingWatershedFromDemWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosDelineatingWatershedFromDemWidget(
      ReosWatershedDelineating *watershedDelineatingModule,
      ReosGisEngine *gisEngine,
      ReosMap *map,
      QWidget *parent = nullptr );
    ~ReosDelineatingWatershedFromDemWidget();

  private slots:
    void onDownstreamLineDrawn( const QPolygonF &downstreamLine );
    void onDemComboboxChanged();

  private:
    Ui::ReosDelineatingWatershedFromDemWidget *ui;
    ReosWatershedDelineating *mModule = nullptr;

    ReosMapToolDrawPolyline *mMapToolDrawDownStreamLine;
    QAction *mActionDrawDownstreamLine = nullptr;
    QAction *mActionDrawnPredefinedExtent = nullptr;

    void updateToolButton();
};

#endif // REOSDELINEATINGWATERSHEDFROMDEMWIDGET_H
