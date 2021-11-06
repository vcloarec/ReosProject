/***************************************************************************
  reoshubeauwidget.h - ReosHubEauWidget

 ---------------------
 begin                : 1.11.2021
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
#ifndef REOSHUBEAUWIDGET_H
#define REOSHUBEAUWIDGET_H

#include <memory>
#include <QPointer>

#include "reosactionwidget.h"
#include "reoshubeauserver.h"
#include "reosmapitem.h"
#include "reosmap.h"
#include "reoshydrograph.h"

class ReosMapMarker;

namespace Ui
{
  class ReosHubEauWidget;
}

class ReosHubEauServer;
class ReosMapToolSelectMapItem;
class ReosHubEauStationMarker;
class ReosHydrograph;
class ReosPlotTimeSerieVariableStep;

class ReosHubEauStationMarker: public ReosMapMarker
{
  public:
    ReosHubEauStationMarker( ReosMap *map, const QPointF &point );
    int stationIndex;
};

class ReosHubEauWidget : public ReosActionWidget
{
    Q_OBJECT
  public:
    explicit ReosHubEauWidget( ReosMap *map, QWidget *parent = nullptr );
    ~ReosHubEauWidget();

  private slots:
    void onMapExtentChanged();
    void onStationUpdated();
    void onClosed();
    void onSelectStation( ReosMapItem *item, const QPointF & );
    void onHydrographUpdated();

  private:
    Ui::ReosHubEauWidget *ui;
    ReosMap *mMap = nullptr;
    ReosHubEauServer *mServer = nullptr;
    QList<ReosHubEauStation> mStations;
    std::vector < std::unique_ptr<ReosHubEauStationMarker>> mStationsMarker;
    ReosMapToolSelectMapItem *mSelectStation = nullptr;
    QPointer<ReosHydrograph> mCurrentHydrograph = nullptr;
    ReosPlotTimeSerieVariableStep *mHydrographPlot = nullptr;
    ReosHubEauStationMarker *mCurrentMarker = nullptr;

    void populateMeta( const QVariantMap &meta );
};

#endif // REOSHUBEAUWIDGET_H
