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
#include "reosactionwidget.h"


class ReosMapMarker;

namespace Ui
{
  class ReosHubEauWidget;
}

class ReosMap;
class ReosHubEauAccess;

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

  private:
    Ui::ReosHubEauWidget *ui;
    ReosMap *mMap = nullptr;
    ReosHubEauAccess *mServer = nullptr;
    std::vector < std::unique_ptr<ReosMapMarker>> mStationsMarker;
};

#endif // REOSHUBEAUWIDGET_H
