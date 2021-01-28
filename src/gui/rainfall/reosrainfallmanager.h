/***************************************************************************
  reosrainfallmanager.h - ReosRainfallManager

 ---------------------
 begin                : 24.1.2021
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
#ifndef REOSRAINFALLMANAGER_H
#define REOSRAINFALLMANAGER_H

#include <QWidget>

#include "reosactionwidget.h"


namespace Ui
{
  class ReosRainfallManager;
}

class QAction;
class ReosRainfallModel;
class ReosRainfallItem;
class ReosFormWidget;

class ReosRainfallManager : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosRainfallManager( ReosRainfallModel *rainfallmodel, QWidget *parent = nullptr );
    ~ReosRainfallManager();

  private slots:
    void onAddRootZone();
    void onAddZoneToZone();
    void onAddStation();
    void onAddGaugedRainfall();
    void onCurrentTreeIndexChanged();
    void onTreeViewContextMenu( const QPoint &pos );

  private:
    Ui::ReosRainfallManager *ui;
    ReosRainfallModel *mModel = nullptr;
    QAction *mAction;

    QAction *mActionOpenRainfallDataFile = nullptr;
    QAction *mActionAddRootZone = nullptr;
    QAction *mActionAddZoneToZone = nullptr;
    QAction *mActionAddStation = nullptr;
    QAction *mActionAddGaugedRainfall = nullptr;

    ReosFormWidget *mCurrentForm = nullptr;

    void selectItem( ReosRainfallItem *item );
};

#endif // REOSRAINFALLMANAGER_H
