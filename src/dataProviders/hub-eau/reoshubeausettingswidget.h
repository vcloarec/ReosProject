/***************************************************************************
  reoshubeausettingswidget.h - ReosHubEauSettingsWidget

 ---------------------
 begin                : 9.11.2021
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
#ifndef REOSHUBEAUSETTINGSWIDGET_H
#define REOSHUBEAUSETTINGSWIDGET_H

#include <QWidget>

#include "reosdataprovidergui.h"


class ReosDataProvider;
class ReosHubEauHydrographProvider;

namespace Ui
{
  class ReosHubEauSettingsWidget;
}

class ReosHubEauSettingsWidget : public ReosDataProviderSettingsWidget
{
    Q_OBJECT

  public:
    explicit ReosHubEauSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr );
    ~ReosHubEauSettingsWidget();

  private slots:
    void onReload();
    void onErrorOccured();

  private:
    Ui::ReosHubEauSettingsWidget *ui;
    ReosHubEauHydrographProvider *mProvider = nullptr;
    void populateDescription();
    void enableLoadButton();
};

#endif // REOSHUBEAUSETTINGSWIDGET_H
