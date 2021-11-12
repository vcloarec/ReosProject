/***************************************************************************
  reosdelftfewssettingswidget.h - ReosDelftFewsSettingsWidget

 ---------------------
 begin                : 12.11.2021
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
#ifndef REOSDELFTFEWSSETTINGSWIDGET_H
#define REOSDELFTFEWSSETTINGSWIDGET_H

#include "reosdataprovidergui.h"

class ReosDelftFewsXMLProvider;

namespace Ui
{
  class ReosDelftFewsSettingsWidget;
}

class ReosDelftFewsSettingsWidget : public ReosDataProviderSettingsWidget
{
    Q_OBJECT
  public:
    explicit ReosDelftFewsSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr );
    ~ReosDelftFewsSettingsWidget();

  private:
    Ui::ReosDelftFewsSettingsWidget *ui;
};

#endif // REOSDELFTFEWSSETTINGSWIDGET_H
