/***************************************************************************
  reosdssgriddedrainfallselector.h - ReosDssGriddedRainfallSelector

 ---------------------
 begin                : 13.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSDSSGRIDDEDRAINFALLSELECTOR_H
#define REOSDSSGRIDDEDRAINFALLSELECTOR_H

#include <QWidget>
#include "reosgriddedrainfallselectorwidget.h"

class ReosDssProviderGriddedRainfall;

namespace Ui
{
  class ReosDssGriddedRainfallSelector;
}

class ReosDssGriddedRainfallSelector : public ReosGriddedRainDataProviderSelectorWidget
{
    Q_OBJECT

  public:
    explicit ReosDssGriddedRainfallSelector( QWidget *parent = nullptr );
    ~ReosDssGriddedRainfallSelector();

    ReosGriddedRainfall *createData( QObject *parent = nullptr )  const override;

    virtual QVariantMap selectedMetadata() const override;

    virtual ReosGriddedRainfallProvider::FileDetails setSource( const QString &source, ReosModule::Message &message ) override;

  private:
    void onComboChanged();

  private:
    Ui::ReosDssGriddedRainfallSelector *ui;
    QString mSource;
    std::unique_ptr<ReosDssProviderGriddedRainfall> mProvider;
};


class ReosDssGuiFactory : public ReosDataProviderGuiFactory
{
  public:
    GuiCapabilities capabilities() const override;
    QString key() const override;
    ReosDssGriddedRainfallSelector *createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent = nullptr ) const override;
    ReosDataProviderSettingsWidget *createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr ) const override;
    QString dataType() const override;

    QString displayText() const override;
};

#endif // REOSDSSGRIDDEDRAINFALLSELECTOR_H
