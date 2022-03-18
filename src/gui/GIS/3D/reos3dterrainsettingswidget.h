/***************************************************************************
  reos3dterrainsettingswidget.h - Reos3DTerrainSettingsWidget

 ---------------------
 begin                : 15.3.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOS3DTERRAINSETTINGSWIDGET_H
#define REOS3DTERRAINSETTINGSWIDGET_H

#include <QWidget>;

class Reos3DTerrainSettings;

namespace Ui
{
  class Reos3DTerrainSettingsWidget;
}

class Reos3DTerrainSettingsWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit Reos3DTerrainSettingsWidget( QWidget *parent = nullptr );
    ~Reos3DTerrainSettingsWidget();

    void setTerrainSettings( const Reos3DTerrainSettings &settings );
    Reos3DTerrainSettings settings() const;

  signals:
    void terrainSettingsChanged();

  private:
    Ui::Reos3DTerrainSettingsWidget *ui;
};

#endif // REOS3DTERRAINSETTINGSWIDGET_H
