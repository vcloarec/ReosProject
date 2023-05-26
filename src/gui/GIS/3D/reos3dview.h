/***************************************************************************
  reos3dview.h - Reos3dView

 ---------------------
 begin                : 3.3.2022
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
#ifndef REOS3DVIEW_H
#define REOS3DVIEW_H

#include <QPointer>

#include "reosactionwidget.h"

class Qgs3DMapCanvas;
class QgsMesh3DSymbol;
class ReosMesh;
class Reos3DTerrainSettingsWidget;
class ReosLightWidget;
class ReosVerticalExaggerationWidget;
class ReosEncodedElement;
class Reos3DMapSettings;
class Reos3DTerrainSettings;
class ReosGuiContext;

namespace Ui
{
  class Reos3dView;
}

class Reos3dView : public ReosActionWidget
{
    Q_OBJECT
  public:
    explicit Reos3dView( ReosMesh *meshTerrain, const ReosGuiContext &guiContext );
    ~Reos3dView();

    void addMesh( ReosMesh *mesh );

    void setMapSettings( const Reos3DMapSettings &map3DSettings, bool updateView = true );
    Reos3DMapSettings map3DSettings() const;

    void setTerrainSettings( const Reos3DTerrainSettings &settings, bool updateView = true );
    Reos3DTerrainSettings terrainSettings() const;

  signals:
    void mapSettingsChanged();
    void terrainSettingsChanged();

  private slots:
    void onExaggerationChange( double value );
    void onLightChange();
    void onTerrainSettingsChanged();

  private:
    Ui::Reos3dView *ui;
    QPointer<ReosMesh> mMeshTerrain;
    Qgs3DMapCanvas *mCanvas = nullptr;
    QAction *mActionZoomExtent = nullptr;
    ReosLightWidget *mLightWidget = nullptr;
    ReosVerticalExaggerationWidget *mExagerationWidget = nullptr;

    Reos3DTerrainSettingsWidget *mTerrainSettingsWidget = nullptr;

    QList<QPointer<ReosMesh>> mMeshes;
};

#endif // REOS3DVIEW_H
