/***************************************************************************
                      lekanmainwindow.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef LEKANMAINWINDOW_H
#define LEKANMAINWINDOW_H

#include <QMainWindow>
#include <QActionGroup>
#include <QUndoStack>

#include <QVBoxLayout>
#include <QPixmap>

#include "reosversion.h"
#include "reosversionmessagebox.h"
#include "reosmainwindow.h"
#include "reoswatershedtree.h"

class ReosModule;
class ReosMap;
class ReosGisEngine;
class ReosWatershedModule;
class ReosHydraulicNetwork;
class ReosDelineatingWatershedWidget;
class ReosRainfallManager;
class ReosRunoffManager;
class ReosWatershedDockWidget;
class ReosHydraulicNetworkDockWidget;
class ReosWatershedWidget;
class ReosHydraulicNetworkWidget;
class ReosCoreModule;

class LekanMainWindow : public ReosMainWindow
{
    Q_OBJECT

  public:
    explicit LekanMainWindow( ReosCoreModule *core, QWidget *parent = nullptr );
    bool openProject() override;

    QStringList recentProjectPathes() const;

  private slots:

    void onMapTimeStepChanged();

  private:
    bool saveProject() override;
    void clearProject() override;
    void checkExtraProjectToSave() override;
    ReosVersion version() const override {return ReosVersion::currentApplicationVersion();}
    QString projectFileFilter()  const override;
    QString projectFileSuffix() const override;

    QFileInfo gisFileInfo() const;

    void storeProjectPath( const QString &path );
    ReosCoreModule *mCore = nullptr;
    ReosMap *mMap = nullptr;
    ReosWatershedWidget *mWatershedWidget = nullptr;
    ReosHydraulicNetworkWidget *mHydraulicNetworkWidget = nullptr;

    QDockWidget *mGisDock = nullptr;
    ReosWatershedDockWidget *mDockWatershed = nullptr;
    ReosHydraulicNetworkDockWidget *mDockHydraulicNetwork = nullptr;
    QDockWidget *mDockMessageBox = nullptr;

    QList<QMenu *> specificMenus() override;

    QAction *mActionRainfallManager = nullptr;
    QAction *mActionRunoffManager = nullptr;
    ReosRainfallManager *mRainFallManagerWidget = nullptr;
    ReosRunoffManager *mRunoffManagerWidget = nullptr;

    bool mIsRainfallDirty = false;
    bool mIsRunoffDirty = false;
};

#endif // LEKANMAINWINDOW_H
