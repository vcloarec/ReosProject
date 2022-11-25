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

static const ReosVersion lekanVersion( "Lekan", 2, 2, 95 );

class LekanMainWindow : public ReosMainWindow
{
    Q_OBJECT

  public:
    explicit LekanMainWindow( QWidget *parent = nullptr );
    bool openProject() override;

  private slots:
    QByteArray encode() const override {return QByteArray();}
    bool decode( const QByteArray &byteArray ) override { return false; }

    void onTimeWindowChanged();
    void onMapTimeStepChanged();

  private:
    bool saveProject() override;
    void clearProject() override;
    void checkExtraProjectToSave() override;
    ReosVersion version() const override {return lekanVersion;}
    QString projectFileFilter()  const override;
    QString projectFileSuffix() const override;

    QFileInfo gisFileInfo() const;

    ReosGisEngine *mGisEngine = nullptr;
    ReosMap *mMap = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    ReosWatershedWidget *mWatershedWidget = nullptr;
    ReosHydraulicNetwork *mHydraulicNetwork = nullptr;
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
