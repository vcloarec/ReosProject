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

//#include <qgsmapcanvas.h>
//#include <qgsrubberband.h>

//#include "../Tools/raster/hdrasterdata.h"
//#include "../ReosProject/GIS/reosmap.h"
//#include "../ReosProject/GIS/hdgismanager.h"
//#include "../hydrologie/Watershed/hlgdelineatewatershed.h"
//#include "../hydrologie/Watershed/hlgwatershedmanager.h"
//#include "../hydrologie/Runoff/hlgrunoffmanager.h"
//#include "../hydrologie/InterfacePluies/hlgrainfallmanager.h"

//#include "../cmn/DEM/hddemmanager.h"


//#include "../cmn/Mainwindow/demarrage.h"
//#include "../ReosProject/Reos/reosmodule.h"
//#include "../ReosProject/Reos/reosmessagebox.h"
//#include "../Reos/reosversion.h"
//#include "../Reos/reosdocumentation.h"
//#include "../cmn/Noyau/apropos.h"
//#include "../cmn/classes_diverses/dialogchoixlangue.h"

class ReosModule;
class ReosMap;
class ReosGisEngine;

static const ReosVersion lekanVersion( "Lekan", 1, 0, 5 );

class LekanMainWindow : public ReosMainWindow
{
    Q_OBJECT

  public:
    explicit LekanMainWindow( QWidget *parent = nullptr );

    bool openProject() override;

//    bool openBackFile( QString filename );
//    void addDock();

//    QString getGISFileName();

  private slots:

    QByteArray encode() const override {return QByteArray();}
    bool decode( const QByteArray &byteArray ) override {}

  private:

    bool saveProject() override;
    void clearProject() override {}
    ReosVersion version() const {return lekanVersion;}
    QString projectFileFilter()  const override;

    QFileInfo gisFileInfo() const;

//    HdDEMManager *demManager = nullptr;
//    ReosGisManager *gisManager = nullptr;
//    ReosMap *map = nullptr;
//    HlgWatershedManager *watershedManager = nullptr;
//    HlgRunoffManager *runoffManager = nullptr;
//    HlgRainfallManager *rainfallManager = nullptr;
//    ReosMessageBox *messageBox = nullptr;
//    ReosDocumentation *reosDocumentation;

    ReosGisEngine *mGisEngine;
    ReosMap *mMap = nullptr;

    QDockWidget *mGisDock;
    QDockWidget *dockDEM;
    QDockWidget *dockWatershed;
    QDockWidget *mDockMessageBox;

    QToolBar *toolBarRainfallRunoffModel;
    QMenu *menuRainFallRunoffModel;
    QMenu *mMenuInterrogation;

    QActionGroup *mGroupActionEdit;
    QActionGroup *mGroupActionOption;
    QActionGroup *mGroupActionInterrogation;

    QAction *mActionNewVersionAvailable;
    QAction *mActionDocumentation;

    QList<QMenu *> specificMenus() override;
};

#endif // LEKANMAINWINDOW_H
