/***************************************************************************
                      lekanmainwindow.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
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

#include <QVBoxLayout>
#include <QPixmap>
#include <qgsmapcanvas.h>
#include <qgsrubberband.h>

#include "../Tools/raster/hdrasterdata.h"
#include "../ReosProject/GIS/reosmap.h"
#include "../ReosProject/GIS/hdgismanager.h"
#include "../hydrologie/Watershed/hlgdelineatewatershed.h"
#include "../hydrologie/Watershed/hlgwatershedmanager.h"
#include "../hydrologie/Runoff/hlgrunoffmanager.h"
#include "../hydrologie/InterfacePluies/hlgrainfallmanager.h"

#include "../cmn/DEM/hddemmanager.h"


#include "../cmn/Mainwindow/demarrage.h"
#include "../ReosProject/Reos/reosmodule.h"
#include "../ReosProject/Reos/reosmessagebox.h"
#include "../Reos/reosversion.h"
#include "../Reos/reosdocumentation.h"
#include "../cmn/Noyau/apropos.h"
#include "../cmn/classes_diverses/dialogchoixlangue.h"


static const ReosVersion lekanVersion( "Lekan", 1, 0, 5 );
static const QString webSite( "www.reos.site/projet-reos/" );

namespace Ui
{
  class LekanMainWindow;
}

class LekanMainWindow : public QMainWindow
{
    Q_OBJECT

  public:
    explicit LekanMainWindow( QWidget *parent = nullptr );
    ~LekanMainWindow() override;

    bool open();
    bool openProject( QString file );
    bool openBackFile( QString filename );
    void addDock();


    QString getGISFileName();

  private slots:
    void newUndoCommand( QUndoCommand *command );

    bool saveProject();
    bool saveProjectAs();
    void newProject();


    QByteArray encode() const;
    bool decode( const QByteArray &byteArray );
    void languageSelection();
    void aPropos();
    void newVersionAvailable()
    {
      new ReosVersionMessageBox( this, lekanVersion, false );
    }

  private:
    Ui::LekanMainWindow *ui;
    ReosModule *rootReosModule;

    HdDEMManager *demManager = nullptr;
    ReosGisManager *gisManager = nullptr;
    ReosMap *map = nullptr;
    HlgWatershedManager *watershedManager = nullptr;
    HlgRunoffManager *runoffManager = nullptr;
    HlgRainfallManager *rainfallManager = nullptr;
    ReosMessageBox *messageBox = nullptr;
    ReosDocumentation *reosDocumentation;

    QDockWidget *dockSIG;
    QDockWidget *dockDEM;
    QDockWidget *dockWatershed;
    QDockWidget *dockMessageBox;

    QActionGroup *groupActionFile;
    QAction *actionNewProject;
    QAction *actionOpenFile;
    QAction *actionSaveFile;
    QAction *actionSaveFileAs;



    QToolBar *toolBarFile;
    QToolBar *toolBarEdit;
    QToolBar *toolBarRainfallRunoffModel;
    QMenu *menuFile;
    QMenu *menuEdit;
    QMenu *menuRainFallRunoffModel;
    QMenu *menuOption;
    QMenu *menuInterrogation;

    QString fileNameCurrentProject = "";

    QActionGroup *groupActionEdit;
    QActionGroup *groupActionOption;
    QActionGroup *groupActionInterrogation;

    QAction *actionLanguageSelection;
    QAction *actionAPropos;
    QAction *actionNewVersionAvailable;
    QAction *actionDocumentation;

    QUndoStack *undoStack;

    void clearProject();




    // QWidget interface
  protected:
    void closeEvent( QCloseEvent *event ) override;
    void keyPressEvent( QKeyEvent *event ) override;
};

#endif // LEKANMAINWINDOW_H
