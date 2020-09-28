/***************************************************************************
                      reosmainwindow.h
                     --------------------------------------
Date                 : 07-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSMAINWINDOW_H
#define REOSMAINWINDOW_H

#include <QMainWindow>
#include <QActionGroup>
#include <QDir>
#include <QUndoStack>

#include <QVBoxLayout>
#include <QPixmap>

#include "reosversion.h"

class ReosModule;
class ReosDocumentation;
class ReosMessageBox;

static const QString webSite( "www.reos.site/projet-reos/" );

//! Mainwindow of application
class ReosMainWindow : public QMainWindow
{
    Q_OBJECT

  public:
    explicit ReosMainWindow( QWidget *parent = nullptr );
    bool open();

  protected:
    void closeEvent( QCloseEvent *event ) override;
    void keyPressEvent( QKeyEvent *event ) override;

    void init();
    void addActionsFile( const QList<QAction *> actions );
    void addActionEdit( const QList<QAction *> actions );
    void addActionOption( const QList<QAction *> actions );
    void addActionInterrogation( const QList<QAction *> actions );

    ReosModule *rootModule() const;

    QString currentProjectFilePath() const;
    QString currentProjectFileName() const;
    QString currentProjectBaseName() const;
    QString currentProjectPath() const;

  private slots:
    void newUndoCommand( QUndoCommand *command );

    bool save();
    bool saveAs();
    void newProject();

  private:
    virtual ReosVersion version() const {return ReosVersion();}
    virtual bool saveProject() = 0;
    virtual void clearProject() = 0;
    virtual bool openProject() = 0;
    virtual QByteArray encode() const = 0;
    virtual bool decode( const QByteArray &byteArray ) = 0;
    virtual QString projectFileFilter() const;

    void languageSelection();
    void about();
    void newVersionAvailable();

    virtual QList<QMenu *> specificMenus() {return QList<QMenu *>();}

    ReosModule *mRootModule = nullptr;

    ReosMessageBox *messageBox = nullptr;
    ReosDocumentation *mDocumentation;

    QDockWidget *mDockMessageBox;

    QMenu *mMenuFile;
    QMenu *mMenuEdit;
    QMenu *mMenuOption;
    QMenu *mMenuInterrogation;

    QActionGroup *mGroupActionFile;
    QActionGroup *mGroupActionEdit;
    QActionGroup *mGroupActionOption;
    QActionGroup *mGroupActionInterrogation;

    QAction *mActionNewProject;
    QAction *mActionOpenFile;
    QAction *mActionSaveFile;
    QAction *mActionSaveFileAs;

    QAction *mActionLanguageSelection;
    QAction *mActionAbout;
    QAction *mActionNewVersionAvailable;
    QAction *mActionDocumentation;

    QToolBar *mToolBarFile;
    QToolBar *mToolBarEdit;

    QFileInfo mCurrentProjectFileInfo;
    QUndoStack *mUndoStack;
};

#endif // REOSMAINWINDOW_H
