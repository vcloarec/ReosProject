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
#include <QAbstractListModel>

#include <QVBoxLayout>
#include <QPixmap>
#include <memory>

#include "reosgui.h"
#include "reosversion.h"
#include "reosmodule.h"

class ReosDocumentation;
class ReosMessageBox;

static const QString webSite( "www.reos.site/reos-project/" );

//! Mainwindow of application
class REOSGUI_EXPORT ReosMainWindow : public QMainWindow
{
    Q_OBJECT

  public:
    explicit ReosMainWindow( ReosModule *rootModule, QWidget *parent = nullptr );
    ~ReosMainWindow();
    bool openFile();
    bool openFileWithPath( const QString &filePath );

    void setRecentProjects( const QStringList &recentProjects );

    ReosModule *rootModule() const;

public slots:
    void newProject();

  protected:

    ReosModule *guiRootModule() const;
    void closeEvent( QCloseEvent *event ) override;

    void init();
    void addActionsFile( const QList<QAction *> &actions );
    void addActionEdit( const QList<QAction *> &actions );
    void addActionOption( const QList<QAction *> &actions );
    void addActionInterrogation( const QList<QAction *> &actions );

    //! Returns the complete project file path, with path to the folder and file name
    QString currentProjectFilePath() const;

    //! Returns the project file name, withour path
    QString currentProjectFileName() const;

    //! Returns the project base file name, withour path and extension
    QString currentProjectBaseName() const;

    //! Returns the project file's path. This doesn't include the file name.
    QString currentProjectPath() const;

    QVariantList mCriticalInfo;

  private slots:
    bool save();
    bool saveAs();

    void onRemoteInformation( const QVariantMap &information );

  private:
    virtual ReosVersion version() const {return ReosVersion();}
    virtual bool saveProject() = 0;
    virtual void clearProject() = 0;
    virtual bool openProject() = 0;
    virtual void checkExtraProjectToSave() = 0;
    virtual QString projectFileFilter() const;
    virtual QString projectFileSuffix() const;
    virtual void onCriticalInfo() {};

    void languageSelection();
    void about();
    void newVersionAvailable();

    virtual QList<QMenu *> specificMenus() {return QList<QMenu *>();}

    bool mProjectIsDirty = false;

    ReosModule *mRootModule = nullptr;
    ReosModule *mGuiRootModule = nullptr;
    ReosMessageBox *messageBox = nullptr;

    QDockWidget *mDockMessageBox = nullptr;

    QMenu *mMenuFile = nullptr;
    QMenu *mMenuEdit = nullptr;

    QMenu *mMenuOption = nullptr;
    QMenu *mMenuInterrogation = nullptr;

    QActionGroup *mGroupActionFile = nullptr;
    QActionGroup *mGroupActionEdit = nullptr;
    QActionGroup *mGroupActionOption = nullptr;
    QActionGroup *mGroupActionInterrogation = nullptr;

    QAction *mActionNewProject = nullptr;
    QAction *mActionOpenFile = nullptr;
    QMenu *mMenuRecentProjects = nullptr;
    QAction *mActionSaveFile = nullptr;
    QAction *mActionSaveFileAs = nullptr;

    QAction *mActionLanguageSelection = nullptr;
    QAction *mActionAbout;
    QAction *mActionNewVersionAvailable = nullptr;
    QAction *mActionDocumentation = nullptr;
    QAction *mActionHowToSupport = nullptr;

    QToolBar *mToolBarFile = nullptr;
    QToolBar *mToolBarEdit = nullptr;

    QFileInfo mCurrentProjectFileInfo;
    QUndoStack *mUndoStack = nullptr;

    QString mDocumentationUrl;
    QString mHowToSupportUrl;
    QString mFinancialContribution;
};


class ReosRecentProjectModel : public QAbstractListModel
{
    Q_OBJECT
  public:
    ReosRecentProjectModel( QObject *parent = nullptr );

    int rowCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    void setPathes( const QStringList &newPathes );

    const QString path( const QModelIndex &index );

  private:
    QStringList mPathes;
};

#endif // REOSMAINWINDOW_H
