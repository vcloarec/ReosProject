/***************************************************************************
                      mainwindow.h
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QUndoGroup>

#include <qgsmeshdataprovider.h>


#include "../GIS/hdgismanager.h"

#include "../GIS/reosmap.h"
#include "../Reos/reosmodule.h"

#include "provider/meshdataprovider.h"
#include "tinEditorUi/reostineditorgraphic.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void showTinEditor(bool b)
    {
        if(b)
            tinEditor->showWidget();
        else
            tinEditor->hideWidget();
    }

    void tinEditorClosed()
    {
        actionTinEditor->setChecked(false);
    }

    void activeUndoStack(QUndoStack *undoStack)
    {
        if (!mUndoGroup->stacks().contains(undoStack))
        {
            mUndoGroup->addStack(undoStack);
        }
        mUndoGroup->setActiveStack(undoStack);
    }

private:
    Ui::MainWindow *ui;
    ReosTinEditorUi *tinEditor;
    QUndoGroup* mUndoGroup;

    QAction* actionTinEditor;
};

#endif // MAINWINDOW_H
