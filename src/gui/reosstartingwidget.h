/***************************************************************************
                      reosstartingwidget.h
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

#ifndef DEMARRAGE_H
#define DEMARRAGE_H

#include <QWidget>
#include <QDialog>
#include <QDebug>

#include "reosgui.h"
#include "reosmainwindow.h"
#include "ui_reosstartingwidget.h"

namespace Ui
{
  class ReosStartingWidget;
}

//! Popup widget oin the start of the aplication to purpose to the user to create a new project or opening an xisting one
class REOSGUI_EXPORT ReosStartingWidget : public QDialog
{
    Q_OBJECT
  public:
    explicit ReosStartingWidget( ReosMainWindow *parent = nullptr );
    ~ReosStartingWidget();

    void setBan( const QPixmap &image );

  private:
    Ui::ReosStartingWidget *ui;
    ReosMainWindow *mMainWindow = nullptr;

  private slots:
    void onNewProject();
    void onOpenProject();
};

#endif // DEMARRAGE_H
