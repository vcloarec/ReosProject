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

#include "ui_reosstartingwidget.h"

namespace Ui
{
  class ReosStartingWidget;
}

class ReosStartingWidget : public QDialog
{
    Q_OBJECT
  public:
    explicit ReosStartingWidget( QWidget *parent = nullptr );
    ~ReosStartingWidget();

    bool openProjectChoice() {return mOpenProjectChoice;}

    void setBan( const QPixmap &image );

  private:
    Ui::ReosStartingWidget *ui;
    bool mOpenProjectChoice;

  private slots:
    void onNewProject();
    void onOpenProject();
};

#endif // DEMARRAGE_H
