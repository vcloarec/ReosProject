/***************************************************************************
  reosdockwidget.h - ReosDockWidget

 ---------------------
 begin                : 29.12.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSDOCKWIDGET_H
#define REOSDOCKWIDGET_H

#include <QDockWidget>

class ReosDockWidget : public QDockWidget
{
    Q_OBJECT
  public:
    ReosDockWidget( const QString &title, QWidget *parent = nullptr );

  signals:
    void shown();
    void closed();

  protected:
    void showEvent( QShowEvent *e );
    void closeEvent( QCloseEvent *e );
};

#endif // REOSDOCKWIDGET_H
