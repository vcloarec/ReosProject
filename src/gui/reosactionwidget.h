/***************************************************************************
  reosactionwidget.h - ReosActionWidget

 ---------------------
 begin                : 25.1.2021
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
#ifndef REOSACTIONWIDGET_H
#define REOSACTIONWIDGET_H

#include <QWidget>

class ReosMapTool;

class ReosActionWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosActionWidget( QWidget *parent = nullptr );
    void setAction( QAction *action );

  signals:
    void closed();
    void opened();

  protected:
    void closeEvent( QCloseEvent *event ) override;
    void restore();

  private:
    QAction *mAction;
    QList<ReosMapTool *> mMapTools;

    void storeGeometry();



};

#endif // REOSACTIONWIDGET_H
