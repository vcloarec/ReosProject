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

#include "reosgui.h"

class ReosMapTool;
class QStackedWidget;

class REOSGUI_EXPORT ReosActionWidget : public QWidget
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

    //! Reemplement this method allow to the dervied class to do what is needed when the widget is closed, default implementation does nothing
    virtual void closingWidget() {}

    QList<ReosMapTool *> mMapTools;

  private:
    QAction *mAction;
    void storeGeometry();
};

class ReosStackedPageWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosStackedPageWidget( QWidget *parent = nullptr ): QWidget( parent )
    {}

    virtual void showBackButton() {};

  signals:
    void backToPreviousPage();
    void addOtherPage( ReosStackedPageWidget *page );
};

class ReosActionStackedWidget: public ReosActionWidget
{
    Q_OBJECT
  public:
    explicit ReosActionStackedWidget( QWidget *parent = nullptr );

  public slots:
    void addPage( ReosStackedPageWidget *widget );
    void backToPrevious();
    void backToFirstPage();

  private:
    QStackedWidget *mStackedWidget;
};

#endif // REOSACTIONWIDGET_H
