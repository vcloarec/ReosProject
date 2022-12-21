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
class ReosActionStackedWidget;

class REOSGUI_EXPORT ReosActionWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosActionWidget( QWidget *parent = nullptr );
    void setAction( QAction *action );

    void showWidgetAction();

  signals:
    void closed();
    void opened();

  protected:
    void closeEvent( QCloseEvent *event ) override;
    void restore();

    //! Reimplement this method allow to the dervied class to do what is needed when the widget is closed, default implementation does nothing
    virtual void closingWidget() {}

    QList<ReosMapTool *> mMapTools;

  private:
    void storeGeometry();
    QAction *mAction = nullptr;
};

class ReosStackedPageWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosStackedPageWidget( QWidget *parent = nullptr ): QWidget( parent )
    {}

    virtual void showBackButton() {};
    virtual void hideBackButton() {};

    virtual bool canBeDetached() const {return false;}
    virtual void hideDetachButton() {};

    void setStackedWidget( ReosActionStackedWidget *newStackedWidget );

    void setAction( QAction *newAction );

  public slots:
    void addOtherPage( ReosStackedPageWidget *page );

    /**
     * Creates a new floatable stacked widget and set this page at first one.
     */
    void detach( QWidget *newPArent );

    void showPage();

  signals:
    void askForShow();

  signals:
    void backToPreviousPage();

  private:
    ReosActionStackedWidget *mStackedWidget = nullptr;
    QAction *mAction = nullptr;
};

class ReosActionStackedWidget: public ReosActionWidget
{
    Q_OBJECT
  public:
    explicit ReosActionStackedWidget( QWidget *parent = nullptr );

    int indexOf( ReosStackedPageWidget *page );
    void removePage( ReosStackedPageWidget *page );
    void setCurrentPage( ReosStackedPageWidget *page );

    void detachePage( ReosStackedPageWidget *page );

  public slots:
    void addPage( ReosStackedPageWidget *widget, int index );
    void backToPrevious();
    void backToFirstPage();

  private slots:
    void purgeDetachedPages();

  private:
    QStackedWidget *mStackedWidget;
    QList < QPointer<ReosStackedPageWidget>> mDetachedPages;
};

#endif // REOSACTIONWIDGET_H
