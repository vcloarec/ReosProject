/***************************************************************************
  reosactionwidget.cpp - ReosActionWidget

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
#include "reosactionwidget.h"
#include <QAction>
#include <QCloseEvent>
#include <QStackedWidget>
#include "reossettings.h"
#include "reosmaptool.h"
#include "QLayout"
#include <QDebug>

ReosActionWidget::ReosActionWidget( QWidget *parent )
  : QWidget( parent )
{
  connect( this, &QObject::destroyed, this, &ReosActionWidget::storeGeometry );
}

void ReosActionWidget::setAction( QAction *action )
{
  mAction = action;
  action->setCheckable( true );
  action->setChecked( isVisible() );
  connect( action, &QAction::triggered, this, [this]
  {
    if ( mAction->isChecked() )
    {
      showWidgetAction();
    }
    else
    {
      storeGeometry();
      close();
    }
  } );
}

void ReosActionWidget::showWidgetAction()
{
  restore();
  show();
  emit opened();
}

void ReosActionWidget::closeEvent( QCloseEvent *event )
{
  storeGeometry();
  if ( mAction )
    mAction->setChecked( false );
  setVisible( false );
  for ( ReosMapTool *tool : std::as_const( mMapTools ) )
  {
    if ( tool->isCurrentToolInMap() )
    {
      tool->quitMap();
      break;
    }
  }
  closingWidget();
  emit closed();
  event->accept();
}

void ReosActionWidget::storeGeometry()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "Windows/%1/Geometry" ).arg( objectName() ), saveGeometry() );
}

void ReosActionWidget::restore()
{
  ReosSettings settings;
  restoreGeometry( settings.value( QStringLiteral( "Windows/%1/Geometry" ).arg( objectName() ) ).toByteArray() );
}

ReosActionStackedWidget::ReosActionStackedWidget( QWidget *parent ): ReosActionWidget( parent )
{
  setLayout( new QHBoxLayout );
  mStackedWidget = new QStackedWidget( this );
  layout()->addWidget( mStackedWidget );

  connect( this, &ReosActionWidget::closed, this, &ReosActionStackedWidget::backToFirstPage );
}

int ReosActionStackedWidget::indexOf( ReosStackedPageWidget *page )
{
  return mStackedWidget->indexOf( page );
}

void ReosActionStackedWidget::removePage( ReosStackedPageWidget *page )
{
  mStackedWidget->removeWidget( page );
  page->setParent( nullptr );
  page->setStackedWidget( nullptr );
}

void ReosActionStackedWidget::setCurrentPage( ReosStackedPageWidget *page )
{
  mStackedWidget->setCurrentWidget( page );
}

void ReosActionStackedWidget::detachePage( ReosStackedPageWidget *page )
{
  mDetachedPages.append( page );
  removePage( page );
}

void ReosActionStackedWidget::addPage( ReosStackedPageWidget *widget, int index )
{
  for ( ReosStackedPageWidget *detachedPage : std::as_const( mDetachedPages ) )
  {
    if ( detachedPage->objectName() == widget->objectName() )
    {
      widget->deleteLater();
      detachedPage->showPage();
      return;
    }
  }

  if ( index >= 0 )
  {
    //remove all page with index greater or equal with coming index
    for ( int i = mStackedWidget->count() - 1; i >= index; --i )
    {
      QWidget *w = mStackedWidget->widget( i );
      mStackedWidget->removeWidget( w );
      w->deleteLater();
    }
  }

  if ( mStackedWidget->count() > 0 )
    widget->showBackButton();

  mStackedWidget->addWidget( widget );
  mStackedWidget->setCurrentWidget( widget );
  connect( widget, &ReosStackedPageWidget::backToPreviousPage, this, &ReosActionStackedWidget::backToPrevious );
  widget->setStackedWidget( this );
}

void ReosActionStackedWidget::backToPrevious()
{
  if ( mStackedWidget->count() < 2 )
    close();

  int currentIndex = mStackedWidget->currentIndex();
  QWidget *currentWidget = mStackedWidget->currentWidget();
  mStackedWidget->setCurrentIndex( currentIndex - 1 );
  mStackedWidget->removeWidget( currentWidget );
  delete currentWidget;
}

void ReosActionStackedWidget::backToFirstPage()
{
  while ( mStackedWidget->count() > 1 )
    backToPrevious();
}

void ReosStackedPageWidget::addOtherPage( ReosStackedPageWidget *page )
{
  if ( mStackedWidget )
  {
    int index = mStackedWidget->indexOf( this );
    mStackedWidget->addPage( page, index + 1 );
  }
}

void ReosStackedPageWidget::detach( QWidget *newPArent )
{
  mStackedWidget->detachePage( this );
  ReosActionStackedWidget *newStacked = new ReosActionStackedWidget( newPArent );
  newStacked->setWindowFlag( Qt::Dialog );
  newStacked->addPage( this, 0 );
  newStacked->show();
  newStacked->setAction( mAction );
  hideBackButton();
  hideDetachButton();
}

void ReosStackedPageWidget::showPage()
{
  mStackedWidget->setCurrentPage( this );
  mStackedWidget->show();
  raise();
}

void ReosStackedPageWidget::setAction( QAction *newAction )
{
  mAction = newAction;
}

void ReosStackedPageWidget::setStackedWidget( ReosActionStackedWidget *newStackedWidget )
{
  mStackedWidget = newStackedWidget;
}
