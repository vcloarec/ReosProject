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
  if ( !isHidden() )
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
  QByteArray encodedGeom = saveGeometry();
  settings.setValue( QStringLiteral( "Windows/%1/Geometry" ).arg( objectName() ), encodedGeom );
}

void ReosActionWidget::restore()
{
  ReosSettings settings;
  QByteArray encodedGeom = settings.value( QStringLiteral( "Windows/%1/Geometry" ).arg( objectName() ) ).toByteArray();
  restoreGeometry( encodedGeom );
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
  connect( page, &QObject::destroyed, this, &ReosActionStackedWidget::purgeDetachedPages );
}

void ReosActionStackedWidget::reattachPage( ReosStackedPageWidget *page, int index )
{
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
    page->showBackButton();

  mStackedWidget->addWidget( page );
  mStackedWidget->setCurrentWidget( page );
  page->setStackedWidget( this );
}

void ReosActionStackedWidget::addPage( ReosStackedPageWidget *widget, int index, bool show )
{
  for ( ReosStackedPageWidget *detachePage : std::as_const( mDetachedPages ) )
  {
    if ( detachePage && detachePage->objectName() == widget->objectName() )
    {
      widget->deleteLater();
      if ( show )
        detachePage->showPage();
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

  QWidget *oriParent = widget->parentWidget();

  mStackedWidget->addWidget( widget );
  mStackedWidget->setCurrentWidget( widget );
  connect( widget, &ReosStackedPageWidget::backToPreviousPage, this, &ReosActionStackedWidget::backToPrevious );
  widget->setStackedWidget( this );
  if ( !widget->isDetached() )
    widget->restoreDetach( oriParent );
  if ( show )
    widget->showPage();
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

void ReosActionStackedWidget::purgeDetachedPages()
{
  int i = 0;
  while ( i < mDetachedPages.count() )
  {
    ReosStackedPageWidget *detachePage = mDetachedPages.at( i );
    if ( !detachePage )
      mDetachedPages.removeAt( i );
    else
      ++i;
  }
}

void ReosStackedPageWidget::addOtherPage( ReosStackedPageWidget *page, bool show )
{
  if ( mStackedWidget )
  {
    int index = mStackedWidget->indexOf( this );
    mStackedWidget->addPage( page, index + 1, show );
  }
}

ReosActionStackedWidget *ReosStackedPageWidget::detach( QWidget *newParent )
{
  mIsDetached = true;
  mOriginalStackedWidget = mStackedWidget;
  mOriginalIndex = mStackedWidget->indexOf( this );
  mStackedWidget->detachePage( this );
  ReosActionStackedWidget *newStacked = new ReosActionStackedWidget( newParent );
  newStacked->setAttribute( Qt::WA_DeleteOnClose );
  newStacked->setWindowFlag( Qt::Dialog );
  newStacked->addPage( this, 0, true );
  newStacked->show();
  newStacked->setAction( mAction );
  mStackedWidget = newStacked;
  hideBackButton();
  switchDetachButton();

  ReosSettings settings;
  settings.setValue( QStringLiteral( "Windows/%1/detached" ).arg( objectName() ), true );
  QByteArray saveGeometry = settings.value( QStringLiteral( "Windows/%1/geometry" ).arg( objectName() ) ).toByteArray();
  newStacked->restoreGeometry( saveGeometry );
  connect( newStacked, &ReosActionStackedWidget::closed, this, &ReosStackedPageWidget::undetached );

  emit detached();
  return newStacked;
}

void ReosStackedPageWidget::reattach()
{
  if ( !mIsDetached || ! mOriginalStackedWidget )
    return;

  ReosSettings settings;
  settings.setValue( QStringLiteral( "Windows/%1/geometry" ).arg( objectName() ), mStackedWidget->saveGeometry() );

  mStackedWidget->deleteLater();
  mStackedWidget->detachePage( this );
  mOriginalStackedWidget->reattachPage( this, mOriginalIndex );
  mStackedWidget = mOriginalStackedWidget;
  mIsDetached = false;
  settings.setValue( QStringLiteral( "Windows/%1/detached" ).arg( objectName() ), false );
  switchDetachButton();
  mStackedWidget->show();
  emit undetached();
}

ReosActionStackedWidget *ReosStackedPageWidget::restoreDetach( QWidget *newParent )
{
  ReosSettings settings;
  bool shouldBeDetached = settings.value( QStringLiteral( "Windows/%1/detached" ).arg( objectName() ), false ).toBool();
  if ( shouldBeDetached )
    return detach( newParent );

  return nullptr;
}

void ReosStackedPageWidget::showPage()
{
  mStackedWidget->setCurrentPage( this );
  mStackedWidget->show();
  raise();
}

bool ReosStackedPageWidget::isDetached() const
{
  return mIsDetached;
}

QAction *ReosStackedPageWidget::action() const
{
  return mAction;
}

void ReosStackedPageWidget::setAction( QAction *newAction )
{
  mAction = newAction;
}

ReosStackedPageWidget::ReosStackedPageWidget( QWidget *parent ): QWidget( parent )
{}

ReosStackedPageWidget::~ReosStackedPageWidget()
{
  if ( !mAction.isNull() )
    mAction->setChecked( false );

  if ( mIsDetached && mStackedWidget )
  {
    ReosSettings settings;
    settings.setValue( QStringLiteral( "Windows/%1/geometry" ).arg( objectName() ), mStackedWidget->saveGeometry() );
  }
}

void ReosStackedPageWidget::setStackedWidget( ReosActionStackedWidget *newStackedWidget )
{
  mStackedWidget = newStackedWidget;
}
