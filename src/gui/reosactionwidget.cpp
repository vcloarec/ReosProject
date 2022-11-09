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

ReosActionWidget::ReosActionWidget( QWidget *parent ) : QWidget( parent )
{
  connect( this, &QObject::destroyed, this, &ReosActionWidget::storeGeometry );
}

void ReosActionWidget::setAction( QAction *action )
{
  mAction = action;
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

void ReosActionStackedWidget::addPage( ReosStackedPageWidget *widget )
{
  if ( mStackedWidget->count() > 0 )
    widget->showBackButton();

  mStackedWidget->addWidget( widget );
  mStackedWidget->setCurrentWidget( widget );
  connect( widget, &ReosStackedPageWidget::backToPreviousPage, this, &ReosActionStackedWidget::backToPrevious );
  connect( widget, &ReosStackedPageWidget::addOtherPage, this, &ReosActionStackedWidget::addPage );
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
