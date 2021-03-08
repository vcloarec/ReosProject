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
      restore();
      show();
      emit opened();
    }
    else
    {
      storeGeometry();
      close();
    }
  } );
}

void ReosActionWidget::closeEvent( QCloseEvent *event )
{
  storeGeometry();
  if ( mAction )
    mAction->setChecked( false );
  setVisible( false );
  for ( ReosMapTool *tool : qAsConst( mMapTools ) )
  {
    if ( tool->isCurrentToolInMap() )
    {
      tool->quitMap();
      break;
    }
  }
  closingWidget();
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
