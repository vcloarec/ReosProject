/***************************************************************************
  reosdockwidget.cpp - ReosDockWidget

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
#include "reosdockwidget.h"
#include <QShowEvent>

ReosDockWidget::ReosDockWidget( const QString &title, QWidget *parent ) : QDockWidget( title, parent )
{}

void ReosDockWidget::showEvent( QShowEvent *e )
{
  emit shown();
  QDockWidget::changeEvent( e );
}

void ReosDockWidget::closeEvent( QCloseEvent *e )
{
  emit closed();
  QDockWidget::closeEvent( e );
}
