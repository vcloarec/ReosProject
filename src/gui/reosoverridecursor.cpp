/***************************************************************************
  reosoverridecursor.cpp - ReosOverrideCursor

 ---------------------
 begin                : 6.3.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosoverridecursor.h"
#include <QApplication>

ReosOverrideCursor::ReosOverrideCursor()
{
  QApplication::setOverrideCursor( Qt::WaitCursor );
}

ReosOverrideCursor::~ReosOverrideCursor()
{
  QApplication::restoreOverrideCursor();
}
