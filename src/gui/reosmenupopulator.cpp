/***************************************************************************
  reosmenupopulator.cpp - ReosMenuPopulator

 ---------------------
 begin                : 18.1.2021
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

#include <QAction>
#include <QMenu>

#include "reosmenupopulator.h"

ReosMenuPopulator::ReosMenuPopulator()
{}

void ReosMenuPopulator::populate( QMenu *menu )
{
  menu->clear();
  for ( QAction *action : std::as_const( mActions ) )
    menu->addAction( action );
}

void ReosMenuPopulator::addAction( QAction *action )
{
  mActions.append( action );
}
