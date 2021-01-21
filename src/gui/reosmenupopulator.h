/***************************************************************************
  reosmenupopulator.h - ReosMenuPopulator

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
#ifndef REOSMENUPOPULATOR_H
#define REOSMENUPOPULATOR_H

#include <QList>

class QMenu;
class QAction;

/**
 *  Class that populate QMenu, action can be embeded to be used later. This class can be derived to provide more functionnality.
 *  An instance of this clas scan be set in a ReosMapTool instance to provide action to context menu.
 */
class ReosMenuPopulator
{
  public:
    //! Constructor
    ReosMenuPopulator();

    //! Populates the menu
    virtual void populate( QMenu *menu );

    //! Add an action than will be added to menu
    void addAction( QAction *action );

  private:
    QList<QAction *> mActions;
};

#endif // REOSMENUPOPULATOR_H
