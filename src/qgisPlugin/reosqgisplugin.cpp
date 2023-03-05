/***************************************************************************
  reosqgisplugin.cpp - ReosQgisPlugin

 ---------------------
 begin                : 3.3.2023
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
#include "reosqgisplugin.h"
#include <qgisinterface.h>
#include <QAction>
#include <QMenu>

#include <qgsmapcanvas.h>

#include "reoscore.h"
#include "reosgisengine.h"
#include "reoswatershedmodule.h"
#include "reosdelineatingwatershedwidget.h"
#include "reosmap.h"
#include "reosguicontext.h"

ReosQgisPlugin::ReosQgisPlugin( QgisInterface *iface )
  : QgisPlugin( sName, sDescription, sCategory, sPluginVersion, sPluginType )
  , mIface( iface )
  , mGisEngine( new ReosGisEngine( this, false ) )
  , mRootModule( new ReosModule( this ) )
  , mWatershedModule( new ReosWatershedModule( mRootModule, mGisEngine ) )
{
}

void ReosQgisPlugin::initGui()
{
  mMenuAction = new QAction( tr( "Reos plugin" ), this );
  mIface->addPluginToVectorMenu( "Reos", mMenuAction );
  mMap = new ReosMap( mIface->mapCanvas(), mGisEngine );
  ReosGuiContext context( mIface->mainWindow() );
  context.setMap( mMap );
  ReosDelineatingWatershedWidget *delinateWidget = new ReosDelineatingWatershedWidget( mWatershedModule, context );
  delinateWidget->setAction( mMenuAction );
}

void ReosQgisPlugin::unload()
{
  mMenuAction->deleteLater();
  mIface->vectorMenu()->removeAction( mMenuAction );
}


/**
 * Required extern functions needed  for every plugin
 * These functions can be called prior to creating an instance
 * of the plugin class
 */
// Class factory to return a new instance of the plugin class
REOSEXTERN QgisPlugin *classFactory( QgisInterface *qgisInterfacePointer )
{
  return new ReosQgisPlugin( qgisInterfacePointer );
}
// Return the name of the plugin - note that we do not user class members as
// the class may not yet be insantiated when this method is called.
REOSEXTERN const QString *name()
{
  return &sName;
}

// Return the description
REOSEXTERN const QString *description()
{
  return &sDescription;
}

// Return the category
REOSEXTERN const QString *category()
{
  return &sCategory;
}

// Return the type (either UI or MapLayer plugin)
REOSEXTERN int type()
{
  return sPluginType;
}

// Return the version number for the plugin
REOSEXTERN const QString *version()
{
  return &sPluginVersion;
}

REOSEXTERN const QString *icon()
{
  return &sPluginIcon;
}

// Delete ourself
REOSEXTERN void unload( QgisPlugin *pluginPointer )
{
  delete pluginPointer;
}
