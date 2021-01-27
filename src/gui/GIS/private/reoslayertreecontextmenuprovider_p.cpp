/***************************************************************************
                      reoslayertreecontextmenuprovider_p.cpp
                     --------------------------------------
Date                 : 01-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QMenu>

#include <qgslayertreelayer.h>
#include <qgslayertreeviewdefaultactions.h>
#include <qgsmapcanvas.h>
#include <qgsrasterlayer.h>

#include "reosgislayerswidget.h"
#include "reoslayertreecontextmenuprovider_p.h"

ReosGisLayerTreeContextMenuProvider::ReosGisLayerTreeContextMenuProvider( ReosGisLayersWidget *layerWidget, QgsLayerTreeView *layerTreeView, ReosMap *map ):
  QgsLayerTreeViewMenuProvider(),
  mLayerWidget( layerWidget ),
  mLayerTreeView( layerTreeView ),
  mMap( map )
{
  mDefaultAction = new QgsLayerTreeViewDefaultActions( layerTreeView );
}

QMenu *ReosGisLayerTreeContextMenuProvider::createContextMenu()
{
  QMenu *menu = new QMenu;
  menu->addAction( mDefaultAction->actionRemoveGroupOrLayer( mLayerTreeView ) );
  menu->addAction( mDefaultAction->actionRenameGroupOrLayer( mLayerTreeView ) );
  menu->addSeparator();
  menu->addAction( mDefaultAction->actionCheckAndAllChildren( mLayerTreeView ) );
  menu->addAction( mDefaultAction->actionUncheckAndAllChildren( mLayerTreeView ) );
  menu->addAction( mDefaultAction->actionCheckAndAllParents( mLayerTreeView ) );

  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mMap->mapCanvas() );
  if ( mapCanvas )
  {
    menu->addSeparator();
    menu->addAction( mDefaultAction->actionZoomToLayers( mapCanvas, mLayerTreeView ) );
    menu->addAction( mDefaultAction->actionZoomToGroup( mapCanvas, mLayerTreeView ) );
  }

  menu->addSeparator();
  menu->addAction( mDefaultAction->actionMoveOutOfGroup( mLayerTreeView ) );
  menu->addAction( mDefaultAction->actionMoveToTop( mLayerTreeView ) );
  menu->addAction( mDefaultAction->actionMoveToBottom( mLayerTreeView ) );
  menu->addAction( mDefaultAction->actionGroupSelected( mLayerTreeView ) );

  QgsRasterLayer *rasterLayer = qobject_cast<QgsRasterLayer *>( mLayerTreeView->currentLayer() );
  if ( rasterLayer && rasterLayer->dataProvider() && rasterLayer->dataProvider()->bandCount() == 1 )
  {
    menu->addSeparator();
    if ( mLayerWidget->isLayerDigitalElevationModel( rasterLayer->id() ) )
    {
      QAction *unRegisterAction = menu->addAction( QPixmap( ":/images/noDem.svg" ), QObject::tr( "Unregister as Digital Elevation Model" ) );
      QObject::connect( unRegisterAction, &QAction::triggered, mLayerWidget, &ReosGisLayersWidget::unRegisterCurrentLayerAsDigitalElevationModel );
    }
    else
    {
      QAction *registerAction = menu->addAction( QPixmap( ":/images/dem.svg" ), QObject::tr( "Register as Digital Elevation Model" ) );
      QObject::connect( registerAction, &QAction::triggered, mLayerWidget, &ReosGisLayersWidget::registerCurrentLayerAsDigitalElevationModel );
    }
  }

  return menu;
}
