#ifndef REOSLAYERTREECONTEXTMENUPROVIDER_H
#define REOSLAYERTREECONTEXTMENUPROVIDER_H

#include <qgslayertreeview.h>

#include "reosmap.h"

class ReosGisLayersWidget;

class ReosGisLayerTreeContextMenuProvider: public QgsLayerTreeViewMenuProvider
{
  public:
    ReosGisLayerTreeContextMenuProvider( ReosGisLayersWidget *layerWidget, QgsLayerTreeView *layerTreeView, ReosMap *map );
    QMenu *createContextMenu() override;

  private:
    ReosGisLayersWidget *mLayerWidget;
    QgsLayerTreeView *mLayerTreeView = nullptr;
    QgsLayerTreeViewDefaultActions *mDefaultAction = nullptr;
    ReosMap *mMap = nullptr;

    QAction *mActionRegisterAsDem = nullptr;
};

#endif // REOSLAYERTREECONTEXTMENUPROVIDER_H
