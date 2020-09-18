/***************************************************************************
                      reosgisengine.h
                     --------------------------------------
Date                 : 17-09-2020
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


#ifndef REOSGISENGINE_H
#define REOSGISENGINE_H

#include <QAbstractItemModel>

#include "reosmodule.h"

/**
 * Reos module class that handles GIS layer
 */
class ReosGisEngine: public ReosModule
{
  public:
    //! Constructor
    ReosGisEngine( QObject *parent = nullptr );

    //! Add a vector layer, if the loaded vector layer is invalid, do nothing and return false
    bool addVectorLayer( QString uri, QString name );
    //! Add a raster layer, if the loaded vector layer is invalid, do nothing and return false
    bool addRasterLayer( QString uri, QString name );

    //! Add a raster layer, if the loaded vector layer is invalid, do nothing and return false
    bool addMeshLayer( QString uri, QString name );

    //! Returns the model containing GIS layers tree
    QAbstractItemModel *layerTreeModel();

    QString vectorLayerFilters() const;
    QString rasterLayerFilters() const;
    QString meshLayerFilters() const;

  private:
    QAbstractItemModel *mLayerTreeModel;
};

#endif // REOSGISENGINE_H
