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

class ReosDigitalElevationModel;
class ReosDigitalElevationModelProvider;

/**
 * Reos module class that handles GIS layer
 */
class ReosGisEngine: public ReosModule
{
    Q_OBJECT
  public:
    //! Constructor
    ReosGisEngine( QObject *parent = nullptr );

    //! Adds a vector layer, if the loaded vector layer is invalid, do nothing and return false
    QString addVectorLayer( const QString &uri, const QString &name );
    //! Adds a raster layer, if the loaded vector layer is invalid, do nothing and return false
    QString addRasterLayer( const QString &uri, const QString &name );

    //! Adds a raster layer, if the loaded vector layer is invalid, do nothing and return false
    QString addMeshLayer( const QString &uri, const QString &name );

    //! Adds a empty group layer
    void addGroupLayer();

    //! Returns the model containing GIS layers tree
    QAbstractItemModel *layerTreeModel();

    //! Returns vector layer file suffix filter
    QString vectorLayerFilters() const;
    //! Returns raster layer file suffix filter
    QString rasterLayerFilters() const;
    //! Returns mesh layer file suffix filter
    QString meshLayerFilters() const;

    //! Returns the coordinate reference system of the GIS project
    QString crs() const ;

    //! Sets the coordinate reference system of the GIS project
    void setCrs( const QString &wktCrs );

    //! loads a QGIS project as GIS project
    void loadQGISProject( const QString &fileName );

    //! Saves the GIS project as a QGIS project
    void saveQGISProject( const QString &fileName ) const;

    //! Registers the layer with \a layerId unique as a Digital Elevation Model, returns true is sucessful
    bool registerLayerAsDigitalElevationModel( const QString &layerId );

    //! Unregisters the layer with \a layerId unique as a Digital Elevation Model, does nothing if no valid layer Id
    void unRegisterLayerAsDigitalElevationModel( const QString &layerId );

    //! Returns whether the layrId is registered as a Digigtal Elevation Model
    bool isDigitalElevationModel( const QString &layerId ) const;

    //! Returns a pointer to the on the top Digitial Elevation Model, caller take ownership
    ReosDigitalElevationModel *getTopDigitalElevationModel() const;

  signals:
    void crsChanged( const QString &wktCrs );

  private:
    QAbstractItemModel *mAbstractLayerTreeModel;
    ReosDigitalElevationModelProvider *mDemProvider;
    QList<QString> mAsDEMRegisteredLayer;
};

#endif // REOSGISENGINE_H
