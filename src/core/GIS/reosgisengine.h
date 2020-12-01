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
#include "reosencodedelement.h"

class ReosDigitalElevationModel;
class ReosDigitalElevationModelProvider;

/**
 * Reos module class that handles GIS layer
 */
class ReosGisEngine: public ReosModule
{
    Q_OBJECT
  public:
    //! Supported layer types
    enum LayerType
    {
      NoLayer,
      VectorLayer,
      RasterLayer,
      MeshLayer,
      NotSupported
    };


    //! Constructor
    ReosGisEngine( QObject *parent = nullptr );

    //! Adds a vector layer, if the loaded vector layer is invalid, do nothing and return false
    QString addVectorLayer( const QString &uri, const QString &name = QString() );
    //! Adds a raster layer, if the loaded vector layer is invalid, do nothing and return false
    QString addRasterLayer( const QString &uri, const QString &name = QString() );

    //! Adds a raster layer, if the loaded vector layer is invalid, do nothing and return false
    QString addMeshLayer( const QString &uri, const QString &name = QString() );

    //! Returns the layer type corresponding to the the layer Id
    LayerType layerType( const QString layerId ) const;

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

    //! Returns the list of layer Ids that are registered as Digital Elevation Model with associated name
    QMap<QString, QString> digitalElevationModelRasterList() const;

    //! Returns the list of layer Ids that are registered as Digital Elevation
    QStringList digitalElevationModelIds() const;

    //! Returns encoded information about the GIS engine after saving GIS project int the \a path with the \a baseFileName
    ReosEncodedElement encode( const QString &path, const QString baseFileName );
    //! Decode information about the GIS engine and load the GIS poject from the \a path with the \a baseFileName
    bool decode( const ReosEncodedElement &encodedElement, const QString &path, const QString baseFileName );

  signals:
    void crsChanged( const QString &wktCrs );
    void digitalElevationRegistered( const QString &layerId );
    void digitalElevationUnregistered( const QString &layerId );
    void updated();

  private:
    QAbstractItemModel *mAbstractLayerTreeModel;
    ReosDigitalElevationModelProvider *mDemProvider;

    QStringList mAsDEMRegisteredLayer;
};

#endif // REOSGISENGINE_H
