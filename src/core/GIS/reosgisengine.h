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

#include <cmath>
#include <QAbstractItemModel>

#include "reoscore.h"
#include "reosmodule.h"
#include "reosencodedelement.h"
#include "reosarea.h"

class ReosDigitalElevationModel;
class ReosDigitalElevationModelProvider;
class ReosTriangularIrregularNetwork;
class QgsRasterLayer;

/**
 * Reos module class that handles GIS layer
 */
class REOSCORE_EXPORT ReosGisEngine: public ReosModule
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
    ~ReosGisEngine();

    void initGisEngine();

    /**
     *  Adds a vector layer, return the layer Id.
     *  If the loaded vector layer is invalid, do nothing and return void QString
    */
    QString addVectorLayer( const QString &uri, const QString &name = QString() );
     *  Adds a raster layer, return the layer Id.

    /**
     *  If the rasgter layer as to be registered as a DEM, \a isDem has to point to a bool with true value.
     */
    QString addRasterLayer( const QString &uri, const QString &name = QString(), bool *isDEM = nullptr );
    /**
     *  Adds a mesh layer, return the layer Id.
     *  If the loaded mesh layer is invalid, do nothing and return void QString
    */

    QString addMeshLayer( const QString &uri, const QString &name = QString() );

    /**
     *  Create a new TIN editor layer, return the layer Id.
     *  If the layer is not created return void QString.
    */
    QString createTinEditor( const QString &name );

    //! Returns the TIN assocated with the mesh layer with \a layerId, return nullptr if this Id does not correspond with a TIN
    ReosTriangularIrregularNetwork *triangularIrregularNetWork( const QString &layerId ) const;

    //! Returns the layer type corresponding to the the layer Id
    LayerType layerType( const QString layerId ) const;

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
    void setCrs( const QString &crsString );

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

    //! Returns a pointer to a Digitial Elevation Model corresponding to the topest layer registered as DEM, caller take ownership
    ReosDigitalElevationModel *getTopDigitalElevationModel() const;

    //! Returns a pointer to a Digitial Elevation Model corresponding to \a layerId, caller take ownership. Returns nullptr if layer is not registered as DEM
    ReosDigitalElevationModel *getDigitalElevationModel( const QString &layerId ) const;

    //! Returns the list of layer Ids that are registered as Digital Elevation Model with associated name
    QMap<QString, QString> digitalElevationModelRasterList() const;

    //! Returns the list of layer Ids that are registered as Digital Elevation
    QStringList digitalElevationModelIds() const;

    //! Returns area of \a polygon cosidering the coordinate reference system \a crs. If no crs is provided, the crs of the project is used
    ReosArea polygonArea( const QPolygonF &polygon, const QString &crs = QString() ) const;

    //! Returns encoded information about the GIS engine after saving GIS project int the \a path with the \a baseFileName
    ReosEncodedElement encode( const QString &path, const QString baseFileName );

    //! Decode information about the GIS engine and load the GIS poject from the \a path with the \a baseFileName
    bool decode( const ReosEncodedElement &encodedElement, const QString &path, const QString baseFileName );

    //! Clears the GIS project
    void clearProject();


    bool canBeRasterDem( const QString &uri ) const;

    static QString gisEngineName();
    static QString gisEngineVersion();
    static QString gisEngineLink();

  signals:
    void crsChanged( const QString &wktCrs );
    void updated();
    void cleared();

  private slots:
    void layerRemoved( const QString &layerId );

  private:
    QAbstractItemModel *mAbstractLayerTreeModel;
    ReosDigitalElevationModelProvider *mDemProvider;
    QStringList mAsDEMRegisteredLayer;

    void defaultstyleRasterLayer( QgsRasterLayer *layer );

    bool canBeRasterDem( QgsRasterLayer *layer ) const;

};

#endif // REOSGISENGINE_H
