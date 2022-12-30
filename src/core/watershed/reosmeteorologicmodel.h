/***************************************************************************
  reosmeteorologicmodel.h - ReosMeteorologicModel

 ---------------------
 begin                : 16.2.2021
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
#ifndef REOSMETEOROLOGICMODEL_H
#define REOSMETEOROLOGICMODEL_H

#include <QPointer>
#include <QIdentityProxyModel>
#include <QMimeData>

#include "reoswatershed.h"
#include "reosrainfallitem.h"

class ReosWatershedTree;
class ReosRainfallRegistery;
class ReosWatershedItemModel;
class ReosSeriesRainfall;
class ReosHydraulicStructure2D;
class ReosHydraulicNetworkElement;
class ReosHydraulicNetwork;
class ReosGriddedRainItem;

//! Class that handle association between watesheds and rainfalls
class REOSCORE_EXPORT ReosMeteorologicModel : public ReosDataObject
{
    Q_OBJECT
  public:
    explicit ReosMeteorologicModel( const QString &name, QObject *parent = nullptr );
    ReosMeteorologicModel( const ReosEncodedElement &element,
                           ReosWatershedTree *watershedTree,
                           ReosRainfallRegistery *rainfallregistery,
                           QObject *parent = nullptr );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosDataObject::staticType() + ':' +  QStringLiteral( "meteorologic-model" );}

    //! Returns a pointer to a copy of \a this
    ReosMeteorologicModel *duplicate( const QString &dupplicateName );

    //! Returns the name parameter
    ReosParameterString *name() const;
    //! Associates a \a rainfall with the \a watershed
    void associate( ReosWatershed *watershed, ReosRainfallDataItem *rainfall );

    //! Associates a \a rainfall with the \a hydraulic structure 2D
    void associate( ReosHydraulicStructure2D *structure, ReosRainfallDataItem *rainfall );

    //! Disassociation the rainfall associated with the \a watershed
    void disassociate( ReosWatershed *watershed );

    //! Disassociation the rainfall associated with the \a watershed
    void disassociate( ReosHydraulicStructure2D *structure2D );

    //! Returns the associated rainfall item of \a watershed
    ReosRainfallDataItem *associatedRainfallItem( ReosWatershed *watershed ) const;

    //! Returns the associated rainfall item of \a structure
    ReosGriddedRainItem *associatedRainfallItem( ReosHydraulicStructure2D *structure ) const;

    //! Returns the associated rainfall of \a watershed
    ReosSeriesRainfall *associatedRainfall( ReosWatershed *watershed ) const;

    //! Returns the associated rainfall of \a hydraulic structure 2d
    ReosGriddedRainfall *associatedRainfall( ReosHydraulicStructure2D *structure ) const;

    //! Returns whether the meteomodel has a associated rainfall for the watershed \a watershed
    bool hasRainfall( ReosWatershed *watershed ) const;

    //! Remove all reference with no association
    void purge() const;

    //! Returns the color used to display curve related to this meteorologic model
    QColor color() const;

    //! Returns the time window of the meteo model
    ReosTimeWindow timeWindow() const;

    //! Returns the minimal time step that can be related to the map
    ReosDuration mapTimeStep() const;

    ReosEncodedElement encode( ReosWatershedTree *watershedTree ) const;

    QHash<QString, ReosDataObject *> allRainfall() const;

  signals:
    //! Emitted when the color change
    void colorChanged( const QColor &color );
    void timeWindowChanged();
    void mapTimeStepChanged();

  public slots:
    //! Sets the color used to display curve related to this meteorologic model
    void setColor( const QColor &color );

  private:
    std::unique_ptr<ReosParameterString> mName;
    struct WatershedRainfallAssociation
    {
      QPointer<ReosWatershed> watershed;
      QPointer<ReosRainfallDataItem> rainfallDataItem;
      std::shared_ptr<ReosSeriesRainfall> resultingRainfall;
      QPointer<ReosHydraulicStructure2D> structure2D;
    };

    mutable QList<WatershedRainfallAssociation> mAssociations;
    mutable QMap<QString, WatershedRainfallAssociation> mTemporaryStructureAssocations; //used to link structure 2D to its id until we can resolve access to pointer
    QColor mColor;

    //! Searchs for \a watershed, if found , return its index, otherwise return -1
    int findWatershed( ReosWatershed *watershed ) const;

    //! Searchs for \a structure 2D, if found , return its index, otherwise return -1
    int findStructure( ReosHydraulicStructure2D *structure ) const;

    void resolveStructureAssociation( ReosHydraulicStructure2D *structure ) const;
};

//! List model class that represents a collection of meteorologic model
class REOSCORE_EXPORT ReosMeteorologicModelsCollection : public QAbstractListModel
{
    Q_OBJECT
  public:
    ReosMeteorologicModelsCollection( QObject *parent = nullptr );

    int rowCount( const QModelIndex & ) const override;
    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    //! Returns the meteorologic model at position \a i
    ReosMeteorologicModel *meteorologicModel( int i ) const;

    //! Return the meteorological model with id \a modelId
    ReosMeteorologicModel *meteorologicModel( const QString &modelid ) const;

    //! Returns the count of meteorologic models
    int modelCount() const;

    //! Returns the index of \a model
    int modelIndex( ReosMeteorologicModel *model ) const;

    //! Adds an empty (no association) meteorologic model with \a name
    void addMeteorologicModel( const QString &name );

    //! Adds an existing meteorologic \a model, takes ownership
    void addMeteorologicModel( ReosMeteorologicModel *model );

    //! Removes the meteorologic model at position \a i
    void removeMeteorologicModel( int i );

    //! Removes all the models
    void clearModels();

    //! Removes all models and create just a void one
    void reset();

    ReosEncodedElement encode( ReosWatershedTree *watershedTree ) const;
    void decode( const ReosEncodedElement &element, ReosWatershedTree *watershedTree, ReosRainfallRegistery *rainfallregistery );

  signals:
    emit void changed();

  private:
    QVector<ReosMeteorologicModel *> mMeteoModels;
};


//! Item model class that represents association between watershed and rainfall for a given meteorologic model
class REOSCORE_EXPORT ReosMeteorologicItemModel: public QIdentityProxyModel
{
    Q_OBJECT
  public:

    explicit ReosMeteorologicItemModel( ReosWatershedItemModel *watershedModel, QObject *parent = nullptr );

    QVariant data( const QModelIndex &index, int role )  const override;
    int columnCount( const QModelIndex & ) const override;
    bool canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const override;
    bool dropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    Qt::DropActions supportedDropActions() const override;
    QStringList mimeTypes() const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    //! Sets the current meteorologic model
    void setCurrentMeteorologicalModel( ReosMeteorologicModel *meteoModel );
    //! Removes the association for watershed at \a index
    void removeAssociation( const QModelIndex &index );

  private:
    ReosWatershedItemModel *mWatershedModel = nullptr;
    size_t currentConfig = 0;

    ReosMeteorologicModel *mCurrentMeteoModel = nullptr;

    ReosRainfallDataItem *rainfallDataInMeteorologicModel( const QModelIndex &index );
};

class REOSCORE_EXPORT ReosMeteorologicStructureItemModel: public QAbstractListModel
{
    Q_OBJECT
  public:
    explicit ReosMeteorologicStructureItemModel( ReosHydraulicNetwork *hydraulicNetwork, QObject *parent = nullptr );

    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;
    bool canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const override;
    bool dropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    QStringList mimeTypes() const override;
    Qt::DropActions supportedDropActions() const override;

    void setCurrentMeteoModel( ReosMeteorologicModel *newCurrentMeteoModel );

    QModelIndex structureToIndex( ReosHydraulicStructure2D *structure ) const;

    //! Removes the association for structure at \a index
    void removeAssociation( const QModelIndex &index );


  private slots:
    void onHydraulicNetworkElementAddedRemoved();

  private:
    QPointer<ReosHydraulicNetwork> mNetwork;
    QList<ReosHydraulicStructure2D *> mStructures;
    ReosMeteorologicModel *mCurrentMeteoModel = nullptr;
};

#endif // REOSMETEOROLOGICMODEL_H
