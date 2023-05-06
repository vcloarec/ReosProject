/***************************************************************************
  reosrainfallmodel.h - ReosRainfallModel

 ---------------------
 begin                : 24.1.2021
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
#ifndef REOSRAINFALLMODEL_H
#define REOSRAINFALLMODEL_H

#include <memory>

#include <QAbstractItemModel>

#include "reoscore.h"
#include "reosduration.h"
#include "reosrainfallitem.h"
#include "reosmapextent.h"

#define SIP_NO_FILE

class ReosEncodedElement;
class ReosRainfallItem;
class ReosRootItem;
class ReosZoneItem;
class ReosStationItem;
class ReosRainfallGaugedRainfallItem;
class ReosTimeSeriesConstantInterval;
class ReosRainfallIdfCurvesItem;
class ReosRainfallIntensityDurationCurveItem;
class ReosGriddedRainfall;
class ReosGriddedRainItem;

class REOSCORE_EXPORT ReosRainfallModel: public QAbstractItemModel
{
    Q_OBJECT
  public:
    ReosRainfallModel( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const override;
    QModelIndex parent( const QModelIndex &child ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    bool canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const override;
    bool dropMimeData( const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent ) override;
    QMimeData *mimeData( const QModelIndexList &indexes ) const override;
    Qt::DropActions supportedDropActions() const override {return Qt::MoveAction;}
    Qt::DropActions supportedDragActions() const override {return Qt::MoveAction | Qt::CopyAction;}
    QStringList mimeTypes() const override;

    //! Add a zone to the hierarchical tree, if \a index is invalid, add to the roots return fals if it fails
    ReosZoneItem *addZone( const QString &name, const QString &description, const QModelIndex &index = QModelIndex() );
    ReosStationItem *addStation( const QString &name, const QString &description, const QModelIndex &index, const ReosSpatialPosition &position = ReosSpatialPosition() );
    ReosRainfallGaugedRainfallItem *addGaugedRainfall( const QString &name, const QString &description, const QModelIndex &index, ReosSeriesRainfall *data = nullptr );
    ReosRainfallChicagoItem *addChicagoRainfall( const QString &name, const QString &description, const QModelIndex &index );
    ReosRainfallAlternatingBlockItem *addAlternatingBlockRainfall( const QString &name, const QString &description, const QModelIndex &index );
    ReosRainfallDoubleTriangleItem *addDoubleTriangleRainfall( const QString &name, const QString &description, const QModelIndex &index );
    ReosRainfallIdfCurvesItem *addIDFCurves( const QString &name, const QString &description, const QModelIndex &index );
    ReosRainfallIntensityDurationCurveItem *addIDCurve( const ReosDuration &duration, const QString &description, const QModelIndex &index );
    ReosGriddedRainItem *addGriddedRainfall( const QString &name, const QString &description, const QModelIndex &index, ReosGriddedRainfall *data );
    void removeItem( ReosRainfallItem *item );

    int rootZoneCount() const;

    ReosZoneItem *rootZone( int i ) const;

    QModelIndex itemToIndex( ReosRainfallItem *item ) const;
    ReosRainfallItem *indexToItem( const QModelIndex &index ) const;

    void swapItems( ReosRainfallItem *parent, int first, int second );

    ReosRainfallItem *positonPathToItem( const QList<int> &path ) const;
    ReosRainfallItem *uriToItem( const QString &uri ) const;
    ReosRainfallItem *uniqueIdToItem( const QString &uid ) const;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const;
    bool decode( const ReosEncodedElement &element, const ReosEncodeContext &context );

    bool saveToFile( const QString &path );
    bool loadFromFile( const QString &path );

    bool isPathRelative() const;
    void setIsPathRelative( bool newIsPathRelative );

  signals:
    void saved( const QString &file );
    void loaded( const QString &file );
    void changed();

  private slots:
    void onItemChanged( ReosRainfallItem *item );
    void onItemWillBeRemovedfromParent( ReosRainfallItem *item, int pos );
    void onItemRemovedfromParent();
    void onItemWillBeInsertedInParent( ReosRainfallItem *item, int pos );
    void onItemInsertedInParent();

  private:
    std::unique_ptr<ReosRootItem> mRootZone;
    ReosRainfallItem *addItem( ReosRainfallItem *receiver, ReosRainfallItem *newItem );

    bool mIsPathRelative = true;

    //! Connects item and all the children
    void connectItem( ReosRainfallItem *item );

};

#endif // REOSRAINFALLMODEL_H
