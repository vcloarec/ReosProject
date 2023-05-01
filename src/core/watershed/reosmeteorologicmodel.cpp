/***************************************************************************
  reosmeteorologicmodel.cpp - ReosMeteorologicModel

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
#include "reosmeteorologicmodel.h"
#include "reoswatershedtree.h"
#include "reosrainfallregistery.h"
#include "reosstyleregistery.h"
#include "reosgriddedrainitem.h"
#include "reosseriesrainfall.h"
#include "reosgriddedrainitem.h"
#include "reoshydraulicstructure2d.h"

ReosMeteorologicModel::ReosMeteorologicModel( const QString &name, QObject *parent ):
  ReosDataObject( parent )
  , mName( new ReosParameterString( QObject::tr( "Meteorologic model name" ), false, nullptr ) )
{
  mName->setValue( name );
  mColor = ReosStyleRegistery::instance()->curveColor();
}

ReosMeteorologicModel::ReosMeteorologicModel(
  const ReosEncodedElement &element,
  ReosWatershedTree *watershedTree,
  ReosRainfallRegistery *rainfallregistery,
  QObject *parent ):
  ReosDataObject( parent )
  , mName( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, QObject::tr( "Meteorologic model name" ), nullptr ) )
{
  ReosDataObject::decode( element );

  if ( element.description() != QStringLiteral( "meteorologic-configuration" ) )
    return;

  QMap<QString, QString> associations;
  QMap<QString, QString> structureAssociations;
  element.getData( QStringLiteral( "associations" ), associations );
  element.getData( QStringLiteral( "structure-2d-associations" ), structureAssociations );
  element.getData( QStringLiteral( "color" ), mColor );

  if ( !mColor.isValid() )
    mColor = ReosStyleRegistery::instance()->curveColor();

  const QList<QString> keys = associations.keys();
  for ( const QString &watershedUri : keys )
  {
    ReosWatershed *ws = watershedTree->uriToWatershed( watershedUri );
    ReosRainfallDataItem *rainfall = qobject_cast<ReosRainfallDataItem *>
                                     ( rainfallregistery->itemByUniqueId( associations.value( watershedUri ) ) );

    if ( ws && rainfall )
      mAssociations.append( {QPointer<ReosWatershed>( ws ), QPointer<ReosRainfallDataItem>( rainfall ), nullptr, nullptr} );
  }

  for ( auto it = structureAssociations.constBegin(); it != structureAssociations.constEnd(); ++it )
  {
    ReosGriddedRainItem *rainfall = qobject_cast<ReosGriddedRainItem *>
                                    ( rainfallregistery->itemByUniqueId( it.value() ) );
    mTemporaryStructureAssocations.insert( it.key(), {nullptr, QPointer<ReosRainfallDataItem>( rainfall ), nullptr, nullptr} );
  }
}

ReosEncodedElement ReosMeteorologicModel::encode( ReosWatershedTree *watershedTree ) const
{
  QMap<QString, QString> associations;
  for ( const WatershedRainfallAssociation &association : std::as_const( mAssociations ) )
  {
    if ( !association.watershed.isNull() && !association.rainfallDataItem.isNull() )
    {
      const QString watershedUri = watershedTree->watershedUri( association.watershed );
      const QString rainfallUid = association.rainfallDataItem->uniqueId();
      associations[watershedUri] = rainfallUid;
    }
  }

  QMap<QString, QString> structureAssociations;
  for ( const WatershedRainfallAssociation &association : std::as_const( mAssociations ) )
  {
    if ( !association.structure2D.isNull() && !association.rainfallDataItem.isNull() )
    {
      const QString structureId = association.structure2D->id();
      const QString rainfallUid = association.rainfallDataItem->uniqueId();
      structureAssociations[structureId] = rainfallUid;
    }
  }

  for ( auto it = mTemporaryStructureAssocations.constBegin(); it != mTemporaryStructureAssocations.constEnd(); ++it )
  {
    const QString structureId = it.key();
    const QString rainfallUid = it->rainfallDataItem->uniqueId();
    structureAssociations[structureId] = rainfallUid;
  }

  ReosEncodedElement element( QStringLiteral( "meteorologic-configuration" ) );

  element.addData( QStringLiteral( "associations" ), associations );
  element.addData( QStringLiteral( "structure-2d-associations" ), structureAssociations );
  element.addEncodedData( QStringLiteral( "name" ), mName->encode() );
  element.addData( QStringLiteral( "color" ), mColor );

  ReosDataObject::encode( element );

  return element;
}

ReosMeteorologicModel *ReosMeteorologicModel::duplicate( const QString &dupplicateName )
{
  std::unique_ptr<ReosMeteorologicModel> duplicate = std::make_unique<ReosMeteorologicModel>( dupplicateName );
  duplicate->mAssociations = mAssociations;
  duplicate->mTemporaryStructureAssocations = mTemporaryStructureAssocations;
  return duplicate.release();
}

ReosParameterString *ReosMeteorologicModel::name() const
{
  return mName.get();
}

QHash<QString, ReosDataObject *> ReosMeteorologicModel::allRainfall() const
{
  QHash<QString, ReosDataObject *> ret;
  for ( const WatershedRainfallAssociation &association : std::as_const( mAssociations ) )
  {
    if ( association.rainfallDataItem.isNull() || !association.rainfallDataItem->data() )
      continue;
    ret.insert( association.rainfallDataItem->data()->id(), association.rainfallDataItem->data() );
  }

  for ( const WatershedRainfallAssociation &association : std::as_const( mTemporaryStructureAssocations ) )
  {
    if ( association.rainfallDataItem.isNull() || !association.rainfallDataItem->data() )
      continue;
    ret.insert( association.rainfallDataItem->data()->id(), association.rainfallDataItem->data() );
  }
  return ret;
}

void ReosMeteorologicModel::setColor( const QColor &color )
{
  mColor = color;
  emit colorChanged( color );
}

void ReosMeteorologicModel::associate( ReosWatershed *watershed, ReosRainfallDataItem *rainfall )
{
  int index = findWatershed( watershed );

  if ( index >= 0 )
  {
    mAssociations[index].rainfallDataItem = rainfall;
    mAssociations[index].resultingRainfall.reset();
  }
  else
    mAssociations.append( {QPointer<ReosWatershed>( watershed ), QPointer<ReosRainfallDataItem>( rainfall ), nullptr, nullptr} );

  purge();

  emit timeWindowChanged();
  emit mapTimeStepChanged();
  emit dataChanged();
}

void ReosMeteorologicModel::associate( ReosHydraulicStructure2D *structure, ReosRainfallDataItem *rainfall )
{
  if ( !structure || !rainfall )
    return;

  if ( mTemporaryStructureAssocations.contains( structure->id() ) )
    mTemporaryStructureAssocations.remove( structure->id() );

  int index = findStructure( structure );

  if ( index >= 0 )
  {
    mAssociations[index].rainfallDataItem = rainfall;
    mAssociations[index].resultingRainfall.reset();
  }
  else
  {
    mAssociations.append( {nullptr, QPointer<ReosRainfallDataItem>( rainfall ), nullptr, structure} );
  }

  purge();

  emit timeWindowChanged();
  emit mapTimeStepChanged();
  emit dataChanged();
}

void ReosMeteorologicModel::disassociate( ReosWatershed *watershed )
{
  int index = findWatershed( watershed );

  if ( index >= 0 )
    mAssociations.removeAt( index );

  emit timeWindowChanged();
  emit mapTimeStepChanged();
  emit dataChanged();
}

void ReosMeteorologicModel::disassociate( ReosHydraulicStructure2D *structure2D )
{
  if ( mTemporaryStructureAssocations.contains( structure2D->id() ) )
    mTemporaryStructureAssocations.remove( structure2D->id() );

  int index = findStructure( structure2D );

  if ( index >= 0 )
    mAssociations.removeAt( index );

  emit timeWindowChanged();
  emit mapTimeStepChanged();
  emit dataChanged();
}

ReosRainfallDataItem *ReosMeteorologicModel::associatedRainfallItem( ReosWatershed *watershed ) const
{
  int watershedIndex = findWatershed( watershed );

  if ( watershedIndex >= 0 )
    return mAssociations.at( watershedIndex ).rainfallDataItem;
  else
    return nullptr;
}

ReosGriddedRainItem *ReosMeteorologicModel::associatedRainfallItem( ReosHydraulicStructure2D *structure ) const
{
  resolveStructureAssociation( structure );
  int index = findStructure( structure );

  if ( index >= 0 )
    return qobject_cast<ReosGriddedRainItem *>( mAssociations.at( index ).rainfallDataItem );
  else
    return nullptr;
}

ReosSeriesRainfall *ReosMeteorologicModel::associatedRainfall( ReosWatershed *watershed ) const
{
  int watershedIndex = findWatershed( watershed );

  if ( watershedIndex >= 0 )
  {
    ReosRainfallDataItem *rainfallItem = mAssociations.at( watershedIndex ).rainfallDataItem;
    if ( rainfallItem->data() )
    {
      const QString dataType = rainfallItem->data()->type();

      if ( dataType.contains( ReosSeriesRainfall::staticType() ) )
      {
        return qobject_cast<ReosSeriesRainfall *>( rainfallItem->data() );
      }
      else if ( dataType.contains( ReosGriddedRainfall::staticType() ) )
      {
        if ( !mAssociations.at( watershedIndex ).resultingRainfall )
        {
          ReosGriddedRainfall *griddedRainfall = qobject_cast<ReosGriddedRainfall *>( rainfallItem->data() );
          mAssociations[watershedIndex].resultingRainfall.reset( new ReosSeriesRainfallFromGriddedOnWatershed( watershed, griddedRainfall ) );
        }
        return mAssociations[watershedIndex].resultingRainfall.get();
      }
    }
  }

  return nullptr;
}

ReosGriddedRainfall *ReosMeteorologicModel::associatedRainfall( ReosHydraulicStructure2D *structure ) const
{
  resolveStructureAssociation( structure );
  int index = findStructure( structure );

  if ( index >= 0 )
  {
    ReosGriddedRainItem *rainfallItem = qobject_cast<ReosGriddedRainItem *>( mAssociations.at( index ).rainfallDataItem );
    if ( rainfallItem && rainfallItem->data() )
      return rainfallItem->data();
  }

  return nullptr;
}

bool ReosMeteorologicModel::hasRainfall( ReosWatershed *watershed ) const
{
  if ( !watershed )
    return false;

  for ( const WatershedRainfallAssociation &as : std::as_const( mAssociations ) )
    if ( as.watershed.data() == watershed && !as.rainfallDataItem.isNull() )
      return true;

  return false;
}

int ReosMeteorologicModel::findWatershed( ReosWatershed *watershed ) const
{
  for ( int i = 0; i < mAssociations.count(); ++i )
  {
    if ( !mAssociations.at( i ).watershed.isNull() && mAssociations.at( i ).watershed == watershed )
      return i;
  }

  return -1;
}

int ReosMeteorologicModel::findStructure( ReosHydraulicStructure2D *structure ) const
{
  for ( int i = 0; i < mAssociations.count(); ++i )
  {
    if ( !mAssociations.at( i ).structure2D.isNull() && mAssociations.at( i ).structure2D == structure )
      return i;
  }

  return -1;
}

void ReosMeteorologicModel::resolveStructureAssociation( ReosHydraulicStructure2D *structure ) const
{
  if ( mTemporaryStructureAssocations.contains( structure->id() ) )
  {
    ReosRainfallDataItem *item = mTemporaryStructureAssocations.value( structure->id() ).rainfallDataItem;
    mAssociations.append( {nullptr, item, nullptr, structure} );
    mTemporaryStructureAssocations.remove( structure->id() );
  }
}

void ReosMeteorologicModel::purge() const
{
  int i = 0;
  while ( i < mAssociations.count() )
  {
    if ( ( mAssociations.at( i ).watershed.isNull() && mAssociations.at( i ).structure2D.isNull() )
         || mAssociations.at( i ).rainfallDataItem.isNull() )
      mAssociations.removeAt( i );
    else
      ++i;
  }
}

QColor ReosMeteorologicModel::color() const
{
  return mColor;
}

ReosTimeWindow ReosMeteorologicModel::timeWindow() const
{
  ReosTimeWindow globalTw;
  const QList<ReosDataObject *> allData = allRainfall().values();
  for ( ReosDataObject *object : allData )
  {
    ReosTimeWindow tw;
    QPair<QDateTime, QDateTime> timeExtent;
    if ( ReosSeriesRainfall *sr = qobject_cast<ReosSeriesRainfall *>( object ) )
      timeExtent = sr->timeExtent();
    else if ( ReosGriddedRainfall *gr = qobject_cast<ReosGriddedRainfall *>( object ) )
      timeExtent = gr->timeExtent();

    tw = ReosTimeWindow( timeExtent.first, timeExtent.second );

    globalTw = globalTw.unite( tw );
  }

  return globalTw;
}

ReosDuration ReosMeteorologicModel::mapTimeStep() const
{
  ReosDuration minTimeStep;
  const QList<ReosDataObject *> allData = allRainfall().values();

  for ( ReosDataObject *object : allData )
  {
    if ( ReosGriddedRainfall *gr = qobject_cast<ReosGriddedRainfall *>( object ) )
    {
      ReosTimeWindow tw;
      if ( minTimeStep != ReosDuration() )
      {
        ReosDuration grTs = gr->minimumTimeStep();
        if ( grTs < minTimeStep )
          minTimeStep = grTs;
      }
      else
      {
        minTimeStep = gr->minimumTimeStep();
      }
    }
  }

  return minTimeStep;
}


ReosMeteorologicItemModel::ReosMeteorologicItemModel( ReosWatershedItemModel *watershedModel, QObject *parent ):
  QIdentityProxyModel( parent )
{
  setSourceModel( watershedModel );
}

int ReosMeteorologicItemModel::columnCount( const QModelIndex & ) const {return 2;}

QVariant ReosMeteorologicItemModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.column() == 0 )
    return sourceModel()->data( index, role );

  if ( !mCurrentMeteoModel )
    return QVariant();

  if ( index.column() == 1 )
  {
    ReosWatershed *ws = mWatershedModel->indexToWatershed( mapToSource( index ) );
    ReosRainfallDataItem *rainfallData = mCurrentMeteoModel->associatedRainfallItem( ws );
    if ( role == Qt::DisplayRole )
    {
      if ( rainfallData )
        return rainfallData->name();
    }

    if ( role == Qt::DecorationRole )
    {
      if ( rainfallData )
        return rainfallData->icone();
    }
  }

  return QVariant();

}

static ReosRainfallDataItem *rainfallDataInRainfallModel( const QString &uri )
{
  if ( ReosRainfallRegistery::isInstantiate() )
    return  qobject_cast<ReosRainfallDataItem *>( ReosRainfallRegistery::instance()->itemByUri( uri ) ) ;

  return nullptr;
}

bool ReosMeteorologicItemModel::canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const
{
  if ( rainfallDataInRainfallModel( data->text() ) && parent.isValid() )
    return true;

  return false;
}

bool ReosMeteorologicItemModel::dropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent )
{
  ReosRainfallDataItem *rainfallData = rainfallDataInRainfallModel( data->text() );

  if ( rainfallData && parent.isValid() && mCurrentMeteoModel )
  {
    ReosWatershed *ws = mWatershedModel->indexToWatershed( mapToSource( parent ) );
    if ( ws )
    {
      mCurrentMeteoModel->associate( ws, rainfallData );
      emit dataChanged( parent, parent );
      return true;
    }
  }

  return false;
}

Qt::ItemFlags ReosMeteorologicItemModel::flags( const QModelIndex &index ) const
{
  return sourceModel()->flags( mapToSource( index ) ) | Qt::ItemIsDropEnabled;
}

Qt::DropActions ReosMeteorologicItemModel::supportedDropActions() const
{
  return Qt::CopyAction;
}

QStringList ReosMeteorologicItemModel::mimeTypes() const
{
  QStringList ret;
  ret << QStringLiteral( "text/plain" );
  return ret;
}

QVariant ReosMeteorologicItemModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( section > 2 )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    if ( section == 0 )
      return tr( "Watershed" );
    if ( section == 1 )
      return tr( "Associated rainfall" );
  }

  return QVariant();
}

void ReosMeteorologicItemModel::setCurrentMeteorologicalModel( ReosMeteorologicModel *meteoModel )
{
  beginResetModel();
  mCurrentMeteoModel = meteoModel;
  endResetModel();
}

void ReosMeteorologicItemModel::removeAssociation( const QModelIndex &index )
{
  if ( !mCurrentMeteoModel )
    return;

  ReosWatershed *ws = mWatershedModel->indexToWatershed( mapToSource( index ) );
  mCurrentMeteoModel->disassociate( ws );
  dataChanged( index, index.siblingAtColumn( 1 ) );
}


ReosRainfallDataItem *ReosMeteorologicItemModel::rainfallDataInMeteorologicModel( const QModelIndex &index )
{
  if ( !mCurrentMeteoModel )
    return nullptr;
  ReosWatershed *ws = mWatershedModel->indexToWatershed( mapToSource( index ) );
  return mCurrentMeteoModel->associatedRainfallItem( ws );
}

ReosMeteorologicModelsCollection::ReosMeteorologicModelsCollection( QObject *parent ): QAbstractListModel( parent )
{
  reset();
}

int ReosMeteorologicModelsCollection::rowCount( const QModelIndex & ) const
{
  return static_cast<int>( mMeteoModels.size() );
}

QModelIndex ReosMeteorologicModelsCollection::index( int row, int column, const QModelIndex & ) const
{
  if ( row < 0 || row >= modelCount() )
    return QModelIndex();

  return createIndex( row, column, mMeteoModels.at( row ) );
}

QModelIndex ReosMeteorologicModelsCollection::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

QVariant ReosMeteorologicModelsCollection::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() || index.row() > modelCount() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    return meteorologicModel( index.row() )->name()->value();
  }

  return QVariant();
}

ReosMeteorologicModel *ReosMeteorologicModelsCollection::meteorologicModel( int i ) const
{
  if ( i < 0 || i >= static_cast<int>( mMeteoModels.size() ) )
    return nullptr;

  return mMeteoModels.at( i );
}

ReosMeteorologicModel *ReosMeteorologicModelsCollection::meteorologicModel( const QString &modelid ) const
{
  for ( int i = 0; i < mMeteoModels.count(); ++i )
  {
    if ( mMeteoModels.at( i )->id() == modelid )
      return mMeteoModels.at( i );
  }

  return nullptr;
}

int ReosMeteorologicModelsCollection::modelCount() const
{
  return static_cast<int>( mMeteoModels.size() );
}

int ReosMeteorologicModelsCollection::modelIndex( ReosMeteorologicModel *model ) const
{
  return mMeteoModels.indexOf( model );
}

void ReosMeteorologicModelsCollection::addMeteorologicModel( const QString &name )
{
  beginInsertRows( QModelIndex(), modelCount(), modelCount() );
  mMeteoModels.append( new ReosMeteorologicModel( name, this ) );
  connect( mMeteoModels.last(), &ReosMeteorologicModel::dataChanged, this, &ReosMeteorologicModelsCollection::changed );
  endInsertRows();
}

void ReosMeteorologicModelsCollection::addMeteorologicModel( ReosMeteorologicModel *model )
{
  beginInsertRows( QModelIndex(), modelCount(), modelCount() );
  mMeteoModels.append( model );
  model->setParent( this );
  connect( model, &ReosMeteorologicModel::dataChanged, this, &ReosMeteorologicModelsCollection::changed );
  endInsertRows();
}

void ReosMeteorologicModelsCollection::removeMeteorologicModel( int i )
{
  beginRemoveRows( QModelIndex(), i, i );
  mMeteoModels.takeAt( i )->deleteLater();
  endRemoveRows();
  emit changed();
}

void ReosMeteorologicModelsCollection::clearModels()
{
  while ( !mMeteoModels.isEmpty() )
    mMeteoModels.takeAt( 0 )->deleteLater();
}

void ReosMeteorologicModelsCollection::reset()
{
  clearModels();
  addMeteorologicModel( tr( "Model 1" ) );
}

ReosEncodedElement ReosMeteorologicModelsCollection::encode( ReosWatershedTree *watershedTree ) const
{
  ReosEncodedElement element( QStringLiteral( "meteo-model-collection" ) );

  QList<ReosEncodedElement> encodedModels;

  for ( ReosMeteorologicModel *model : mMeteoModels )
    encodedModels.append( model->encode( watershedTree ) );

  element.addListEncodedData( QStringLiteral( "models" ), encodedModels );

  return element;
}

void ReosMeteorologicModelsCollection::decode(
  const ReosEncodedElement &element,
  ReosWatershedTree *watershedTree,
  ReosRainfallRegistery *rainfallregistery )
{
  clearModels();

  if ( element.description() != QStringLiteral( "meteo-model-collection" ) )
    return;

  QList<ReosEncodedElement> encodedModels = element.getListEncodedData( QStringLiteral( "models" ) );

  for ( const ReosEncodedElement &elem : encodedModels )
    mMeteoModels.append( new ReosMeteorologicModel( elem, watershedTree, rainfallregistery, this ) );

  for ( ReosMeteorologicModel *model : std::as_const( mMeteoModels ) )
    connect( model, &ReosMeteorologicModel::dataChanged, this, &ReosMeteorologicModelsCollection::changed );

}

ReosMeteorologicStructureItemModel::ReosMeteorologicStructureItemModel( ReosHydraulicNetwork *hydraulicNetwork, QObject *parent )
  : QAbstractListModel( parent )
  , mNetwork( hydraulicNetwork )
{

  connect( mNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosMeteorologicStructureItemModel::onHydraulicNetworkElementAddedRemoved );
  connect( mNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosMeteorologicStructureItemModel::onHydraulicNetworkElementAddedRemoved );

  onHydraulicNetworkElementAddedRemoved();
}

int ReosMeteorologicStructureItemModel::rowCount( const QModelIndex & ) const
{
  return mStructures.count();
}

int ReosMeteorologicStructureItemModel::columnCount( const QModelIndex & ) const
{
  return 2;
}

QVariant ReosMeteorologicStructureItemModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  int row = index.row();
  if ( row < 0 || row >= mStructures.count() )
    return QVariant();

  ReosHydraulicStructure2D *struct2D = mStructures.at( index.row() );

  if ( !struct2D )
    return QVariant();

  if ( index.column() == 0 )
  {
    if ( role == Qt::DisplayRole )
      return struct2D->elementNameParameter()->value();

    if ( role == Qt::DecorationRole )
      return QIcon( QStringLiteral( ":/images/hydraulicStructure2D.svg" ) );
  }

  if ( !mCurrentMeteoModel )
    return QVariant();

  if ( index.column() == 1 )
  {
    ReosGriddedRainItem *item = mCurrentMeteoModel->associatedRainfallItem( struct2D );
    if ( role == Qt::DisplayRole )
      if ( item )
        return item->name();

    if ( role == Qt::DecorationRole )
      if ( item )
        return QIcon( QStringLiteral( ":/images/griddedRainfall.svg" ) );
  }

  return QVariant();
}

QVariant ReosMeteorologicStructureItemModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( section > 2 )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    if ( section == 0 )
      return tr( "Hydraulic Structure" );
    if ( section == 1 )
      return tr( "Associated rainfall" );
  }

  return QVariant();
}

bool ReosMeteorologicStructureItemModel::canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const
{
  ReosRainfallDataItem   *item = rainfallDataInRainfallModel( data->text() );
  if ( !item || !parent.isValid() )
    return false;

  if ( item->dataType() != ReosGriddedRainfall::staticType() )
    return false;

  return true;
}

bool ReosMeteorologicStructureItemModel::dropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent )
{
  if ( !mCurrentMeteoModel )
    return false;

  ReosRainfallDataItem   *item = rainfallDataInRainfallModel( data->text() );
  if ( !item || !parent.isValid() )
    return false;

  if ( item->dataType() != ReosGriddedRainfall::staticType() )
    return false;

  ReosGriddedRainItem *griddedItem = qobject_cast<ReosGriddedRainItem *>( item );

  int strucIndex = parent.row();
  if ( strucIndex < 0 || strucIndex > mStructures.count() )
    return false;

  mCurrentMeteoModel->associate( mStructures.at( strucIndex ), griddedItem );
  emit dataChanged( parent, parent );
  return true;
}

Qt::ItemFlags ReosMeteorologicStructureItemModel::flags( const QModelIndex &index ) const
{
  return  QAbstractItemModel::flags( index ) | Qt::ItemIsDropEnabled;
}

QStringList ReosMeteorologicStructureItemModel::mimeTypes() const
{
  QStringList ret;
  ret << QStringLiteral( "text/plain" );
  return ret;
}

void ReosMeteorologicStructureItemModel::setCurrentMeteoModel( ReosMeteorologicModel *newCurrentMeteoModel )
{
  beginResetModel();
  mCurrentMeteoModel = newCurrentMeteoModel;
  endResetModel();
}

QModelIndex ReosMeteorologicStructureItemModel::structureToIndex( ReosHydraulicStructure2D *structure ) const
{
  return createIndex( mStructures.indexOf( structure ), 0 );
}

void ReosMeteorologicStructureItemModel::removeAssociation( const QModelIndex &index )
{
  if ( !mCurrentMeteoModel )
    return;

  if ( !index.isValid() )
    return;

  ReosHydraulicStructure2D *str = mStructures.at( index.row() );
  if ( str )
  {
    mCurrentMeteoModel->disassociate( str );
    emit dataChanged( index, index.siblingAtColumn( 1 ) );
  }
}

Qt::DropActions ReosMeteorologicStructureItemModel::supportedDropActions() const
{
  return Qt::CopyAction | Qt::MoveAction ;
}

void ReosMeteorologicStructureItemModel::onHydraulicNetworkElementAddedRemoved()
{
  beginResetModel();

  mStructures.clear();
  const QList<ReosHydraulicNetworkElement *> elems = mNetwork->hydraulicNetworkElements( ReosHydraulicStructure2D::staticType() );

  for ( ReosHydraulicNetworkElement *elemStruct2D : elems )
    if ( ReosHydraulicStructure2D *struct2D = qobject_cast<ReosHydraulicStructure2D *>( elemStruct2D ) )
      if ( struct2D->hasCapability( ReosHydraulicStructure2D::Structure2DCapability::GriddedPrecipitation ) )
        mStructures.append( struct2D );

  std::sort( mStructures.begin(), mStructures.end(), []( ReosHydraulicStructure2D * elem1, ReosHydraulicStructure2D * elem2 )->bool
  {
    if ( elem1 && elem2 )
      return elem1->elementNameParameter()->value() < elem2->elementNameParameter()->value();

    return false;
  } );

  endResetModel();
}
