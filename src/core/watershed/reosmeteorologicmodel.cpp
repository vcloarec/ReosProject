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

ReosMeteorologicModel::ReosMeteorologicModel( const QString &name, QObject *parent ):
  ReosDataObject( parent )
  , mName( new ReosParameterString( QObject::tr( "Meteorologic model name" ), false, nullptr ) )
{
  mName->setValue( name );
  mColor = ReosStyleRegistery::instance()->curveColor();
}

ReosMeteorologicModel::ReosMeteorologicModel( const ReosEncodedElement &element,
    ReosWatershedTree *watershedTree,
    ReosRainfallRegistery *rainfallregistery, QObject *parent ):
  ReosDataObject( parent )
  , mName( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, QObject::tr( "Meteorologic model name" ), nullptr ) )
{
  ReosDataObject::decode( element );

  if ( element.description() != QStringLiteral( "meteorologic-configuration" ) )
    return;

  QMap<QString, QString> associations;
  element.getData( QStringLiteral( "associations" ), associations );
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
      mAssociations.append( {QPointer<ReosWatershed>( ws ), QPointer<ReosRainfallDataItem>( rainfall ), nullptr} );
  }
}

ReosMeteorologicModel *ReosMeteorologicModel::duplicate( const QString &dupplicateName )
{
  std::unique_ptr<ReosMeteorologicModel> duplicate = std::make_unique<ReosMeteorologicModel>( dupplicateName );
  duplicate->mAssociations = mAssociations;
  return duplicate.release();
}

ReosParameterString *ReosMeteorologicModel::name() const
{
  return mName.get();
}

ReosEncodedElement ReosMeteorologicModel::encode( ReosWatershedTree *watershedTree ) const
{
  QMap<QString, QString> associations;
  for ( const WatershedRainfallAssociation &association : std::as_const( mAssociations ) )
  {
    if ( association.watershed.isNull() || association.rainfallDataItem.isNull() )
      continue;
    const QString watershedUri = watershedTree->watershedUri( association.watershed );
    const QString rainfallUid = association.rainfallDataItem->uniqueId();
    associations[watershedUri] = rainfallUid;
  }

  ReosEncodedElement element( QStringLiteral( "meteorologic-configuration" ) );

  element.addData( QStringLiteral( "associations" ), associations );
  element.addEncodedData( QStringLiteral( "name" ), mName->encode() );
  element.addData( QStringLiteral( "color" ), mColor );

  ReosDataObject::encode( element );

  return element;
}

void ReosMeteorologicModel::setColor( const QColor &color )
{
  mColor = color;
  emit colorChange( color );
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
    mAssociations.append( {QPointer<ReosWatershed>( watershed ), QPointer<ReosRainfallDataItem>( rainfall ), nullptr} );

  purge();

  emit dataChanged();
}

void ReosMeteorologicModel::disassociate( ReosWatershed *watershed )
{
  int index = findWatershed( watershed );

  if ( index >= 0 )
    mAssociations.removeAt( index );

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

void ReosMeteorologicModel::purge() const
{
  int i = 0;
  while ( i < mAssociations.count() )
  {
    if ( mAssociations.at( i ).watershed.isNull() || mAssociations.at( i ).rainfallDataItem.isNull() )
      mAssociations.removeAt( i );
    else
      ++i;
  }
}

QColor ReosMeteorologicModel::color() const
{
  return mColor;
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

ReosRainfallDataItem *ReosMeteorologicItemModel::rainfallDataInRainfallModel( const QString &uri ) const
{
  if ( ReosRainfallRegistery::isInstantiate() )
    return  qobject_cast<ReosRainfallDataItem *>( ReosRainfallRegistery::instance()->itemByUri( uri ) ) ;

  return nullptr;
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
  addMeteorologicModel( tr( "Meteorological Model" ) );
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

void ReosMeteorologicModelsCollection::decode( const ReosEncodedElement &element, ReosWatershedTree *watershedTree, ReosRainfallRegistery *rainfallregistery )
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
