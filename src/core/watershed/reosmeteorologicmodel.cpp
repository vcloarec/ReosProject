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

ReosMeteorologicModel::ReosMeteorologicModel( const QString &name, QObject *parent ):
  ReosDataObject( parent )
  , mName( new ReosParameterString( QObject::tr( "Meteorologic model name" ), false, nullptr ) )
{
  mName->setValue( name );
}

ReosMeteorologicModel::ReosMeteorologicModel( const ReosEncodedElement &element,
    ReosWatershedTree *watershedTree,
    ReosRainfallRegistery *rainfallregistery, QObject *parent ):
  ReosDataObject( parent )
  , mName( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, nullptr ) )
{
  if ( element.description() != QStringLiteral( "meteorologic-configuration" ) )
    return;

  QMap<QString, QString> associations;
  element.getData( QStringLiteral( "associations" ), associations );

  for ( const QString &watershedUri : associations.keys() )
  {
    ReosWatershed *ws = watershedTree->uriToWatershed( watershedUri );
    ReosRainfallSerieRainfallItem *rainfall = qobject_cast<ReosRainfallSerieRainfallItem *>
        ( rainfallregistery->itemByUniqueId( associations.value( watershedUri ) ) );

    if ( ws && rainfall )
      mAssociations.append( {QPointer<ReosWatershed>( ws ), QPointer<ReosRainfallSerieRainfallItem>( rainfall )} );
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
  for ( const WatershedRainfallAssociation &association : mAssociations )
  {
    if ( association.first.isNull() || association.second.isNull() )
      continue;
    QString watershedUri = watershedTree->watershedUri( association.first );
    QString rainfallUid = association.second->uniqueId();
    associations[watershedUri] = rainfallUid;
  }

  ReosEncodedElement element( QStringLiteral( "meteorologic-configuration" ) );

  element.addData( QStringLiteral( "associations" ), associations );
  element.addEncodedData( QStringLiteral( "name" ), mName->encode() );

  return element;
}

void ReosMeteorologicModel::associate( ReosWatershed *watershed, ReosRainfallSerieRainfallItem *rainfall )
{
  int index = findWatershed( watershed );

  if ( index >= 0 )
    mAssociations[index].second = rainfall;
  else
    mAssociations.append( {QPointer<ReosWatershed>( watershed ), QPointer<ReosRainfallSerieRainfallItem>( rainfall )} );

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

ReosRainfallSerieRainfallItem *ReosMeteorologicModel::associatedRainfall( ReosWatershed *watershed ) const
{
  int watershedIndex = findWatershed( watershed );

  if ( watershedIndex >= 0 )
    return mAssociations.at( watershedIndex ).second;
  else
    return nullptr;
}

int ReosMeteorologicModel::findWatershed( ReosWatershed *watershed ) const
{
  for ( int i = 0; i < mAssociations.count(); ++i )
  {
    if ( !mAssociations.at( i ).first.isNull() && mAssociations.at( i ).first == watershed )
      return i;
  }

  return -1;
}

void ReosMeteorologicModel::purge() const
{
  int i = 0;
  while ( i < mAssociations.count() )
  {
    if ( mAssociations.at( i ).first.isNull() || mAssociations.at( i ).second.isNull() )
      mAssociations.removeAt( i );
    else
      ++i;
  }
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
    ReosRainfallSerieRainfallItem *rainfall = mCurrentMeteoModel->associatedRainfall( ws );
    if ( role == Qt::DisplayRole )
    {
      if ( rainfall )
        return rainfall->name();
    }

    if ( role == Qt::DecorationRole )
    {
      if ( rainfall )
        return rainfall->icone();
    }
  }

  return QVariant();

}

bool ReosMeteorologicItemModel::canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const
{
  if ( rainfallInRainfallModel( data->text() ) && parent.isValid() )
    return true;

  return false;
}

bool ReosMeteorologicItemModel::dropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent )
{
  ReosRainfallSerieRainfallItem *rainfall = rainfallInRainfallModel( data->text() );

  if ( rainfall && parent.isValid() && mCurrentMeteoModel )
  {
    ReosWatershed *ws = mWatershedModel->indexToWatershed( mapToSource( parent ) );
    if ( ws )
    {
      mCurrentMeteoModel->associate( ws, rainfall );
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

ReosRainfallSerieRainfallItem *ReosMeteorologicItemModel::rainfallInRainfallModel( const QString &uri ) const
{
  if ( ReosRainfallRegistery::isInstantiate() )
    return qobject_cast<ReosRainfallSerieRainfallItem *>( ReosRainfallRegistery::instance()->itemByUri( uri ) );

  return nullptr;
}

ReosRainfallSerieRainfallItem *ReosMeteorologicItemModel::rainfallInMeteorologicModel( const QModelIndex &index )
{
  if ( !mCurrentMeteoModel )
    return nullptr;
  ReosWatershed *ws = mWatershedModel->indexToWatershed( mapToSource( index ) );
  return mCurrentMeteoModel->associatedRainfall( ws );
}

ReosMeteorologicModelsCollection::ReosMeteorologicModelsCollection()
{
  addMeteorologicModel( tr( "Meteorological Model" ) );
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

int ReosMeteorologicModelsCollection::modelCount() const
{
  return static_cast<int>( mMeteoModels.size() );
}

void ReosMeteorologicModelsCollection::addMeteorologicModel( const QString &name )
{
  beginInsertRows( QModelIndex(), modelCount(), modelCount() );
  mMeteoModels.append( new ReosMeteorologicModel( name, this ) );
  endInsertRows();
}

void ReosMeteorologicModelsCollection::addMeteorologicModel( ReosMeteorologicModel *model )
{
  beginInsertRows( QModelIndex(), modelCount(), modelCount() );
  mMeteoModels.append( model );
  model->setParent( this );
  endInsertRows();
}

void ReosMeteorologicModelsCollection::removeMeteorologicModel( int i )
{
  beginRemoveRows( QModelIndex(), i, i );
  mMeteoModels.takeAt( i )->deleteLater();
  endRemoveRows();
}

void ReosMeteorologicModelsCollection::clearModels()
{
  while ( mMeteoModels.isEmpty() )
    mMeteoModels.takeAt( 0 )->deleteLater();
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

}
