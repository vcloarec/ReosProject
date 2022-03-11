/***************************************************************************
  reostopographycollection.cpp - ReosTopographyCollection

 ---------------------
 begin                : 28.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostopographycollection.h"

#include <QMimeData>

#include "reostopographycollection_p.h"
#include "reosgisengine.h"
#include "reosparameter.h"

ReosTopographyCollection *ReosTopographyCollection::createTopographyCollection( ReosGisEngine *gisEngine, QObject *parent )
{
  return new ReosTopographyCollection_p( gisEngine, parent );
}

ReosTopographyCollection *ReosTopographyCollection::createTopographyCollection( const ReosEncodedElement &element, ReosGisEngine *gisEngine, QObject *parent )
{
  return new ReosTopographyCollection_p( element, gisEngine, parent );
}

int ReosTopographyCollection::topographyCount() const
{
  return mTopographyIds.count();
}

QString ReosTopographyCollection::topographyName( int index ) const
{
  return mGisEngine->layerName( mTopographyIds.at( index ) );
}


QString ReosTopographyCollection::topographyId( int index ) const
{
  return mTopographyIds.at( index );
}

void ReosTopographyCollection::insertTopography( int index, const QString &topographyId )
{
  mTopographyIds.insert( index, topographyId );
  emit dataChanged();
}

void ReosTopographyCollection::removeTopography( int index )
{
  mTopographyIds.removeAt( index );
  emit dataChanged();
}

void ReosTopographyCollection::removeTopography( const QString &topographyId )
{
  if ( mTopographyIds.contains( topographyId ) )
  {
    mTopographyIds.removeOne( topographyId );
    emit dataChanged();
  }
}

void ReosTopographyCollection::moveTopoGraphyTo( const QString &topographyId, int destIndex )
{
  int originIndex = mTopographyIds.indexOf( topographyId );
  mTopographyIds.move( originIndex, destIndex );
  emit dataChanged();
}

bool ReosTopographyCollection::contains( const QString &topographyId ) const
{
  return mTopographyIds.contains( topographyId );
}

QPixmap ReosTopographyCollection::icon( const QString &layerId ) const
{
  ReosGisEngine::LayerType layerType = mGisEngine->layerType( layerId );

  switch ( layerType )
  {
    case ReosGisEngine::NoLayer:
      break;
    case ReosGisEngine::VectorLayer:
      break;
    case ReosGisEngine::RasterLayer:
      return QPixmap( QStringLiteral( ":/images/layerRaster.svg" ) );
      break;
    case ReosGisEngine::MeshLayer:
      break;
    case ReosGisEngine::NotSupported:
      break;
  }

  return QPixmap();
}

ReosEncodedElement ReosTopographyCollection::encode() const
{
  ReosEncodedElement element( QStringLiteral( "topography-collection" ) );

  element.addData( QStringLiteral( "topography-id" ), mTopographyIds );
  element.addEncodedData( QStringLiteral( "auto-apply" ), mAutoApply->encode() );
  return element;
}

ReosTopographyCollection::ReosTopographyCollection( ReosGisEngine *gisEngine, QObject *parent )
  : ReosDataObject( parent )
  , mGisEngine( gisEngine )
  , mAutoApply( new ReosParameterBoolean( tr( "Auto apply topographies" ), false, this ) )
{
  mAutoApply->setValue( false );
  connect( gisEngine, &ReosGisEngine::layerRemoved, this, [this]( const QString & layerId )
  {
    if ( mTopographyIds.contains( layerId ) )
    {
      mTopographyIds.removeOne( layerId );
      emit dataChanged();
    }
  } );
}

ReosTopographyCollection::ReosTopographyCollection( const ReosEncodedElement &element, ReosGisEngine *gisEngine, QObject *parent )
  : ReosTopographyCollection( gisEngine, parent )
{
  mAutoApply = ReosParameterBoolean::decode( element.getEncodedData( QStringLiteral( "auto-apply" ) ), false, tr( "Auto apply topographies" ), this );
  if ( !mAutoApply->isValid() )
    mAutoApply->setValue( false );

  if ( element.description() == QStringLiteral( "topography-collection" ) )
  {
    element.getData( "topography-id", mTopographyIds );

    int i = 0;
    while ( i < mTopographyIds.count() )
    {
      if ( !mGisEngine->hasValidLayer( mTopographyIds.at( i ) ) )
        mTopographyIds.removeAt( i );
      else
        ++i;
    }
  }
}

ReosParameterBoolean *ReosTopographyCollection::autoApply() const
{
  return mAutoApply;
}

ReosTopographyCollectionListModel::ReosTopographyCollectionListModel( ReosTopographyCollection *collection, QObject *parent )
  : QAbstractListModel( parent )
  , mCollection( collection )
{
  connect( collection, &ReosDataObject::dataChanged, this, &ReosTopographyCollectionListModel::onCollectionChanged );
}

QModelIndex ReosTopographyCollectionListModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTopographyCollectionListModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTopographyCollectionListModel::rowCount( const QModelIndex & ) const
{
  return mCollection->topographyCount();
}

int ReosTopographyCollectionListModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosTopographyCollectionListModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() < 0 || index.row() >= mCollection->topographyCount() )
    return QVariant();

  if ( role == Qt::DisplayRole )
    return mCollection->topographyName( index.row() );

  if ( role == Qt::DecorationRole )
    return mCollection->icon( mCollection->topographyId( index.row() ) );

  return QVariant();
}

Qt::ItemFlags ReosTopographyCollectionListModel::flags( const QModelIndex &index ) const
{
  if ( index.isValid() )
    return Qt::ItemIsDropEnabled | Qt::ItemIsDragEnabled | QAbstractListModel::flags( index );
  else
    return Qt::ItemIsDropEnabled | QAbstractListModel::flags( index );
}

QStringList ReosTopographyCollectionListModel::mimeTypes() const
{
  QStringList types;
  types << QStringLiteral( "application/vnd.text.list" );
  return types;
}

QMimeData *ReosTopographyCollectionListModel::mimeData( const QModelIndexList &indexes ) const
{
  QMimeData *mimeData = new QMimeData();
  QByteArray encodedData;

  QDataStream stream( &encodedData, QIODevice::WriteOnly );

  for ( const QModelIndex &index : indexes )
  {
    if ( index.isValid() )
    {
      QString text = mCollection->topographyId( index.row() );
      stream << text;
    }
  }

  mimeData->setData( QStringLiteral( "application/vnd.text.list" ), encodedData );
  return mimeData;
}

Qt::DropActions ReosTopographyCollectionListModel::supportedDropActions() const
{
  return Qt::MoveAction;
}

Qt::DropActions ReosTopographyCollectionListModel::supportedDragActions() const
{
  return Qt::MoveAction;
}

bool ReosTopographyCollectionListModel::canDropMimeData( const QMimeData *data, Qt::DropAction action, int, int, const QModelIndex &parent ) const
{
  return ( !parent.isValid() && action == Qt::MoveAction && data->hasFormat( QStringLiteral( "application/vnd.text.list" ) ) );
}

bool ReosTopographyCollectionListModel::dropMimeData( const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent )
{
  if ( parent.isValid() || action != Qt::MoveAction || !data->hasFormat( QStringLiteral( "application/vnd.text.list" ) ) )
    return false;

  QByteArray encodedData = data->data( "application/vnd.text.list" );
  QDataStream stream( &encodedData, QIODevice::ReadOnly );
  QString id;
  stream >> id;

  mCollection->removeTopography( id );
  mCollection->insertTopography( row, id );
  return true;
}

void ReosTopographyCollectionListModel::onCollectionChanged()
{
  beginResetModel();
  endResetModel();
}
