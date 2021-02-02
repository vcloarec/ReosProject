/***************************************************************************
  reosrainfallmodel.cpp - ReosRainfallModel

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
#include "reosrainfallmodel.h"

#include <QFile>
#include <QFileInfo>
#include <QMimeData>
#include <QStack>
#include <QTime>
#include <QElapsedTimer>

#include "reosparameter.h"
#include "reosrainfallitem.h"

ReosRainfallModel::ReosRainfallModel( QObject *parent ):
  QAbstractItemModel( parent )
  , mRootZone( new ReosRootItem() )
{

}

QModelIndex ReosRainfallModel::index( int row, int column, const QModelIndex &parent ) const
{
  ReosRainfallItem *parentItem = nullptr;

  if ( parent.isValid() )
  {
    parentItem = indexToItem( parent );
  }
  else
  {
    parentItem = mRootZone.get();
  }

  if ( ! parentItem || row >= parentItem->childrenCount() )
    return QModelIndex();

  return createIndex( row, column, parentItem->itemAt( row ) );
}

QModelIndex ReosRainfallModel::parent( const QModelIndex &child ) const
{
  if ( !child.isValid() )
    return QModelIndex();

  ReosRainfallItem *childItem = indexToItem( child );

  if ( !childItem || childItem->parentItem() == nullptr || childItem->parentItem() == mRootZone.get() )
    return QModelIndex();

  return itemToIndex( childItem->parentItem() );
}

int ReosRainfallModel::rowCount( const QModelIndex &parent ) const
{
  if ( parent.isValid() )
  {
    ReosRainfallItem *parentItem = indexToItem( parent );
    if ( parentItem )
      return parentItem->childrenCount();
  }

  return static_cast<int>( mRootZone->childrenCount() );
}

int ReosRainfallModel::columnCount( const QModelIndex & ) const
{
  return 2;
}

QVariant ReosRainfallModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  ReosRainfallItem *item = indexToItem( index );
  if ( !item || item == mRootZone.get() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    if ( index.column() == 0 )
      return item->name();

    if ( index.column() == 1 )
      return item->description();

  }

  if ( role == Qt::DecorationRole && index.column() == 0 )
  {
    return item->icone();
  }

  return QVariant();
}

QVariant ReosRainfallModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    if ( section == 0 )
      return tr( "Name" );

    if ( section == 1 )
      return tr( "Description" );
  }

  return QVariant();
}

Qt::ItemFlags ReosRainfallModel::flags( const QModelIndex &index ) const
{
  return QAbstractItemModel::flags( index ) | Qt::ItemIsDragEnabled | Qt::ItemIsDropEnabled;
}

bool ReosRainfallModel::canDropMimeData( const QMimeData *data, Qt::DropAction, int row, int, const QModelIndex &parent ) const
{
  return true;
  QElapsedTimer timer;
  timer.start();
  QByteArray ba = data->data( QStringLiteral( "lekan/rainfallItem_position_path" ) );
  QDataStream stream( ba );

  QList<int> positionPath;
  stream >> positionPath;

  ReosRainfallItem *item = positonPathToItem( positionPath );
  ReosRainfallItem *receiver = indexToItem( parent );

  if ( item == receiver )
  {
    return false;
  }

  if ( receiver == nullptr )
    receiver = mRootZone.get();

  if ( !item || !receiver || receiver->isSubItem( item ) )
  {
    qDebug() << timer.elapsed();
    return false;
  }

  qDebug() << timer.elapsed();
  return receiver->accept( item );
}

bool ReosRainfallModel::dropMimeData( const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent )
{
//  QByteArray ba = data->data( QStringLiteral( "lekan/rainfallItem_position_path" ) );
//  QDataStream stream( ba );

//  QList<int> positionPath;
//  stream >> positionPath;

//  ReosRainfallItem *item = positonPathToItem( positionPath );
//  ReosRainfallItem *receiver = indexToItem( parent );
//  if ( receiver == nullptr )
//    receiver = mRootZone.get();

//  if ( !item || !receiver || receiver->isSubItem( item ) )
//    return false;

//  if ( row == -1 )
//    row = receiver->itemCount();
//  ReosRainfallItem *oldParent = item->parent();
//  beginMoveRows( itemToIndex( oldParent ), item->positionInParent(), item->positionInParent(), parent, row );
//  receiver->insertChild( row, oldParent->takeChild( item->positionInParent() ) );
//  endMoveRows();

  return false;

  return true;
}


QMimeData *ReosRainfallModel::mimeData( const QModelIndexList &indexes ) const
{
  std::unique_ptr<QMimeData> mimeData = std::make_unique<QMimeData>();
  if ( indexes.count() == 0 )
    return mimeData.release();

  ReosRainfallItem *item = indexToItem( indexes.at( 0 ) );
  if ( !item )
    return mimeData.release();

  QByteArray data;
  QDataStream stream( &data, QIODevice::WriteOnly );

  stream << item->positionPathInTree();
  mimeData->setData( QStringLiteral( "lekan/rainfallItem_position_path" ), data );

  return mimeData.release();
}

ReosZoneItem *ReosRainfallModel::addZone( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    receiver = mRootZone.get();
  else
  {
    if ( receiver->type() != ReosRainfallItem::Zone )
      return nullptr;
  }

  std::unique_ptr<ReosZoneItem> newZone = std::make_unique<ReosZoneItem>( name, description );
  if ( receiver->accept( newZone.get() ) )
    return static_cast<ReosZoneItem *>( addItem( receiver, newZone.release() ) );

  return nullptr;

}

ReosStationItem *ReosRainfallModel::addStation( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Zone )
      return nullptr;
  }

  std::unique_ptr<ReosStationItem> newStation = std::make_unique<ReosStationItem>( name, description );

  if ( ! receiver->accept( newStation.get() ) )
    return nullptr;

  return static_cast<ReosStationItem *>( addItem( receiver, newStation.release() ) );
}

ReosRainfallSeriesItem *ReosRainfallModel::addGaugedRainfall( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Station )
      return nullptr;
  }

  std::unique_ptr<ReosRainfallSeriesItem> newRainfal = std::make_unique<ReosRainfallSeriesItem>( name, description );

  if ( ! receiver->accept( newRainfal.get() ) )
    return nullptr;

  return static_cast<ReosRainfallSeriesItem *>( addItem( receiver, newRainfal.release() ) );
}

void ReosRainfallModel::removeItem( ReosRainfallItem *item )
{
  ReosRainfallItem *parentItem = item->parentItem();

  beginRemoveRows( itemToIndex( parentItem ), item->positionInParent(), item->positionInParent() );
  parentItem->removeItem( item );
  endRemoveRows();
}

int ReosRainfallModel::rootZoneCount() const {return mRootZone->childrenCount();}

QModelIndex ReosRainfallModel::itemToIndex( ReosRainfallItem *item ) const
{
  if ( item == nullptr || item == mRootZone.get() )
    return QModelIndex();

  ReosRainfallItem *parentItem = item->parentItem();
  int row = -1;

  QModelIndex parentIndex;
  if ( parentItem == mRootZone.get() )
    parentIndex = QModelIndex();
  else
    parentIndex = itemToIndex( parentItem );

  for ( int i = 0; i < parentItem->childrenCount(); ++i )
  {
    if ( item == parentItem->itemAt( i ) )
    {
      row = i;
      break;
    }
  }

  if ( row >= 0 )
    return index( row, 0, parentIndex );
  else
    return QModelIndex();
}

ReosRainfallItem *ReosRainfallModel::indexToItem( const QModelIndex &index ) const
{
  if ( !index.isValid() )
    return nullptr;

  return static_cast<ReosRainfallItem *>( index.internalPointer() );
}

ReosRainfallItem *ReosRainfallModel::positonPathToItem( const QList<int> &path ) const
{
  ReosRainfallItem *item = mRootZone.get();
  for ( int pos : qAsConst( path ) )
  {
    if ( pos < 0 || pos >= item->childrenCount() )
      return nullptr;
    item = item->itemAt( pos );
  }

  return item;
}

ReosEncodedElement ReosRainfallModel::encode() const
{
  ReosEncodedElement element( QStringLiteral( "rainfall-data" ) );
  if ( mRootZone )
    element.addEncodedData( QStringLiteral( "rainfall-tree" ), mRootZone->encode() );

  return element;
}

bool ReosRainfallModel::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "rainfall-data" ) )
    return false;

  beginResetModel();
  if ( mRootZone )
    mRootZone->clear();

  if ( !element.hasEncodedData( QStringLiteral( "rainfall-tree" ) ) )
  {
    endResetModel();
    return false;
  }

  ReosEncodedElement encodedRootItem = element.getEncodedData( QStringLiteral( "rainfall-tree" ) );
  if ( !encodedRootItem.hasEncodedData() || encodedRootItem.description() != QStringLiteral( "root-item" ) )
  {
    endResetModel();
    return false;
  }

  mRootZone.reset( new ReosRootItem( encodedRootItem ) );
  connectItem( mRootZone.get() );
  endResetModel();
  return true;

}

bool ReosRainfallModel::saveToFile( const QString &path, const QString &header )
{
  QFileInfo fileInfo( path );

  if ( fileInfo.exists() )
  {
    QFile file( path );
    file.copy( path + QStringLiteral( ".bck" ) );
  }

  QFile file( path );

  QDataStream stream( &file );
  if ( file.open( QIODevice::WriteOnly ) )
  {
    stream << header;
    stream << encode().bytes();
    return true;
  }

  return false;
}

bool ReosRainfallModel::loadFromFile( const QString &path, const QString &header )
{
  Q_UNUSED( header );

  QFileInfo fileInfo( path );

  if ( !fileInfo.exists() )
    return false;

  QFile file( path );
  QDataStream stream( &file );

  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QString readenHeader;
  stream >> readenHeader;

  QByteArray data;
  stream >> data;

  ReosEncodedElement dataElement( data );

  return decode( dataElement );
}

void ReosRainfallModel::onItemChanged( ReosRainfallItem *item )
{
  QModelIndex ind = itemToIndex( item );
  if ( ind.isValid() )
  {
    dataChanged( ind, index( ind.row(), 1, ind.parent() ) );
  }

}

ReosRainfallItem *ReosRainfallModel::addItem( ReosRainfallItem *receiver, ReosRainfallItem *newItem )
{
  std::unique_ptr<ReosRainfallItem> item( newItem );
  QModelIndex index = itemToIndex( receiver );
  beginInsertRows( index, receiver->childrenCount(), receiver->childrenCount() );
  ReosRainfallItem *ret = receiver->addItem( item.release() );
  endInsertRows();

  connectItem( ret );
  return ret;
}

void ReosRainfallModel::connectItem( ReosRainfallItem *item )
{
  QStack<ReosRainfallItem *> itemToConnect;

  itemToConnect.push( item );

  while ( !itemToConnect.isEmpty() )
  {
    ReosRainfallItem *currentItem = itemToConnect.pop();
    connect( currentItem, &ReosRainfallItem::changed, this, &ReosRainfallModel::onItemChanged );

    for ( int i = 0; i < currentItem->childrenCount(); ++i )
      itemToConnect.push( currentItem->itemAt( i ) );
  }

}


