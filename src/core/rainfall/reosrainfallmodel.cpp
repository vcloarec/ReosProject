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
#include "reosgriddedrainitem.h"
#include "reosversion.h"

#define FILE_MAGIC_NUMBER  1909201401

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

bool ReosRainfallModel::canDropMimeData( const QMimeData *data, Qt::DropAction, int, int, const QModelIndex &parent ) const
{
  ReosRainfallItem *item = uriToItem( data->text() );
  ReosRainfallItem *receiver = indexToItem( parent );

  if ( item == receiver )
  {
    return false;
  }

  if ( receiver == nullptr )
    receiver = mRootZone.get();

  if ( !item || !receiver )
  {
    return false;
  }

  return receiver->accept( item, true );
}

bool ReosRainfallModel::dropMimeData( const QMimeData *data, Qt::DropAction,  int row, int, const QModelIndex &parent )
{
  ReosRainfallItem *item = uriToItem( data->text() );
  ReosRainfallItem *receiver = indexToItem( parent );
  if ( receiver == nullptr )
    receiver = mRootZone.get();

  if ( !item || !receiver )
    return false;

  if ( row == -1 )
    row = receiver->childrenCount();
  ReosRainfallItem *oldParent = item->parentItem();
  int positionInOldParent = item->positionInParent();

  if ( receiver == oldParent && row > positionInOldParent )
    row--;
  receiver->insertChild( row, oldParent->takeChild( item->positionInParent() ) );

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

  mimeData->setText( item->uri() );

  return mimeData.release();
}

QStringList ReosRainfallModel::mimeTypes() const
{
  QStringList ret;
  ret << QStringLiteral( "text/plain" );
  return ret;
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

ReosStationItem *ReosRainfallModel::addStation( const QString &name, const QString &description, const QModelIndex &index, const ReosSpatialPosition &position )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Zone )
      return nullptr;
  }

  std::unique_ptr<ReosStationItem> newStation = std::make_unique<ReosStationItem>( name, description, position );

  if ( ! receiver->accept( newStation.get() ) )
    return nullptr;

  return static_cast<ReosStationItem *>( addItem( receiver, newStation.release() ) );
}

ReosRainfallGaugedRainfallItem *ReosRainfallModel::addGaugedRainfall( const QString &name, const QString &description, const QModelIndex &index, ReosSeriesRainfall *data )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Station )
      return addGaugedRainfall( name, description, index.parent(), data );
  }

  std::unique_ptr<ReosRainfallGaugedRainfallItem> newRainfal = std::make_unique<ReosRainfallGaugedRainfallItem>( name, description, data );

  if ( ! receiver->accept( newRainfal.get() ) )
    return nullptr;

  return static_cast<ReosRainfallGaugedRainfallItem *>( addItem( receiver, newRainfal.release() ) );
}

ReosRainfallChicagoItem *ReosRainfallModel::addChicagoRainfall( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Station )
      return addChicagoRainfall( name, description, index.parent() );
  }

  std::unique_ptr<ReosRainfallChicagoItem> newRainfal = std::make_unique<ReosRainfallChicagoItem>( name, description );

  if ( ! receiver->accept( newRainfal.get() ) )
    return nullptr;

  return static_cast<ReosRainfallChicagoItem *>( addItem( receiver, newRainfal.release() ) );
}

ReosRainfallAlternatingBlockItem *ReosRainfallModel::addAlternatingBlockRainfall( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Station )
      return addAlternatingBlockRainfall( name, description, index.parent() );
  }

  std::unique_ptr<ReosRainfallAlternatingBlockItem> newRainfal = std::make_unique<ReosRainfallAlternatingBlockItem>( name, description );

  if ( ! receiver->accept( newRainfal.get() ) )
    return nullptr;

  return static_cast<ReosRainfallAlternatingBlockItem *>( addItem( receiver, newRainfal.release() ) );
}

ReosRainfallDoubleTriangleItem *ReosRainfallModel::addDoubleTriangleRainfall( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Station )
      return addDoubleTriangleRainfall( name, description, index.parent() );
  }

  std::unique_ptr<ReosRainfallDoubleTriangleItem> newRainfal = std::make_unique<ReosRainfallDoubleTriangleItem>( name, description );

  if ( ! receiver->accept( newRainfal.get() ) )
    return nullptr;

  return static_cast<ReosRainfallDoubleTriangleItem *>( addItem( receiver, newRainfal.release() ) );
}

ReosRainfallIdfCurvesItem *ReosRainfallModel::addIDFCurves( const QString &name, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;
  else
  {
    if ( receiver->type() != ReosRainfallItem::Station )
      return addIDFCurves( name, description, index.parent() );
  }

  std::unique_ptr<ReosRainfallIdfCurvesItem> newIDF = std::make_unique<ReosRainfallIdfCurvesItem>( name, description );

  if ( ! receiver->accept( newIDF.get() ) )
    return nullptr;

  return static_cast<ReosRainfallIdfCurvesItem *>( addItem( receiver, newIDF.release() ) );
}

ReosRainfallIntensityDurationCurveItem *ReosRainfallModel::addIDCurve( const ReosDuration &duration, const QString &description, const QModelIndex &index )
{
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;

  if ( receiver->type() != ReosRainfallItem::Data )
    return nullptr;

  ReosRainfallDataItem *dataItemReceiver = qobject_cast<ReosRainfallDataItem *>( receiver );

  if ( !dataItemReceiver || dataItemReceiver->dataType() != QStringLiteral( "idf-curves" ) )
    return addIDCurve( duration, description, index.parent() );

  ReosRainfallIdfCurvesItem *idfItem = qobject_cast<ReosRainfallIdfCurvesItem *>( dataItemReceiver );
  if ( !idfItem )
    return nullptr;

  std::unique_ptr<ReosRainfallIntensityDurationCurveItem> newID = std::make_unique<ReosRainfallIntensityDurationCurveItem>( duration, QString(), description );

  if ( ! receiver->accept( newID.get() ) )
    return nullptr;

  return static_cast<ReosRainfallIntensityDurationCurveItem *>( idfItem->itemAt( idfItem->placeIdCurveItem( newID.release() ) ) );
}

ReosGriddedRainItem *ReosRainfallModel::addGriddedRainfall( const QString &name, const QString &description, const QModelIndex &index, ReosGriddedRainfall *data )
{
  std::unique_ptr<ReosGriddedRainfall> griddedRainfall( data );
  ReosRainfallItem *receiver = indexToItem( index );
  if ( receiver == nullptr )
    return nullptr;

  std::unique_ptr<ReosGriddedRainItem> item( new ReosGriddedRainItem( name, description, griddedRainfall.release() ) );

  if ( !receiver->accept( item.get() ) )
    return nullptr;

  return qobject_cast<ReosGriddedRainItem *>( addItem( receiver, item.release() ) );
}

void ReosRainfallModel::removeItem( ReosRainfallItem *item )
{
  ReosRainfallItem *parentItem = item->parentItem();

  beginRemoveRows( itemToIndex( parentItem ), item->positionInParent(), item->positionInParent() );
  parentItem->removeItem( item );
  endRemoveRows();
  emit changed();
}

int ReosRainfallModel::rootZoneCount() const
{
  return mRootZone->childrenCount();
}

ReosZoneItem *ReosRainfallModel::rootZone( int i ) const
{
  if ( mRootZone->childrenCount() <= i )
    return nullptr;

  return qobject_cast<ReosZoneItem *>( mRootZone->itemAt( i ) );
}

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

void ReosRainfallModel::swapItems( ReosRainfallItem *parent, int first, int second )
{
  QModelIndex indexFrom;
  QModelIndex indexTo;
  if ( first < second )
  {
    indexFrom = itemToIndex( parent->itemAt( first ) );
    indexTo = itemToIndex( parent->itemAt( second ) );
  }
  else
  {
    indexFrom = itemToIndex( parent->itemAt( second ) );
    indexTo = itemToIndex( parent->itemAt( first ) );
  }
  parent->swapChildren( first, second );
  emit changed();
}

ReosRainfallItem *ReosRainfallModel::positonPathToItem( const QList<int> &path ) const
{
  ReosRainfallItem *item = mRootZone.get();
  for ( int pos : std::as_const( path ) )
  {
    if ( pos < 0 || pos >= item->childrenCount() )
      return nullptr;
    item = item->itemAt( pos );
  }

  return item;
}

ReosRainfallItem *ReosRainfallModel::uriToItem( const QString &uri ) const
{
  QStringList strList = uri.split( ':' );

  QList<int> path;

  for ( const QString &str : std::as_const( strList ) )
    path.append( str.toInt() );

  return positonPathToItem( path );
}

ReosRainfallItem *ReosRainfallModel::uniqueIdToItem( const QString &uid ) const
{
  return mRootZone->searchForChildWithUniqueId( uid );
}

ReosEncodedElement ReosRainfallModel::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "rainfall-data" ) );
  if ( mRootZone )
    element.addEncodedData( QStringLiteral( "rainfall-tree" ), mRootZone->encode( context ) );

  return element;
}

bool ReosRainfallModel::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
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

  mRootZone.reset( new ReosRootItem( encodedRootItem, context ) );
  connectItem( mRootZone.get() );
  endResetModel();
  return true;
}

bool ReosRainfallModel::saveToFile( const QString &path )
{
  QFileInfo fileInfo( path );

  if ( fileInfo.exists() )
  {
    QFile file( path );
    file.copy( path + QStringLiteral( ".bck" ) );
  }

  ReosEncodeContext context;
  context.setBaseDir( fileInfo.dir() );
  QByteArray data = encode( context ).bytes();

  QFile file( path );

  QDataStream stream( &file );

  if ( file.open( QIODevice::WriteOnly ) )
  {
    //**** bytes header
    qint32 magicNumber = FILE_MAGIC_NUMBER;
    qint32 serialisationVersion = stream.version();
    QByteArray versionBytes = ReosVersion::currentApplicationVersion().bytesVersion();

    stream << magicNumber;
    stream << serialisationVersion;
    stream << versionBytes;
    //*****

    stream << data;
    file.close();
    emit saved( path );
    return true;
  }

  return false;
}

bool ReosRainfallModel::loadFromFile( const QString &path )
{

  QFileInfo fileInfo( path );

  if ( !fileInfo.exists() )
    return false;

  QFile file( path );
  QDataStream stream( &file );

  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  ReosVersion version;

  //*** read header
  qint32 magicNumber;
  qint32 serialisationVersion;
  QByteArray bytesVersion;
  stream >> magicNumber;

  if ( magicNumber == FILE_MAGIC_NUMBER )
  {
    // since Lekan 2.2
    stream >> serialisationVersion;
    stream >> bytesVersion;
    QDataStream::Version v = static_cast<QDataStream::Version>( serialisationVersion );
    ReosEncodedElement::setSerialisationVersion( v );
    version = ReosVersion( bytesVersion, v );
  }
  else
  {
    //old version don't have real header but a text header
    ReosEncodedElement::setSerialisationVersion( QDataStream::Qt_5_12 ); /// TODO : check the Qt version of Lekan 2.0 / 2.1
    stream.device()->reset();

    QString readenHeader;
    stream >> readenHeader;
  }

  QByteArray data;
  stream >> data;

  ReosEncodedElement dataElement( data );

  file.close();
  emit loaded( path );

  ReosEncodeContext context;
  context.setEncodeRelativePath( mIsPathRelative );
  context.setBaseDir( fileInfo.dir() );
  return decode( dataElement, context );
}

void ReosRainfallModel::onItemChanged( ReosRainfallItem *item )
{
  QModelIndex ind = itemToIndex( item );
  if ( ind.isValid() )
  {
    dataChanged( ind, index( ind.row(), 1, ind.parent() ) );
  }
  emit changed();
}

void ReosRainfallModel::onItemWillBeRemovedfromParent( ReosRainfallItem *item, int pos )
{
  QModelIndex index = itemToIndex( item );
  //if ( index.isValid() )
  beginRemoveRows( index, pos, pos );
}

void ReosRainfallModel::onItemRemovedfromParent()
{
  endRemoveRows();
  emit changed();
}

void ReosRainfallModel::onItemWillBeInsertedInParent( ReosRainfallItem *item, int pos )
{
  QModelIndex index = itemToIndex( item );
  //if ( index.isValid() )
  beginInsertRows( index, pos, pos );
}

void ReosRainfallModel::onItemInsertedInParent()
{
  endInsertRows();
  emit changed();
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

bool ReosRainfallModel::isPathRelative() const
{
  return mIsPathRelative;
}

void ReosRainfallModel::setIsPathRelative( bool newIsPathRelative )
{
  mIsPathRelative = newIsPathRelative;
}


void ReosRainfallModel::connectItem( ReosRainfallItem *item )
{
  QStack<ReosRainfallItem *> itemToConnect;

  itemToConnect.push( item );

  while ( !itemToConnect.isEmpty() )
  {
    ReosRainfallItem *currentItem = itemToConnect.pop();
    currentItem->resolveDependencies();

    connect( currentItem, &ReosRainfallItem::changed, this, &ReosRainfallModel::onItemChanged );
    connect( currentItem, &ReosRainfallItem::itemInsertedInParent, this, &ReosRainfallModel::onItemInsertedInParent );
    connect( currentItem, &ReosRainfallItem::itemRemovedfromParent, this, &ReosRainfallModel::onItemRemovedfromParent );
    connect( currentItem, &ReosRainfallItem::itemWillBeInsertedInParent, this, &ReosRainfallModel::onItemWillBeInsertedInParent );
    connect( currentItem, &ReosRainfallItem::itemWillBeRemovedfromParent, this, &ReosRainfallModel::onItemWillBeRemovedfromParent );

    for ( int i = 0; i < currentItem->childrenCount(); ++i )
      itemToConnect.push( currentItem->itemAt( i ) );
  }
}




