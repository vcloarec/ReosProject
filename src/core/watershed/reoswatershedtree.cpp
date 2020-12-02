/***************************************************************************
                      reoswatershedtree.cpp
                     --------------------------------------
Date                 : 04-10-2020
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

#include <QStack>
#include "reoswatershedtree.h"
#include "reoswatershed.h"

ReosWatershedTree::ReosWatershedTree( QObject *parent ): QObject( parent )
{
}

ReosWatershed *ReosWatershedTree::addWatershed( ReosWatershed *watershedToAdd, ReosWatershed *downstreamWatershed, bool adaptDelineating )
{
  std::unique_ptr<ReosWatershed> ws( watershedToAdd );
  emit watershedWillBeAdded();
  ReosWatershed *addedWatershed = nullptr;
  if ( downstreamWatershed )
  {
    emit watershedAdded( downstreamWatershed->addUpstreamWatershed( ws.release(), adaptDelineating ) );
  }
  else
  {
    // Check if another watershed is upstream
    size_t i = 0;
    while ( i < mWatersheds.size() )
    {
      std::unique_ptr<ReosWatershed> &existingWatershed = mWatersheds.at( i );
      if ( ws->contains( existingWatershed->outletPoint() ) )
      {
        ws->addUpstreamWatershed( mWatersheds.at( i ).release(), adaptDelineating );
        mWatersheds.erase( mWatersheds.begin() + i );
      }
      else
        ++i;
    }

    // Add the wartershed here
    if ( ws->name().isEmpty() )
      ws->setName( tr( "Watershed-%1" ).arg( mWatersheds.size() + 1 ) );
    mWatersheds.emplace_back( ws.release() );
  }

  emit watershedAdded( nullptr );
  return addedWatershed;
}

ReosWatershed *ReosWatershedTree::downstreamWatershed( const QPolygonF &line, bool &ok ) const
{
  for ( const std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
  {
    assert( watershed );
    switch ( watershed->contains( line ) )
    {
      case ReosInclusionType::None:
        continue;
        break;
      case ReosInclusionType::Partial:
        ok = false;
        return nullptr;
        break;
      case ReosInclusionType::Total:
        ok = true;
        return watershed.get();
        break;
    }
  }

  ok = true;
  return nullptr;
}

int ReosWatershedTree::watershedCount() const
{
  int count = 0;
  for ( const std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
    count += watershed->upstreamWatershedCount() + 1;

  return count;
}

int ReosWatershedTree::masterWatershedCount() const
{
  return int( mWatersheds.size() );
}

ReosWatershed *ReosWatershedTree::masterWatershed( int index ) const
{
  if ( index >= 0 && index < int( mWatersheds.size() ) )
    return mWatersheds.at( index ).get();
  else
    return nullptr;
}

int ReosWatershedTree::masterWatershedPosition( ReosWatershed *watershed )
{
  for ( size_t i = 0; i < mWatersheds.size(); ++i )
  {
    if ( watershed == mWatersheds.at( i ).get() )
      return i;
  }

  return -1;
}

QList<ReosWatershed *> ReosWatershedTree::allWatershed() const
{
  QList<ReosWatershed *> list;
  for ( const std::unique_ptr<ReosWatershed> &ws : mWatersheds )
  {
    list.append( ws->allDownstreamWatershed() );
    list.append( ws.get() );
  }

  return list;
}

ReosWatershedItemModel::ReosWatershedItemModel( ReosWatershedTree *watershedTree, QObject *parent ):
  mWatershedTree( watershedTree )
{
  connect( watershedTree, &ReosWatershedTree::watershedWillBeAdded, this, &ReosWatershedItemModel::onWatershedWillBeAdded );
  connect( watershedTree, &ReosWatershedTree::watershedAdded, this, &ReosWatershedItemModel::onWatershedAdded );
}

QModelIndex ReosWatershedItemModel::index( int row, int column, const QModelIndex &parent ) const
{
  if ( !parent.isValid() )
  {
    if ( row < mWatershedTree->masterWatershedCount() )
      return createIndex( row, column, mWatershedTree->masterWatershed( row ) );
    else
      return QModelIndex();
  }

  ReosWatershed *watershed = indexToWatershed( parent );
  if ( !watershed )
    return
      QModelIndex();

  return createIndex( row, column, watershed->directUpstreamWatershed( row ) );

}

QModelIndex ReosWatershedItemModel::parent( const QModelIndex &child ) const
{
  ReosWatershed *watershed = static_cast<ReosWatershed *>( child.internalPointer() );
  if ( watershed && watershed->downstreamWatershed() )
    return watershedToIndex( watershed->downstreamWatershed() );
  else
    return QModelIndex();
}

int ReosWatershedItemModel::rowCount( const QModelIndex &parent ) const
{
  if ( parent.isValid() )
    return indexToWatershed( parent )->directUpstreamWatershedCount();
  else
    return mWatershedTree->masterWatershedCount();
}

int ReosWatershedItemModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosWatershedItemModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
      return indexToWatershed( index )->name();
      break;
    default:
      break;

  }

  return QVariant();
}

QList<ReosWatershed *> ReosWatershedItemModel::allWatersheds() const
{
  return mWatershedTree->allWatershed();
}

void ReosWatershedItemModel::onWatershedWillBeAdded()
{
  // for now, it is quite a mess to know where is inserted the watershed, so ... reset the model...
  beginResetModel();
}

void ReosWatershedItemModel::onWatershedAdded( ReosWatershed *watershed )
{
  endResetModel();
  emit watershedAdded( watershedToIndex( watershed ) );
}

QModelIndex ReosWatershedItemModel::watershedToIndex( ReosWatershed *watershed ) const
{
  if ( !watershed )
    return QModelIndex();

  QModelIndex parentIndex = watershedToIndex( watershed->downstreamWatershed() );

  int row;
  if ( watershed->downstreamWatershed() )
    row = watershed->positionInDownstreamWatershed();
  else
    row = mWatershedTree->masterWatershedPosition( watershed );

  if ( row < 0 )
    return QModelIndex();

  return index( row, 0, parentIndex );
}

ReosWatershed *ReosWatershedItemModel::indexToWatershed( const QModelIndex &index )
{
  if ( index.isValid() )
    return static_cast<ReosWatershed *>( index.internalPointer() );
  else
    return nullptr;
}
