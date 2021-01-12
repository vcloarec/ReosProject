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
{}

bool ReosWatershedTree::isWatershedIntersectExisting( ReosWatershed *purposedWatershed )
{
  const ReosWatershed *dws = watershed( purposedWatershed->outletPoint() );

  if ( dws )
  {
    if ( ReosInclusionType::Partial == purposedWatershed->isContainedBy( *dws ) )
      return true;

    int subWsCount = dws->directUpstreamWatershedCount();
    for ( int i = 0; i < subWsCount; ++i )
    {
      if ( ReosInclusionType::Partial == purposedWatershed->isContainedBy( *dws->directUpstreamWatershed( i ) ) )
        return true;
    }
  }
  else
  {
    for ( size_t i = 0; i < mWatersheds.size(); ++i )
    {
      ReosWatershed *other = mWatersheds.at( i ).get();
      if ( purposedWatershed->contain( other->outletPoint() ) )
      {
        if ( ReosInclusionType::Partial == other->isContainedBy( *purposedWatershed ) )
          return true;
      }
      else if ( ReosInclusionType::Partial == purposedWatershed->isContainedBy( *other ) )
        return true;
    }
  }

  return false;

}

ReosWatershed *ReosWatershedTree::addWatershed( ReosWatershed *watershedToAdd, bool adaptDelineating )
{
  if ( !watershedToAdd )
    return nullptr;

  emit watershedWillBeAdded();

  std::unique_ptr<ReosWatershed> ws( watershedToAdd );

  ReosWatershed *includingWatershed = watershed( ws->outletPoint() );

  if ( includingWatershed ) // There is a watershed that contains the new one, deal with it
  {
    ReosWatershed *addedWatersehd = includingWatershed->addUpstreamWatershed( ws.release(), adaptDelineating );
    emit watershedAdded( addedWatersehd );
    return addedWatersehd;
  }
  else
  {
    // first assign new name
    if ( ws->name().isEmpty() )
      ws->setName( tr( "Watershed-%1" ).arg( mWatersheds.size() + 1 ) );

    size_t i = 0;
    while ( i < mWatersheds.size() )
    {
      std::unique_ptr<ReosWatershed> &sibling = mWatersheds.at( i );
      if ( ws->contain( sibling->outletPoint() ) ) // the sibling is in the new watershed -> move it in the new watershed
      {
        if ( adaptDelineating )
          ws->extentTo( *sibling.get() );
        ws->addUpstreamWatershed( sibling.release(), adaptDelineating );
        mWatersheds.erase( mWatersheds.begin() + i );
      }
      else
      {
        if ( adaptDelineating )
          ws->adjust( *sibling );
        ++i;
      }
    }

    mWatersheds.emplace_back( ws.release() );
    emit watershedAdded( mWatersheds.back().get() );
    return mWatersheds.back().get();
  }
}

ReosWatershed *ReosWatershedTree::downstreamWatershed( const QPolygonF &line, bool &ok ) const
{
  for ( const std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
  {
    assert( watershed );
    switch ( watershed->contain( line ) )
    {
      case ReosInclusionType::None:
        continue;
        break;
      case ReosInclusionType::Partial:
        ok = false;
        return nullptr;
        break;
      case ReosInclusionType::Total:
      {
        ReosWatershed *upstream = watershed->upstreamWatershed( line, ok );
        if ( upstream && ok )
          return upstream;
        else if ( !ok )
          return nullptr;

        ok = true;
        return watershed.get();
      }
      break;
    }
  }

  ok = true;
  return nullptr;
}

ReosWatershed *ReosWatershedTree::watershed( const QPointF &point )
{
  for ( std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
  {
    if ( watershed->contain( point ) )
    {
      ReosWatershed *upstream = watershed->upstreamWatershed( point );
      if ( upstream )
        return upstream;

      return watershed.get();
    }
  }

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
    list.append( ws->allUpstreamWatershed() );
    list.append( ws.get() );
  }

  return list;
}

void ReosWatershedTree::removeDirectionData()
{
  for ( size_t i = 0; i < mWatersheds.size(); ++i )
    mWatersheds.at( i )->removeDirectionData();
}

ReosWatershed *ReosWatershedTree::extractWatershed( ReosWatershed *ws )
{
  ReosWatershed *ds = ws->downstreamWatershed();
  if ( ds )
  {
    emit watershedWillBeRemoved();
    std::unique_ptr<ReosWatershed> ret( ds->extractOnlyDirectUpstreamWatershed( ws->positionInDownstreamWatershed() ) );
    emit watershedRemoved();
    return ret.release();
  }

  for ( size_t i = 0; i < mWatersheds.size(); ++i )
  {
    if ( mWatersheds.at( i ).get() == ws )
    {
      emit watershedWillBeRemoved();
      std::unique_ptr<ReosWatershed> ret( mWatersheds.at( i ).release() );
      mWatersheds.erase( mWatersheds.begin() + i );
      emit watershedRemoved();
      for ( int j = 1; j < ret->directUpstreamWatershedCount(); ++j )
      {
        mWatersheds.emplace_back( ret->extractCompleteDirectUpstreamWatershed( j ) );
      }
      return ret.release();
    }
  }
  return nullptr;
}

ReosWatershedItemModel::ReosWatershedItemModel( ReosWatershedTree *watershedTree, QObject *parent ):
  QAbstractItemModel( parent ),
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
  beginResetModel();
}

void ReosWatershedItemModel::onWatershedAdded( ReosWatershed *watershed )
{
  endResetModel();
  emit watershedAdded( watershedToIndex( watershed ) );
}

void ReosWatershedItemModel::onWatershedWillBeRemoved()
{
  beginResetModel();
}

void ReosWatershedItemModel::onWatershedRemoved()
{
  endResetModel();
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
