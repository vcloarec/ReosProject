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

ReosWatershedTree::ReosWatershedTree()
{

}

ReosWatershed *ReosWatershedTree::addWatershed( ReosWatershed *watershed )
{
  //! Chexk if another watershed is upstream
  size_t i = 0;
  while ( i < mWatersheds.size() )
  {
    std::unique_ptr<ReosWatershed> &existingWatershed = mWatersheds.at( i );
    if ( watershed->contains( existingWatershed->outletPoint() ) )
    {
      watershed->addUpstreamWatershed( mWatersheds.at( i ).release() );
      mWatersheds.erase( mWatersheds.begin() + i );
    }
    else
      ++i;
  }

  mWatersheds.emplace_back( watershed );
  return mWatersheds.back().get();
}

ReosWatershed *ReosWatershedTree::downstreamWatershed( const QPolygonF &line, bool &ok ) const
{
  for ( const std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
  {
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

ReosWatershedItemModel::ReosWatershedItemModel( ReosWatershedTree *watershedStore, QObject *parent ):
  mWatershedStore( watershedStore )
{
}

QModelIndex ReosWatershedItemModel::index( int row, int column, const QModelIndex &parent ) const
{
  if ( !parent.isValid() )
  {
    if ( row < mWatershedStore->masterWatershedCount() )
      return createIndex( row, column, mWatershedStore->masterWatershed( row ) );
    else
      return QModelIndex();
  }

  ReosWatershed *watershed = static_cast<ReosWatershed *>( parent.internalPointer() );
  if ( !watershed )
    return
      QModelIndex();

  return createIndex( row, column, watershed );

}

QModelIndex ReosWatershedItemModel::parent( const QModelIndex &child ) const
{
  ReosWatershed *watershed = static_cast<ReosWatershed *>( child.internalPointer() );
  if ( watershed && watershed->downstreamWatershed() )
    return createIndex( watershed->downstreamWatershed()->positionInDownstreamWatershed(), 0, watershed->downstreamWatershed() );
  else
    return QModelIndex();
}

int ReosWatershedItemModel::rowCount( const QModelIndex &parent ) const
{
  if ( parent.isValid() )
    return indexToWatershed( parent )->directUpstreamWatershedCount();
  else
    return mWatershedStore->masterWatershedCount();
}

int ReosWatershedItemModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosWatershedItemModel::data( const QModelIndex &index, int role ) const
{

}

ReosWatershed *ReosWatershedItemModel::indexToWatershed( const QModelIndex &index )
{
  return static_cast<ReosWatershed *>( index.internalPointer() );
}
