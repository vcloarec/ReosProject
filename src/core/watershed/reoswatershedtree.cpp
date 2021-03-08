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

ReosWatershedTree::ReosWatershedTree( ReosGisEngine *gisEngine, QObject *parent ):
  QObject( parent )
  , mGisEngine( gisEngine )
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

  std::unique_ptr<ReosWatershed> ws( watershedToAdd );

  if ( !ws )
    return nullptr;

  emit watershedWillBeAdded();

  ReosWatershed *includingWatershed = watershed( ws->outletPoint() );
  ws->setGeographicalContext( mGisEngine );
  ws->calculateArea();

  if ( includingWatershed ) // There is a watershed that contains the new one, deal with it
  {
    ReosWatershed *addedWatersehd = includingWatershed->addUpstreamWatershed( ws.release(), adaptDelineating );
    emit watershedAdded( addedWatersehd );
    return addedWatersehd;
  }
  else
  {
    // first assign new name
    if ( !ws->name()->isValid() )
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
      ReosWatershed *upstream = watershed->upstreamWatershed( point, true );
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

int ReosWatershedTree::masterWatershedPosition( ReosWatershed *watershed ) const
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
    emit watershedWillBeRemoved( ws );
    std::unique_ptr<ReosWatershed> ret( ds->extractOnlyDirectUpstreamWatershed( ws->positionInDownstreamWatershed() ) );
    emit watershedRemoved();
    return ret.release();
  }

  for ( size_t i = 0; i < mWatersheds.size(); ++i )
  {
    if ( mWatersheds.at( i ).get() == ws )
    {
      emit watershedWillBeRemoved( ws );
      std::unique_ptr<ReosWatershed> ret( mWatersheds.at( i ).release() );
      mWatersheds.erase( mWatersheds.begin() + i );
      while ( ret->directUpstreamWatershedCount() > 1 )
      {
        mWatersheds.emplace_back( ret->extractCompleteDirectUpstreamWatershed( 1 ) );
      }
      emit watershedRemoved();
      return ret.release();
    }
  }
  return nullptr;
}

ReosEncodedElement ReosWatershedTree::encode() const
{
  QList<QByteArray> watersheds;
  for ( const std::unique_ptr<ReosWatershed> &ws : mWatersheds )
    watersheds.append( ws->encode().bytes() );

  ReosEncodedElement ret( QStringLiteral( "watershed-tree" ) );
  ret.addData( QStringLiteral( "watersheds" ), watersheds );

  return ret;
}

void ReosWatershedTree::decode( const ReosEncodedElement &elem )
{
  emit treeWillBeReset();
  mWatersheds.clear();
  if ( elem.description() == QStringLiteral( "watershed-tree" ) )
  {
    QList<QByteArray> watershedsList;
    if ( elem.getData( QStringLiteral( "watersheds" ), watershedsList ) )
    {
      std::vector<std::unique_ptr<ReosWatershed>> watersheds;
      for ( const QByteArray &wsba : watershedsList )
      {
        std::unique_ptr<ReosWatershed> uws( ReosWatershed::decode( ReosEncodedElement( wsba ) ) );
        if ( uws )
        {
          uws->setGeographicalContext( mGisEngine );
          watersheds.emplace_back( uws.release() );
        }
      }

      mWatersheds = std::move( watersheds );
    }
  }
  emit treeReset();
}

ReosWatershedItemModel::ReosWatershedItemModel( ReosWatershedTree *watershedTree, QObject *parent ):
  QAbstractItemModel( parent ),
  mWatershedTree( watershedTree )
{
  connect( watershedTree, &ReosWatershedTree::watershedWillBeAdded, this, &ReosWatershedItemModel::onWatershedWillBeAdded );
  connect( watershedTree, &ReosWatershedTree::watershedAdded, this, &ReosWatershedItemModel::onWatershedAdded );
  connect( watershedTree, &ReosWatershedTree::treeWillBeReset, this, &ReosWatershedItemModel::onTreeWillBeReset );
  connect( watershedTree, &ReosWatershedTree::treeReset, this, &ReosWatershedItemModel::onTreeReset );
  connect( watershedTree, &ReosWatershedTree::watershedWillBeRemoved, this, &ReosWatershedItemModel::onWatershedWillBeRemoved );
  connect( watershedTree, &ReosWatershedTree::watershedRemoved, this, &ReosWatershedItemModel::onWatershedRemoved );
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
  if ( !child.isValid() )
    return QModelIndex();

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

  ReosWatershed *ws = indexToWatershed( index );

  if ( !ws )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
      return ws->name()->value();
      break;
    case Qt::ForegroundRole:
      if ( ws->type() == ReosWatershed::Residual )
        return QColor( Qt::gray );
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
  connect( watershed, &ReosWatershed::changed, this, &ReosWatershedItemModel::onWatershedChanged );
  emit watershedAdded( watershedToIndex( watershed ) );
}

void ReosWatershedItemModel::onWatershedWillBeRemoved( ReosWatershed *ws )
{
  disconnect( ws, &ReosWatershed::changed, this, &ReosWatershedItemModel::onWatershedChanged );
  beginResetModel();
}

void ReosWatershedItemModel::onWatershedRemoved()
{
  endResetModel();
}

void ReosWatershedItemModel::onWatershedChanged()
{
  ReosWatershed *ws = qobject_cast<ReosWatershed *>( sender() );

  if ( ws )
  {
    QModelIndex ind = watershedToIndex( ws );
    emit dataChanged( ind, ind );
  }

}

void ReosWatershedItemModel::onTreeWillBeReset()
{
  beginResetModel();
}

void ReosWatershedItemModel::onTreeReset()
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

void ReosWatershedItemModel::removeWatershed( const QModelIndex &index )
{
  ReosWatershed *ws = indexToWatershed( index );

  if ( !ws )
    return;
  std::unique_ptr<ReosWatershed> removed( mWatershedTree->extractWatershed( ws ) );

}

ReosWatershed *ReosWatershedItemModel::uriToWatershed( const QString &uri ) const
{
  return mWatershedTree->uriToWatershed( uri );
}

ReosWatershed *ReosWatershedTree::uriToWatershed( const QString &uri ) const
{
  QStringList strPositions = uri.split( ':' );

  if ( strPositions.isEmpty() )
    return nullptr;

  bool ok = false;

  int pos = strPositions.takeFirst().toInt( &ok );
  if ( pos < 0 || pos >= masterWatershedCount() )
    return nullptr;
  ReosWatershed *ret = masterWatershed( pos );

  while ( !strPositions.isEmpty() && ret )
  {
    pos = strPositions.takeFirst().toInt( &ok );
    if ( !ok || pos < 0 || pos >= ret->upstreamWatershedCount() )
      return nullptr;
    ret = ret->directUpstreamWatershed( pos );
  }

  return ret;
}

QString ReosWatershedTree::watershedUri( ReosWatershed *watershed ) const
{
  QString uri;
  ReosWatershed *currentWatershed = watershed;
  while ( currentWatershed->downstreamWatershed() )
  {
    uri.prepend( QString::number( currentWatershed->positionInDownstreamWatershed() ) );
    uri.prepend( ':' );
    currentWatershed = currentWatershed->downstreamWatershed();
  }

  uri.prepend( QString::number( masterWatershedPosition( currentWatershed ) ) );

  return uri;
}

void ReosWatershedTree::clearWatersheds()
{
  emit treeWillBeReset();
  mWatersheds.clear();
  emit treeReset();
}

QString ReosWatershedItemModel::watershedUri( ReosWatershed *watershed ) const
{
  return mWatershedTree->watershedUri( watershed );
}
