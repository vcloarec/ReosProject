/***************************************************************************
  reoshydraulicelementmodel.cpp - ReosHydraulicElementModel

 ---------------------
 begin                : 30.12.2022
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
#include "reoshydraulicelementmodel.h"

#include "reoshydraulicnetwork.h"

ReosHydraulicElementModel::ReosHydraulicElementModel( ReosHydraulicNetwork *parent )
  : QAbstractListModel( parent )
  , mNetwork( parent )
{
  connect( mNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicElementModel::updateElements );
  connect( mNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosHydraulicElementModel::updateElements );
}

int ReosHydraulicElementModel::rowCount( const QModelIndex & ) const
{
  return mElements.count();
}

QVariant ReosHydraulicElementModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() < 0 || index.row() >= mElements.count() )
    return QVariant();

  ReosHydraulicNetworkElement *elem = indexToElement( index );
  if ( !elem )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
      return elem->elementName()->value();
      break;
    case Qt::DecorationRole:
      return elem->icon();
    default:
      return QVariant();
      break;
  }
}

QModelIndex ReosHydraulicElementModel::elementToIndex( ReosHydraulicNetworkElement *element ) const
{
  int row = mElements.indexOf( element );
  return createIndex( row, 0 );
}

ReosHydraulicNetworkElement *ReosHydraulicElementModel::indexToElement( const QModelIndex &index ) const
{
  if ( index.row() < 0 && index.row() >= mElements.count() )
    return nullptr;

  return mElements.at( index.row() );
}

void ReosHydraulicElementModel::updateElements()
{
  beginResetModel();

  mElements = mNetwork->hydraulicNetworkElements();
  std::sort( mElements.begin(), mElements.end(), []( ReosHydraulicNetworkElement * elem1, ReosHydraulicNetworkElement * elem2 )->bool
  {
    return elem1->elementName()->value() < elem2->elementName()->value();
  } );

  endResetModel();
}
