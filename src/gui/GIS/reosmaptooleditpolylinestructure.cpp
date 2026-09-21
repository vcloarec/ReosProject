/***************************************************************************
  reosmaptooleditpolylinestructure.cpp - ReosMapToolEditPolylineStructure

 ---------------------
 begin                : 12.1.2022
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
#include "reosmaptooleditpolylinestructure.h"
#include "reosmaptooleditpolylinestructure_p.h"

#include <algorithm>
#include <memory>

#include "reospolylinesstructure.h"

ReosMapToolEditPolylineStructure::ReosMapToolEditPolylineStructure( ReosPolylinesStructure *structure, QObject *parent, ReosMap *map )
  : ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolylineStructure_p( canvas );
  d->setStructure( structure );
  setCursor( Qt::CrossCursor );

  std::unique_ptr<ReosEditPolylineStructureMenuPopulator> menuPopulator = std::make_unique<ReosEditPolylineStructureMenuPopulator>( d );

  setContextMenuPopulator( menuPopulator.release() );

  setUp();
}

ReosMapToolEditPolylineStructure::~ReosMapToolEditPolylineStructure()
{
  if ( !d.isNull() )
    d->deleteLater();
}

QActionGroup *ReosMapToolEditPolylineStructure::mainActions() const
{
  return d->mainActions();
}

ReosMapTool_p *ReosMapToolEditPolylineStructure::tool_p() const
{
  return d;
}

ReosPolylineStructureClassModelList::ReosPolylineStructureClassModelList( ReosPolylinesStructure *structure, QObject *parent )
  : QAbstractListModel( parent )
  , mStructure( structure )
{
  connect( mStructure, &ReosPolylinesStructure::classesChanged, this, &ReosPolylineStructureClassModelList::onClassesChanged );
}

QModelIndex ReosPolylineStructureClassModelList::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosPolylineStructureClassModelList::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosPolylineStructureClassModelList::rowCount( const QModelIndex & ) const
{
  return mStructure->classes().count();
}

int ReosPolylineStructureClassModelList::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosPolylineStructureClassModelList::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return false;

  const QStringList classes = orderedClasses();

  switch ( role )
  {
    case Qt::DisplayRole:
      if ( index.row() < classes.count() )
        return mStructure->value( classes.at( index.row() ) );
      break;
    case Qt::TextAlignmentRole:
      return Qt::AlignRight;
      break;
    default:
      break;
  }

  return QVariant();
}

QString ReosPolylineStructureClassModelList::classId( int index ) const
{
  if ( index < mStructure->classes().count() )
    return orderedClasses().at( index );

  return QString();
}

QModelIndex ReosPolylineStructureClassModelList::classToindex( const QString &classId ) const
{
  int ind = orderedClasses().indexOf( classId );
  if ( ind == -1 )
    return QModelIndex();

  return createIndex( ind, 0 );
}

void ReosPolylineStructureClassModelList::onClassesChanged()
{
  beginResetModel();
  endResetModel();
}

QStringList ReosPolylineStructureClassModelList::orderedClasses() const
{
  QStringList classes = mStructure->classes();

  std::sort( classes.begin(), classes.end(), [this]( const QString &classId1, const QString &classId2 ) { return mStructure->value( classId1 ).toString() < mStructure->value( classId2 ).toString(); } );

  return classes;
}
