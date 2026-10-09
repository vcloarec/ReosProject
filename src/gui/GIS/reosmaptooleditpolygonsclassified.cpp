/***************************************************************************
  reosmaptooleditpolygonsclassified.cpp - ReosMapToolEditPolygonsClassified

 ---------------------
 begin                : 1.9.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmaptooleditpolygonsclassified.h"

#include <QIcon>
#include <QLocale>
#include <QPixmap>

#include <algorithm>
#include <memory>

#include "reosmaptooleditpolygonsclassified_p.h"

#include "reospolygonsclassified.h"

ReosMapToolEditPolygonsClassified::ReosMapToolEditPolygonsClassified( ReosPolygonsClassified *structure, QObject *parent, ReosMap *map )
  : ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygonsClassified_p( canvas );
  d->setStructure( structure );
  setCursor( Qt::CrossCursor );

  std::unique_ptr<ReosEditPolygonStructureMenuPopulator> menuPopulator = std::make_unique<ReosEditPolygonStructureMenuPopulator>( d );

  setContextMenuPopulator( menuPopulator.release() );
}

ReosMapToolEditPolygonsClassified::~ReosMapToolEditPolygonsClassified()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolEditPolygonsClassified::setCurrentClass( const QString &classId )
{
  d->setCurrentClassId( classId );
}

void ReosMapToolEditPolygonsClassified::addHelperStructure( ReosGeometryComplex *structure )
{
  d->addHelperStructure( structure );
}

QActionGroup *ReosMapToolEditPolygonsClassified::mainActions() const
{
  return d->mainActions();
}

ReosMapTool_p *ReosMapToolEditPolygonsClassified::tool_p() const
{
  return d;
}

ReosPolygonStructureClassModelList::ReosPolygonStructureClassModelList( ReosPolygonsClassified *structure, QObject *parent )
  : QAbstractListModel( parent )
  , mStructure( structure )
{
  connect( mStructure, &ReosPolygonsClassified::classesChanged, this, &ReosPolygonStructureClassModelList::onClassesChanged );
}

QModelIndex ReosPolygonStructureClassModelList::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosPolygonStructureClassModelList::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosPolygonStructureClassModelList::rowCount( const QModelIndex & ) const
{
  return mStructure->classes().count() + 1;
}

int ReosPolygonStructureClassModelList::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosPolygonStructureClassModelList::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return false;

  const QStringList classes = orderedClasses();

  switch ( role )
  {
    case Qt::DisplayRole:
      if ( index.row() < classes.count() )
        return QLocale().toString( mStructure->value( classes.at( index.row() ) ) );
      else
        return tr( "Default" );
      break;
    case Qt::DecorationRole:
      if ( index.row() < classes.count() )
      {
        QPixmap pixmap( 16, 16 );
        pixmap.fill( mStructure->color( classes.at( index.row() ) ) );
        return QIcon( pixmap );
      }
      break;
    case Qt::TextAlignmentRole:
      return Qt::AlignRight;
      break;
    default:
      break;
  }

  return QVariant();
}

QString ReosPolygonStructureClassModelList::classId( int index ) const
{
  if ( index < mStructure->classes().count() )
    return orderedClasses().at( index );

  return QString();
}

QModelIndex ReosPolygonStructureClassModelList::classToindex( const QString &classId ) const
{
  int ind = orderedClasses().indexOf( classId );
  if ( ind == -1 )
    return QModelIndex();

  return createIndex( ind, 0 );
}

void ReosPolygonStructureClassModelList::onClassesChanged()
{
  beginResetModel();
  endResetModel();
}

QStringList ReosPolygonStructureClassModelList::orderedClasses() const
{
  QStringList classes = mStructure->classes();

  std::sort( classes.begin(), classes.end(), [this]( const QString &classId1, const QString &classId2 ) { return mStructure->value( classId1 ) < mStructure->value( classId2 ); } );

  return classes;
}