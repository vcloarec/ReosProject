/***************************************************************************
  reosmaptooleditpolylinestructure.h - ReosMapToolEditPolylineStructure

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
#ifndef REOSMAPTOOLEDITPOLYLINESTRUCTURE_H
#define REOSMAPTOOLEDITPOLYLINESTRUCTURE_H

#include <QAbstractListModel>
#include <QPointer>
#include <QStringList>

#include "reosmaptool.h"

class QActionGroup;

class ReosMapToolEditPolylineStructure_p;
class ReosPolylinesStructure;


class ReosMapToolEditPolylineStructure : public ReosMapTool
{
  public:
    ReosMapToolEditPolylineStructure( ReosPolylinesStructure *structure, QObject *parent, ReosMap *map );
    ~ReosMapToolEditPolylineStructure();

    QActionGroup *mainActions() const;

  private:
    QPointer<ReosMapToolEditPolylineStructure_p> d;
    ReosMapTool_p *tool_p() const;
};

class ReosPolylineStructureClassModelList : public QAbstractListModel
{
    Q_OBJECT
  public:
    ReosPolylineStructureClassModelList( ReosPolylinesStructure *structure, QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;

    QString classId( int index ) const;
    QModelIndex classToindex( const QString &classId ) const;

  private slots:
    void onClassesChanged();

  private:
    QPointer<ReosPolylinesStructure> mStructure;
    QStringList orderedClasses() const;
};


#endif // REOSMAPTOOLEDITPOLYLINESTRUCTURE_H
