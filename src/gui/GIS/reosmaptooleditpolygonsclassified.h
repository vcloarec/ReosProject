/***************************************************************************
  reosmaptooleditpolygonsclassified.h - ReosMapToolEditPolygonsClassified

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
#ifndef REOSMAPTOOLEDITPOLYGONSCLASSIFIED_H
#define REOSMAPTOOLEDITPOLYGONSCLASSIFIED_H

#include <QAbstractListModel>
#include <QPointer>
#include <QStringList>

#include "reosmaptool.h"

class QActionGroup;

class ReosMapToolEditPolygonsClassified_p;
class ReosGeometryComplex;
class ReosPolygonsClassified;

class ReosMapToolEditPolygonsClassified : public ReosMapTool
{
  public:
    ReosMapToolEditPolygonsClassified( ReosPolygonsClassified *structure, QObject *parent, ReosMap *map );
    ~ReosMapToolEditPolygonsClassified();

    void setCurrentClass( const QString &classId );

    void addHelperStructure( ReosGeometryComplex *structure );

    QActionGroup *mainActions() const;

  private:
    QPointer<ReosMapToolEditPolygonsClassified_p> d;
    ReosMapTool_p *tool_p() const override;
};

class ReosPolygonStructureClassModelList : public QAbstractListModel
{
    Q_OBJECT
  public:
    ReosPolygonStructureClassModelList( ReosPolygonsClassified *structure, QObject *parent = nullptr );

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
    QPointer<ReosPolygonsClassified> mStructure;
    QStringList orderedClasses() const;
};

#endif // REOSMAPTOOLEDITPOLYGONSCLASSIFIED_H