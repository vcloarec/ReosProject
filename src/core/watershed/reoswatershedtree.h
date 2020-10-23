/***************************************************************************
                      reoswatershedtree.h
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

#ifndef REOSWATERSHEDTREE_H
#define REOSWATERSHEDTREE_H

#include <vector>
#include <memory>

#include <QAbstractItemModel>

#include "reoswatershed.h"

//class ReosWatershed;
class QPolygonF;

class ReosWatershedTree: public QObject
{
    Q_OBJECT
  public:
    ReosWatershedTree();

    //! Adds a watershed to the store, take ownership and return a pointer to the watershed
    ReosWatershed *addWatershed( ReosWatershed *watershed );

    //! Returns the smallest watershed that is downstream the line, if the line is partially included by any watershed, ok is false
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *downstreamWatershed( const QPolygonF &line, bool &ok ) const;

    int watershedCount() const;
    int masterWatershedCount() const;

    ReosWatershed *masterWatershed( int index ) const;

  signals:
    void whatershedAdded();

  private:

    std::vector<std::unique_ptr<ReosWatershed>> mWatersheds;
};


class ReosWatershedItemModel: public QAbstractItemModel
{
  public:
    ReosWatershedItemModel( ReosWatershedTree *watershedStore, QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;


  private:
    ReosWatershedTree *mWatershedStore = nullptr;

    static ReosWatershed *indexToWatershed( const QModelIndex &index );
};


#endif // REOSWATERSHEDTREE_H
