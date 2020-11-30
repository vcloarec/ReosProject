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

    /**
     * Adds a watershed to the store, in the \a downstreamWatershed (or in one of its su watershed).
     * Take ownership and return a pointer to the watershed where it was added
     */
    ReosWatershed *addWatershed( ReosWatershed *watershed, ReosWatershed *downstreamWatershed, bool adaptDelineating = false );

    //! Returns the smallest watershed that is downstream the line, if the line is partially included by any watershed, ok is false
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *downstreamWatershed( const QPolygonF &line, bool &ok ) const;

    //! Returns the count ofwatershed (extreme downstream)
    int watershedCount() const;

    //! Returns the count of master watershed (extreme downstream)
    int masterWatershedCount() const;

    //! Returns a pointer to master watershed with \a index
    ReosWatershed *masterWatershed( int index ) const;

    //! Returns the position of the master watershed, if not a master watershed, returns -1
    int masterWatershedPosition( ReosWatershed *watershed );

    //! Returns a list of all the watershed
    QList<ReosWatershed *> allWatershed() const;

  signals:
    void watershedWillBeAdded();
    //! emitted when watershed is added with the pointer to the directly downsteam watershed (nullptr if added watershed is a the extreme downstream)
    void watershedAdded( ReosWatershed * );

  private:

    std::vector<std::unique_ptr<ReosWatershed>> mWatersheds;
};


class ReosWatershedItemModel: public QAbstractItemModel
{
    Q_OBJECT
  public:
    ReosWatershedItemModel( ReosWatershedTree *watershedTree, QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;


    QList<ReosWatershed *> allWatersheds() const;

  signals:
    void watershedAdded( const QModelIndex &index );

  private slots:
    void onWatershedWillBeAdded();
    //! emitted when watershed is added with the index of the directly downsteam watershed (invalid if added watershed is a the extreme downstream)
    void onWatershedAdded( ReosWatershed *watershed );

  private:
    ReosWatershedTree *mWatershedTree = nullptr;

    QModelIndex watershedToIndex( ReosWatershed *watershed ) const;
    static ReosWatershed *indexToWatershed( const QModelIndex &index );
};


#endif // REOSWATERSHEDTREE_H
