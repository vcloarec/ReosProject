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


class QPolygonF;
class ReosGisEngine;

class REOSCORE_EXPORT ReosWatershedTree: public QObject
{
    Q_OBJECT
  public:
    explicit ReosWatershedTree( ReosGisEngine *gisEngine, QObject *parent = nullptr );

    //! Purpose to add a watershed to the tree. Returns whether the delineating of the purposed watershed intersect other delineating
    bool isWatershedIntersectExisting( ReosWatershed *purposedWatershed );

    /**
     * Adds a watershed to the store, in the \a downstreamWatershed (or in one of its sub watershed).
     * Take ownership and returns a pointer to the added watershed
     */
    ReosWatershed *addWatershed( ReosWatershed *watershed, bool adaptDelineating = false );

    //! Returns the smallest watershed that is downstream the line, if the line is partially included by any watershed, ok is false
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *downstreamWatershed( const QPolygonF &line, bool &ok ) const;

    //! Returns the smallest watershed (the more upstream that contains the points
    ReosWatershed *watershed( const QPointF &point );

    //! Returns the count of watershed (extreme downstream)
    int watershedCount() const;

    //! Returns the count of master watershed (extreme downstream)
    int masterWatershedCount() const;

    //! Returns a pointer to master watershed with \a index
    ReosWatershed *masterWatershed( int index ) const;

    //! Returns the position of the master watershed, if not a master watershed, returns -1
    int masterWatershedPosition( ReosWatershed *watershed ) const;

    //! Returns a list of all the watershed sorted from upstream to downstream
    QList<ReosWatershed *> allWatershedsFromUSToDS() const;

    //! Returns a list of all the watershed sorted from downstream to upstream
    QList<ReosWatershed *> allWatershedsFromDSToUS() const;

    //! Removes direction data present in any watershed in the tree
    void removeDirectionData();

    /**
     * Removes (if present) the watershed from the watershed \a ws, but do not delete it, returns a pointer to it
     * Do not maintained sub watershed but move them to downstream.
     */
    ReosWatershed *extractWatershed( ReosWatershed *ws );

    ReosWatershed *uriToWatershed( const QString &uri ) const;
    QString watershedUri( ReosWatershed *watershed ) const;

    //! Removes all the watersheds of the tree
    void clearWatersheds();

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &elem );


  signals:
    void treeWillBeReset();
    void treeReset();
    void watershedWillBeAdded();
    //! emitted when watershed is added with the pointer to the directly downsteam watershed (nullptr if added watershed is a the extreme downstream)
    void watershedAdded( ReosWatershed * );
    void watershedWillBeRemoved( ReosWatershed * );
    void watershedRemoved();
    void watershedChanged();

  private:
    std::vector<std::unique_ptr<ReosWatershed>> mWatersheds;
    ReosGisEngine *mGisEngine = nullptr;
};


class REOSCORE_EXPORT ReosWatershedItemModel: public QAbstractItemModel
{
    Q_OBJECT
  public:
    explicit ReosWatershedItemModel( ReosWatershedTree *watershedTree, QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const override;
    QModelIndex parent( const QModelIndex &child ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    //! Returns all the watershed contained in the model from upstream to downstream
    QList<ReosWatershed *> allWatershedsFromUSToDS() const;

    //! Returns all the watershed contained in the model from downstream to upstream
    QList<ReosWatershed *> allWatershedsFromDSToUS() const;

    QModelIndex watershedToIndex( ReosWatershed *watershed ) const;
    static ReosWatershed *indexToWatershed( const QModelIndex &index );

    void removeWatershed( const QModelIndex &index );

    ReosWatershed *uriToWatershed( const QString &uri ) const;
    QString watershedUri( ReosWatershed *watershed ) const;

  signals:
    void watershedAdded( const QModelIndex &index );

  private slots:
    void onWatershedWillBeAdded();
    void onWatershedAdded( ReosWatershed *watershed );
    void onWatershedWillBeRemoved( ReosWatershed *ws );
    void onWatershedRemoved();
    void onWatershedChanged();
    void onTreeWillBeReset();
    void onTreeReset();

  private:
    ReosWatershedTree *mWatershedTree = nullptr;
};


#endif // REOSWATERSHEDTREE_H
