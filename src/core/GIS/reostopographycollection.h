/***************************************************************************
  reostopographycollection.h - ReosTopographyCollection

 ---------------------
 begin                : 28.2.2022
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
#ifndef REOSTOPOGRAPHYCOLLECTION_H
#define REOSTOPOGRAPHYCOLLECTION_H

#include <QAbstractListModel>
#include <QPointer>

#include "reoscore.h"
#include "reosdataobject.h"

class ReosGisEngine;
class ReosParameterBoolean;

class REOSCORE_EXPORT ReosTopographyCollection : public ReosDataObject
{
    Q_OBJECT

  public:
    static ReosTopographyCollection *createTopographyCollection( ReosGisEngine *gisEngine, QObject *parent = nullptr );
    static ReosTopographyCollection *createTopographyCollection( const ReosEncodedElement &element, ReosGisEngine *gisEngine, QObject *parent = nullptr );

    int topographyCount() const;
    QString topographyName( int index ) const;
    QString topographyId( int index ) const;

    void insertTopography( int index, const QString &topographyId );
    void removeTopography( int index );
    void removeTopography( const QString &topographyId );
    void moveTopoGraphyTo( const QString &topographyId, int destIndex );

    bool contains( const QString &topographyId ) const;

    QPixmap icon( const QString &layerId ) const;

    ReosEncodedElement encode() const;

    ReosParameterBoolean *autoApply() const;

  protected:
    explicit ReosTopographyCollection( ReosGisEngine *gisEngine, QObject *parent = nullptr );
    explicit ReosTopographyCollection( const ReosEncodedElement &element, ReosGisEngine *gisEngine, QObject *parent = nullptr );

    ReosGisEngine *mGisEngine = nullptr;
    QStringList mTopographyIds;
    ReosParameterBoolean *mAutoApply;

};



class REOSCORE_EXPORT ReosTopographyCollectionListModel : public QAbstractListModel
{
    Q_OBJECT
  public:
    ReosTopographyCollectionListModel( ReosTopographyCollection *collection, QObject *parent );

    QModelIndex index( int row, int column, const QModelIndex & ) const;
    QModelIndex parent( const QModelIndex & ) const;
    int rowCount( const QModelIndex & ) const;
    int columnCount( const QModelIndex & ) const;
    QVariant data( const QModelIndex &index, int role ) const;
    Qt::ItemFlags flags( const QModelIndex &index ) const;

    QStringList mimeTypes() const;
    QMimeData *mimeData( const QModelIndexList &indexes ) const;
    Qt::DropActions supportedDropActions() const;
    Qt::DropActions supportedDragActions() const;
    bool canDropMimeData( const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent ) const;
    bool dropMimeData( const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent );


  private slots:
    void onCollectionChanged();

  private:
    QPointer<ReosTopographyCollection> mCollection;

};




#endif // REOSTOPOGRAPHYCOLLECTION_H
