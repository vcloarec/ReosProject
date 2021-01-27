/***************************************************************************
  reosrainfallitem.h - ReosRainfallItem

 ---------------------
 begin                : 26.1.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSRAINFALLITEM_H
#define REOSRAINFALLITEM_H

#include <QObject>
#include <QIcon>

#include "reoscore.h"
#include "reosrainfalldata.h"

class ReosParameterString;
class ReosParameter;
class ReosTimeSerieConstantInterval;

class ReosRainfallItem : public QObject
{
    Q_OBJECT
  public:

    enum Type
    {
      Root,
      Zone,
      Station,
      Data
    };

    virtual ~ReosRainfallItem();

    //! Returns the type of the item
    Type type() const {return mType;}
    //! Returns the name of the item
    QString name();
    //! Returns the description of the item
    QString description();

    //! Returns the parent of the item, root item hasn't parent (nullptr)
    ReosRainfallItem *parent() const {return mParent;}

    //! Returns the count of child items
    int childrenCount() const;
    //! Returns the child at postion \a i
    ReosRainfallItem *itemAt( int i ) const;
    //! Returns whether the item have a child with name \a itemName
    bool hasChildItemName( const QString &itemName ) const;
    //! Returns a list of int representing the position of the item in the tree from root item
    QList<int> positionPathInTree() const;

    //! Returns the position of this item in the parent item, returns -1 if parent is nullptr
    int positionInParent() const;

    /**
     *  Takes the children at position \a pos. If no item at this position returns nullptr.
     *  The taken children has no parent anymore an caller need to handle ownership.
     */
    ReosRainfallItem *takeChild( int pos );

    /**
     * Inserts a child \a item at position \a pos. Take ownership of the new child
     */
    void insertChild( int pos, ReosRainfallItem *item );

    //! Returns whether the item is a somewhere in the child hierarchy
    bool isSubItem( ReosRainfallItem *item ) const;


    virtual QIcon icone() const {return QIcon();}
    virtual ReosRainfallItem *addItem( ReosRainfallItem *item );
    virtual QList<ReosParameter *> parameters() const;
    virtual bool accept( ReosRainfallItem *item ) const;

  signals:
    void changed( ReosRainfallItem *item );

  protected:
    ReosRainfallItem( const QString &name, const QString &description, Type type );

  private:
    std::unique_ptr<ReosParameterString> mName;
    std::unique_ptr<ReosParameterString> mDescription;
    Type  mType = Zone;
    ReosRainfallItem *mParent = nullptr;
    std::vector<std::unique_ptr<ReosRainfallItem>> mChildItem;

};


class ReosRainfallDataItem: public ReosRainfallItem
{
  public:
    ReosRainfallDataItem( const QString &name, const QString &description );

  private:
    std::vector < std::unique_ptr<ReosRainfallDataItem>> mSubData;
};


class ReosStationItem: public ReosRainfallItem
{
  public:
    ReosStationItem( const QString &name, const QString &description );
    QIcon icone() const {return QIcon( QPixmap( ":/images/station.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const;

};


class ReosZoneItem: public ReosRainfallItem
{
  public:
    ReosZoneItem( const QString &name, const QString &descritpion );

    //! Adds an item to the zone, can be any type of item, return false if it fails
    ReosRainfallItem *addItem( ReosRainfallItem *item ) override;

    QIcon icone() const override {return QIcon( QPixmap( ":/images/fakeEarth.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const override;

};


class ReosRootItem: public ReosRainfallItem
{
  public:
    ReosRootItem();

    //! Adds an item to the zone, can only be zone item
    ReosRainfallItem *addItem( ReosRainfallItem *item ) override;

    QIcon icone() const override {return QIcon( QPixmap( ":/images/fakeEarth.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const override;

};


class ReosRainfallSeriesItem: public ReosRainfallDataItem
{

  public:

    ReosRainfallSeriesItem( const QString &name, const QString &description ):
      ReosRainfallDataItem( name, description )
    {

    }


    std::weak_ptr<ReosTimeSerieConstantInterval> data() const
    {
      return std::weak_ptr<ReosTimeSerieConstantInterval>( mData );
    }


  private:
    std::shared_ptr<ReosTimeSerieConstantInterval> mData;



};


#endif // REOSRAINFALLITEM_H
