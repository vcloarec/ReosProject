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

#include "reostimeserie.h"
#include "reoscore.h"

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
    ReosRainfallItem *parentItem() const {return mParent;}

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

    //! Remove child \a item, if not present, do nothing
    void removeItem( ReosRainfallItem *item );

    /**
     * Inserts a child \a item at position \a pos. Take ownership of the new child
     */
    void insertChild( int pos, ReosRainfallItem *item );

    //! Returns whether the item is a somewhere in the child hierarchy
    bool isSubItem( ReosRainfallItem *item ) const;

    //! Return the icon used to represent this item, default implentation return invalid icon
    virtual QIcon icone() const {return QIcon();}

    //! add another item as children of this item
    ReosRainfallItem *addItem( ReosRainfallItem *item );

    //! Returns all parameters of this items
    virtual QList<ReosParameter *> parameters() const;

    //! Returns whether this item can accept the other \a item
    virtual bool accept( ReosRainfallItem *item ) const;

    //! removes all children items
    virtual void clear();

    //! Return the data object link to this item
    virtual ReosDataObject *data() const {return nullptr;}

    //! Encodes in \a element the base information about the item
    void encodeBase( ReosEncodedElement &element ) const;

    //! Encoded the element
    virtual ReosEncodedElement encode() const = 0;

  signals:
    void changed( ReosRainfallItem *item );

  protected:
    ReosRainfallItem( const QString &name, const QString &description, Type type );
    ReosRainfallItem( const ReosEncodedElement &element, Type type );

    void connectParameters();

  private:
    std::unique_ptr<ReosParameterString> mName;
    std::unique_ptr<ReosParameterString> mDescription;
    Type  mType = Zone;
    ReosRainfallItem *mParent = nullptr;
    std::vector<std::unique_ptr<ReosRainfallItem>> mChildItems;
};


class ReosRainfallDataItem: public ReosRainfallItem
{
  public:
    ReosRainfallDataItem( const QString &name, const QString &description );
    ReosRainfallDataItem( const ReosEncodedElement &elem ): ReosRainfallItem( elem, Data ) {}
};

//! Class that represent a station item that contains rainfall data
class ReosStationItem: public ReosRainfallItem
{
  public:
    ReosStationItem( const QString &name, const QString &description );
    ReosStationItem( const ReosEncodedElement &element );

    QIcon icone() const override {return QIcon( QPixmap( ":/images/station.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const override;

    virtual ReosEncodedElement encode() const override
    {
      ReosEncodedElement element( QStringLiteral( "station-item" ) );
      ReosRainfallItem::encodeBase( element );
      return element;
    }

};

//! Class that represents a geographical zone item that can contain other zone or rainfall station
class ReosZoneItem: public ReosRainfallItem
{
  public:
    ReosZoneItem( const QString &name, const QString &descritpion );
    ReosZoneItem( const ReosEncodedElement &element );

    QIcon icone() const override {return QIcon( QPixmap( ":/images/fakeEarth.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const override;

    virtual ReosEncodedElement encode() const override
    {
      ReosEncodedElement element( QStringLiteral( "zone-item" ) );
      ReosRainfallItem::encodeBase( element );
      return element;
    }
};

//! Class that represents the root of the tree, can contain only zone item (\see ReosZoneItem)
class ReosRootItem: public ReosRainfallItem
{
  public:
    ReosRootItem();
    ReosRootItem( const ReosEncodedElement &element );

    QIcon icone() const override {return QIcon( QPixmap( ":/images/fakeEarth.svg" ) );}
    virtual bool accept( ReosRainfallItem *item ) const override;
    virtual ReosEncodedElement encode() const override;
};


//! Class that represents time serie data item
class ReosRainfallSeriesItem: public ReosRainfallDataItem
{
  public:
    ReosRainfallSeriesItem( const QString &name, const QString &description );
    ReosRainfallSeriesItem( const ReosEncodedElement &element );

    ReosTimeSerieConstantInterval *data() const override;
    virtual ReosEncodedElement encode() const override;
    QIcon icone() const override {return QIcon( QPixmap( ":/images/gaugedRainfall.svg" ) );}

  private:
    ReosTimeSerieConstantInterval *mData = nullptr;
    void setupData();
};


#endif // REOSRAINFALLITEM_H
