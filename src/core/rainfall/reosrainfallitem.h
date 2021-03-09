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
#include <QUuid>

#include "reosidfcurves.h"
#include "reostimeserie.h"
#include "reossyntheticrainfall.h"
#include "reoscore.h"

class ReosParameterString;
class ReosParameter;
class ReosRainfallIntensityDurationCurveItem;
class ReosChicagoRainfall;

class REOSCORE_EXPORT ReosRainfallItem : public QObject
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
    virtual Type type() const {return mType;}
    //! Returns the name of the item
    virtual QString name() const;
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

    //! Returns a string that represent the position of this item in the tree
    QString uri() const;

    //! Returns a string that represents the unique id of the item
    QString uniqueId() const;

    //! Searches the child with unique id \a uid as deep as necessary, returns the child item or nullptr it does not exist
    ReosRainfallItem *searchForChildWithUniqueId( const QString &uid ) const;

    //! Returns the position of this item in the parent item, returns -1 if parent is nullptr
    int positionInParent() const;

    //! Remove child \a item, if not present, do nothing
    void removeItem( ReosRainfallItem *item );

    //! add another item as children of this item
    ReosRainfallItem *addItem( ReosRainfallItem *item );

    //! Returns whether the item is a somewhere in the child hierarchy
    bool isSubItem( ReosRainfallItem *item ) const;

    //! Return the icon used to represent this item, default implentation return invalid icon
    virtual QIcon icone() const {return QIcon();}

    //! Returns all parameters of this items
    virtual QList<ReosParameter *> parameters() const;

    //! Returns whether this item can accept the other \a item
    virtual bool accept( ReosRainfallItem *item ) const;

    //! removes all children items
    virtual void clear();

    //! Return the data object link to this item, dfault implementation return nullptr
    virtual ReosDataObject *data() const {return nullptr;}

    //! Encodes in \a element the base information about the item
    void encodeBase( ReosEncodedElement &element ) const;

    //! Encoded the element
    virtual ReosEncodedElement encode() const = 0;

    //! Retrivied item dependencies, for example after loading from disk
    virtual void resolveDependencies() {}

    void swapChildren( int first, int second );


    /**
     *  Inserts a child \a item at position \a pos. Take ownership of the new child,
     * As this method emit signals to the model before and after interting, this method can be used from items
     */
    void insertChild( int pos, ReosRainfallItem *item );

    /**
     *  Takes the children at position \a pos. If no item at this position returns nullptr.
     *  The taken children has no parent anymore an caller need to handle ownership.
     *
     * As this method emit signals to the model before and after taking, this method can be used from items
     */
    ReosRainfallItem *takeChild( int pos );

  public slots :
    //! Setup the data related to the item, default implementation does nothing, must be called before acces to the data of the item
    virtual void setupData() {}

    //! method calles when a children is changed, default implementation does nothing
    virtual void onChildChanged( ReosRainfallItem * ) {}

  signals:
    void changed( ReosRainfallItem *item );
    void itemWillBeRemovedfromParent( ReosRainfallItem *item, int pos );
    void itemRemovedfromParent();
    void itemWillBeInsertedInParent( ReosRainfallItem *item, int pos );
    void itemInsertedInParent();

  protected:
    ReosRainfallItem( const QString &name, const QString &description, Type type );
    ReosRainfallItem( const ReosEncodedElement &element, Type type );

    void connectParameters();

  private:
    ReosParameterString *mName;
    ReosParameterString *mDescription;
    QString mUid;
    Type  mType = Zone;
    ReosRainfallItem *mParent = nullptr;
    std::vector<std::unique_ptr<ReosRainfallItem>> mChildItems;
};


class REOSCORE_EXPORT ReosRainfallDataItem: public ReosRainfallItem
{
    Q_OBJECT
  public:

    ReosRainfallDataItem( const QString &name, const QString &description );
    ReosRainfallDataItem( const ReosEncodedElement &elem ): ReosRainfallItem( elem, Data ) {}

    virtual QString dataType() const = 0;
};

//! Class that represent a station item that contains rainfall data
class REOSCORE_EXPORT ReosStationItem: public ReosRainfallItem
{
    Q_OBJECT
  public:
    ReosStationItem( const QString &name, const QString &description );
    ReosStationItem( const ReosEncodedElement &element );

    QIcon icone() const override {return QIcon( QPixmap( ":/images/station.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const override;

    virtual ReosEncodedElement encode() const override;

};

//! Class that represents a geographical zone item that can contain other zone or rainfall station
class REOSCORE_EXPORT ReosZoneItem: public ReosRainfallItem
{
    Q_OBJECT
  public:
    ReosZoneItem( const QString &name, const QString &descritpion );
    ReosZoneItem( const ReosEncodedElement &element );

    QIcon icone() const override {return QIcon( QPixmap( ":/images/fakeEarth.svg" ) );}

    virtual bool accept( ReosRainfallItem *item ) const override;

    virtual ReosEncodedElement encode() const override;
};

//! Class that represents the root of the tree, can contain only zone item (\see ReosZoneItem)
class ReosRootItem: public ReosRainfallItem
{
    Q_OBJECT
  public:
    ReosRootItem();
    ReosRootItem( const ReosEncodedElement &element );

    QIcon icone() const override {return QIcon( QPixmap( ":/images/fakeEarth.svg" ) );}
    virtual bool accept( ReosRainfallItem *item ) const override;
    virtual ReosEncodedElement encode() const override;
};

class REOSCORE_EXPORT ReosRainfallSerieRainfallItem: public ReosRainfallDataItem
{
    Q_OBJECT
  public:
    ReosRainfallSerieRainfallItem( const QString &name, const QString &description );
    ReosRainfallSerieRainfallItem( const ReosEncodedElement &element );

    ReosSerieRainfall *data() const override = 0;
    QString rainfallInformation() const;
    void setupData() override;
};

//! Class that represents time serie data item
class REOSCORE_EXPORT ReosRainfallGaugedRainfallItem: public ReosRainfallSerieRainfallItem
{
    Q_OBJECT
  public:
    ReosRainfallGaugedRainfallItem( const QString &name, const QString &description, ReosSerieRainfall *data = nullptr );
    ReosRainfallGaugedRainfallItem( const ReosEncodedElement &element );

    QString dataType() const override {return QStringLiteral( "gauged-rainfall" );}
    ReosSerieRainfall *data() const override;

    QIcon icone() const override {return QIcon( QPixmap( ":/images/gaugedRainfall.svg" ) );}
    virtual bool accept( ReosRainfallItem * ) const override;
    virtual ReosEncodedElement encode() const override;

  protected:
    ReosSerieRainfall *mData = nullptr;
};

class REOSCORE_EXPORT ReosRainfallChicagoItem: public ReosRainfallSerieRainfallItem
{
    Q_OBJECT
  public:
    ReosRainfallChicagoItem( const QString &name, const QString &description );
    ReosRainfallChicagoItem( const ReosEncodedElement &element );

    QString dataType() const override {return QStringLiteral( "chicago-rainfall" );}
    ReosChicagoRainfall *data() const override {return mData;}
    QIcon icone() const override {return QIcon( QPixmap( ":/images/chicagoRainfall.svg" ) );}
    virtual bool accept( ReosRainfallItem * ) const override {return false;}
    virtual ReosEncodedElement encode() const override;
    void setupData() override;
    void resolveDependencies() override;

  private slots:
    void setIntensityDurationCurveUniqueId( const QString &uid );
  private:
    ReosChicagoRainfall *mData = nullptr;
    QPointer<ReosRainfallIntensityDurationCurveItem> mCurveItem;
};

class REOSCORE_EXPORT ReosRainfallDoubleTriangleItem: public ReosRainfallSerieRainfallItem
{
    Q_OBJECT
  public:
    ReosRainfallDoubleTriangleItem( const QString &name, const QString &description );
    ReosRainfallDoubleTriangleItem( const ReosEncodedElement &element );

    QString dataType() const override {return QStringLiteral( "double-triangle-rainfall" );}
    ReosDoubleTriangleRainfall *data() const override {return mData;}
    QIcon icone() const override {return QIcon( QPixmap( ":/images/doubleTriangleRainfall.svg" ) );}
    virtual bool accept( ReosRainfallItem * ) const override {return false;}
    virtual ReosEncodedElement encode() const override;
    void setupData() override;
    void resolveDependencies() override;

  private slots:
    void setIntensityDurationCurveUniqueIds( const QString &intenseUid, const QString &totalUid );
  private:
    ReosDoubleTriangleRainfall *mData = nullptr;
    QPointer<ReosRainfallIntensityDurationCurveItem> mIntenseCurveItem;
    QPointer<ReosRainfallIntensityDurationCurveItem> mTotalCurveItem;
};


class REOSCORE_EXPORT ReosRainfallIdfCurvesItem: public ReosRainfallDataItem
{
    Q_OBJECT
  public:
    ReosRainfallIdfCurvesItem( const QString &name, const QString &description );
    ReosRainfallIdfCurvesItem( const ReosEncodedElement &element );

    QString dataType() const override {return QStringLiteral( "idf-curves" );}
    QIcon icone() const override {return QIcon( QPixmap( ":/images/intensityDurationCurves.svg" ) );}
    virtual bool accept( ReosRainfallItem *item ) const override;
    ReosIntensityDurationFrequencyCurves *data() const override;
    ReosEncodedElement encode() const override;
    void setupData() override;

    //! Returns position of the item
    int placeIdCurveItem( ReosRainfallIntensityDurationCurveItem *item );

    //! Returns the curve with index \a i
    ReosIntensityDurationCurve *curve( int i );

  private:
    ReosIntensityDurationFrequencyCurves *mData = nullptr;

};

class REOSCORE_EXPORT ReosRainfallIntensityDurationCurveItem: public ReosRainfallDataItem
{
    Q_OBJECT
  public:
    ReosRainfallIntensityDurationCurveItem( const ReosDuration &returnPeriod, const QString &name, const QString &description );
    ReosRainfallIntensityDurationCurveItem( const ReosEncodedElement &element );

    QString name() const override;
    QString dataType() const override {return QStringLiteral( "id-curve" );}
    QIcon icone() const override {return QIcon( QPixmap( ":/images/intensityDurationCurve.svg" ) );}
    virtual bool accept( ReosRainfallItem * ) const override {return false;}
    QList<ReosParameter *> parameters() const override;
    ReosIntensityDurationCurve *data() const override;
    ReosEncodedElement encode() const override;

  private:
    ReosIntensityDurationCurve *mIntensityDurationCurve = nullptr;
};


#endif // REOSRAINFALLITEM_H
