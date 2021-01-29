/***************************************************************************
  reosrainfallitem.cpp - ReosRainfallItem

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
#include "reosrainfallitem.h"

#include "reosparameter.h"

bool ReosRootItem::accept( ReosRainfallItem *item ) const
{
  return ( item &&
           ( item->type() == ReosRainfallItem::Station || item->type() == ReosRainfallItem::Zone ) &&
           ReosRainfallItem::accept( item ) );
}

ReosEncodedElement ReosRootItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "root-item" ) );
  ReosRainfallItem::encodeBase( element );
  return element;
}

bool ReosZoneItem::accept( ReosRainfallItem *item ) const
{

  return ( item &&
           ( item->type() == ReosRainfallItem::Station || item->type() == ReosRainfallItem::Zone ) &&
           ReosRainfallItem::accept( item ) );
}

bool ReosStationItem::accept( ReosRainfallItem *item ) const
{
  return ( item &&
           item->type() == ReosRainfallItem::Data &&
           ReosRainfallItem::accept( item ) );
}

ReosRainfallItem::~ReosRainfallItem() = default;

QString ReosRainfallItem::name() {return mName->value();}

QString ReosRainfallItem::description() {return mDescription->value();}

int ReosRainfallItem::childrenCount() const
{
  return static_cast<int>( mChildItems.size() );
}

ReosRainfallItem *ReosRainfallItem::itemAt( int i ) const
{
  return mChildItems.at( static_cast<int>( i ) ).get();
}

bool ReosRainfallItem::hasChildItemName( const QString &itemName ) const
{
  for ( const std::unique_ptr<ReosRainfallItem> &other : mChildItems )
    if ( other->name() == itemName )
      return true;

  return false;
}

QList<ReosParameter *> ReosRainfallItem::parameters() const
{
  QList<ReosParameter *> ret;
  ret << mName.get();
  ret << mDescription.get();

  return ret;
}

QList<int> ReosRainfallItem::positionPathInTree() const
{
  QList<int> ret;
  if ( mParent )
  {
    ret = mParent->positionPathInTree();
    ret.append( positionInParent() );
  }

  return ret;
}

int ReosRainfallItem::positionInParent() const
{
  int pos = -1;
  if ( mParent )
  {
    for ( size_t i = 0; i < mParent->mChildItems.size(); ++i )
    {
      if ( mParent->mChildItems.at( i ).get() == this )
      {
        pos = static_cast<int>( i );
        break;
      }
    }
  }
  return pos;
}

bool ReosRainfallItem::accept( ReosRainfallItem *item ) const
{
  return ( ! hasChildItemName( item->name() ) ) ;
}

void ReosRainfallItem::clear()
{
  mChildItems.clear();
}

void ReosRainfallItem::encodeBase( ReosEncodedElement &element ) const
{
  element.addEncodedData( QStringLiteral( "name" ), mName->encode() );
  element.addEncodedData( QStringLiteral( "decription" ), mDescription->encode() );

  QList<ReosEncodedElement> encodedChildren;

  for ( const std::unique_ptr<ReosRainfallItem> &ri : mChildItems )
    encodedChildren.append( ri->encode() );

  element.addListEncodedData( QStringLiteral( "children" ), encodedChildren );
}

ReosRainfallItem *ReosRainfallItem::takeChild( int pos )
{
  if ( pos < 0 || pos >= static_cast<int>( mChildItems.size() ) )
    return nullptr;
  std::unique_ptr<ReosRainfallItem> item( mChildItems.at( static_cast<size_t>( pos ) ).release() );
  mChildItems.erase( mChildItems.begin() + pos );
  item->mParent = nullptr;
  return item.release();
}

void ReosRainfallItem::insertChild( int pos, ReosRainfallItem *item )
{
  mChildItems.emplace( mChildItems.begin() + pos, item );
  item->mParent = this;
}

bool ReosRainfallItem::isSubItem( ReosRainfallItem *item ) const
{
  for ( const std::unique_ptr<ReosRainfallItem> &child : mChildItems )
  {
    if ( child.get() == item || child->isSubItem( item ) )
      return true;
  }

  return false;
}

ReosRainfallItem::ReosRainfallItem( const QString &name, const QString &description, ReosRainfallItem::Type type ):
  QObject()
  , mName( new ReosParameterString( QObject::tr( "Name" ) ) )
  , mDescription( new ReosParameterString( QObject::tr( "Description" ) ) )
  , mType( type )
{
  mName->setValue( name );
  mDescription->setValue( description );

  connect( mName.get(), &ReosParameter::valueChanged, this, [this] {emit changed( this );} );
  connect( mDescription.get(), &ReosParameter::valueChanged, this, [this] {emit changed( this );} );
}

ReosRainfallItem::ReosRainfallItem( const ReosEncodedElement &element, ReosRainfallItem::Type type ): mType( type )
{
  mName.reset( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, this ) );
  mDescription.reset( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "description" ) ), false, this ) );
}

ReosRainfallDataItem::ReosRainfallDataItem( const QString &name, const QString &description ) :
  ReosRainfallItem( name, description, Data )
{}


ReosStationItem::ReosStationItem( const QString &name, const QString &description ): ReosRainfallItem( name, description, Station )
{}

ReosStationItem::ReosStationItem( const ReosEncodedElement &element ): ReosRainfallItem( element, Station )
{
  if ( element.description() != QStringLiteral( "station-item" ) )
    return;

  QList<ReosEncodedElement> encodedChildren = element.getListEncodedData( QStringLiteral( "children" ) );
  for ( const ReosEncodedElement &childElem : qAsConst( encodedChildren ) )
  {
    if ( childElem.description() == QStringLiteral( "rainfall-serie-item" ) )
      addItem( new ReosRainfallSeriesItem( childElem ) );
  }
}

ReosRainfallItem *ReosRainfallItem::addItem( ReosRainfallItem *item )
{
  mChildItems.emplace_back( item );
  item->mParent = this;
  return mChildItems.back().get();
}

ReosZoneItem::ReosZoneItem( const QString &name, const QString &descritpion ): ReosRainfallItem( name, descritpion, Zone )
{}

ReosZoneItem::ReosZoneItem( const ReosEncodedElement &element ): ReosRainfallItem( element, Zone )
{
  if ( element.description() != QStringLiteral( "zone-item" ) )
    return;

  QList<ReosEncodedElement> encodedChildren = element.getListEncodedData( QStringLiteral( "children" ) );
  for ( const ReosEncodedElement &childElem : qAsConst( encodedChildren ) )
  {
    if ( childElem.description() == QStringLiteral( "zone-item" ) )
      addItem( new ReosZoneItem( childElem ) );

    if ( childElem.description() == QStringLiteral( "station-item" ) )
      addItem( new ReosStationItem( childElem ) );
  }
}

ReosRootItem::ReosRootItem(): ReosRainfallItem( QString(), QString(), Root ) {}

ReosRootItem::ReosRootItem( const ReosEncodedElement &element ): ReosRainfallItem( element, Root )
{
  if ( element.description() != QStringLiteral( "root-item" ) )
    return;

  const QList<ReosEncodedElement> encodedChildren = element.getListEncodedData( QStringLiteral( "children" ) );
  for ( const ReosEncodedElement &childElem : encodedChildren )
  {
    if ( childElem.description() == QStringLiteral( "zone-item" ) )
      addItem( new ReosZoneItem( childElem ) );

    if ( childElem.description() == QStringLiteral( "rainfall-station-item" ) )
      addItem( new ReosStationItem( childElem ) );
  }

}

ReosRainfallSeriesItem::ReosRainfallSeriesItem( const QString &name, const QString &description ):
  ReosRainfallDataItem( name, description )
{
  mData = new ReosTimeSerieConstantInterval( this );
}

ReosRainfallSeriesItem::ReosRainfallSeriesItem( const ReosEncodedElement &element ):
  ReosRainfallDataItem( element )
{
  if ( element.description() != QStringLiteral( "rainfall-serie-item" ) )
    return;

  mData = ReosTimeSerieConstantInterval::decode( element.getEncodedData( "data" ), this );
}

ReosTimeSerieConstantInterval *ReosRainfallSeriesItem::data() const
{
  return mData;
}
