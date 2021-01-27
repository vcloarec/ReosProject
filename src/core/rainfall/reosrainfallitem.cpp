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
  return static_cast<int>( mChildItem.size() );
}

ReosRainfallItem *ReosRainfallItem::itemAt( int i ) const
{
  return mChildItem.at( static_cast<int>( i ) ).get();
}

bool ReosRainfallItem::hasChildItemName( const QString &itemName ) const
{
  for ( const std::unique_ptr<ReosRainfallItem> &other : mChildItem )
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
    for ( size_t i = 0; i < mParent->mChildItem.size(); ++i )
    {
      if ( mParent->mChildItem.at( i ).get() == this )
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

ReosRainfallItem *ReosRainfallItem::takeChild( int pos )
{
  if ( pos < 0 || pos >= static_cast<int>( mChildItem.size() ) )
    return nullptr;
  std::unique_ptr<ReosRainfallItem> item( mChildItem.at( static_cast<size_t>( pos ) ).release() );
  mChildItem.erase( mChildItem.begin() + pos );
  item->mParent = nullptr;
  return item.release();
}

void ReosRainfallItem::insertChild( int pos, ReosRainfallItem *item )
{
  mChildItem.emplace( mChildItem.begin() + pos, item );
  item->mParent = this;
}

bool ReosRainfallItem::isSubItem( ReosRainfallItem *item ) const
{
  for ( const std::unique_ptr<ReosRainfallItem> &child : mChildItem )
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

ReosRainfallDataItem::ReosRainfallDataItem( const QString &name, const QString &description ) :
  ReosRainfallItem( name, description, Data )
{}


ReosStationItem::ReosStationItem( const QString &name, const QString &description ): ReosRainfallItem( name, description, Station )
{}

ReosRainfallItem *ReosRainfallItem::addItem( ReosRainfallItem *item )
{
  mChildItem.emplace_back( item );
  item->mParent = this;
  return mChildItem.back().get();
}

ReosZoneItem::ReosZoneItem( const QString &name, const QString &descritpion ): ReosRainfallItem( name, descritpion, Zone )
{}

ReosRainfallItem *ReosZoneItem::addItem( ReosRainfallItem *item )
{
  return ReosRainfallItem::addItem( item );
}

ReosRootItem::ReosRootItem(): ReosRainfallItem( QString(), QString(), Root ) {}

ReosRainfallItem *ReosRootItem::addItem( ReosRainfallItem *item )
{
  return ReosRainfallItem::addItem( item );
}
