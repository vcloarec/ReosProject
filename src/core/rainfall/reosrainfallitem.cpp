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

#include "reosidfcurves.h"
#include "reossyntheticrainfall.h"
#include "reosparameter.h"
#include "reosrainfallregistery.h"
#include "reosrainfallmodel.h"

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

ReosEncodedElement ReosZoneItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "zone-item" ) );
  ReosRainfallItem::encodeBase( element );
  return element;
}

bool ReosStationItem::accept( ReosRainfallItem *item ) const
{
  return ( item &&
           item->type() == ReosRainfallItem::Data &&
           ReosRainfallItem::accept( item ) );
}

ReosEncodedElement ReosStationItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "station-item" ) );
  ReosRainfallItem::encodeBase( element );
  return element;
}

ReosRainfallItem::~ReosRainfallItem() = default;

QString ReosRainfallItem::name() const {return mName->value();}

QString ReosRainfallItem::description() {return mDescription->value();}

int ReosRainfallItem::childrenCount() const
{
  return static_cast<int>( mChildItems.size() );
}

ReosRainfallItem *ReosRainfallItem::itemAt( int i ) const
{
  if ( i < 0 || i >= static_cast<int>( mChildItems.size() ) )
    return nullptr;

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
  ret << mName;
  ret << mDescription;

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

QString ReosRainfallItem::uri() const
{
  QList<int> numericPath = positionPathInTree();
  QStringList strList;
  for ( int i : std::as_const( numericPath ) )
    strList.append( QString::number( i ) );
  QString uri = strList.join( ':' );

  return uri;
}

QString ReosRainfallItem::uniqueId() const
{
  return mUid;
}

ReosRainfallItem *ReosRainfallItem::searchForChildWithUniqueId( const QString &uid ) const
{
  for ( const std::unique_ptr<ReosRainfallItem> &childItem : mChildItems )
  {
    if ( childItem->uniqueId() == uid )
      return childItem.get();

    ReosRainfallItem *subChildMatch = childItem->searchForChildWithUniqueId( uid );

    if ( subChildMatch )
      return subChildMatch;
  }

  return nullptr;
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
  element.addEncodedData( QStringLiteral( "description" ), mDescription->encode() );
  element.addData( QStringLiteral( "uid" ), mUid );

  QList<ReosEncodedElement> encodedChildren;

  for ( const std::unique_ptr<ReosRainfallItem> &ri : mChildItems )
    encodedChildren.append( ri->encode() );

  element.addListEncodedData( QStringLiteral( "children" ), encodedChildren );
}

void ReosRainfallItem::swapChildren( int first, int second )
{
  if ( first < 0 || first >= childrenCount() || second < 0 || second >= childrenCount() )
    return;

  std::unique_ptr<ReosRainfallItem> temp;
  temp.reset( mChildItems.at( first ).release() );
  mChildItems.at( first ).reset( mChildItems.at( second ).release() );
  mChildItems.at( second ).reset( temp.release() );
}

ReosRainfallItem *ReosRainfallItem::takeChild( int pos )
{
  if ( pos < 0 || pos >= static_cast<int>( mChildItems.size() ) )
    return nullptr;
  emit itemWillBeRemovedfromParent( this, pos );
  std::unique_ptr<ReosRainfallItem> item( mChildItems.at( static_cast<size_t>( pos ) ).release() );
  mChildItems.erase( mChildItems.begin() + pos );
  disconnect( item.get(), &ReosRainfallItem::changed, this, &ReosRainfallItem::onChildChanged );
  item->mParent = nullptr;
  emit itemRemovedfromParent();
  return item.release();
}

void ReosRainfallItem::removeItem( ReosRainfallItem *item )
{
  for ( size_t i = 0; i < mChildItems.size(); ++i )
  {
    if ( mChildItems.at( i ).get() == item )
    {
      mChildItems.erase( mChildItems.begin() + i );
      break;
    }
  }
}

void ReosRainfallItem::insertChild( int pos, ReosRainfallItem *item )
{
  emit itemWillBeInsertedInParent( this, pos );
  mChildItems.emplace( mChildItems.begin() + pos, item );
  item->mParent = this;
  connect( item, &ReosRainfallItem::changed, this, &ReosRainfallItem::onChildChanged );
  emit itemInsertedInParent();
}

bool ReosRainfallItem::isSubItem( ReosRainfallItem *item ) const
{
  for ( const std::unique_ptr<ReosRainfallItem> &child : mChildItems )
  {
    if ( child.get() == item /*|| child->isSubItem( item )*/ )
      return true;
  }

  return false;
}

ReosRainfallItem::ReosRainfallItem( const QString &name, const QString &description, ReosRainfallItem::Type type ):
  QObject()
  , mName( new ReosParameterString( QObject::tr( "Name" ), false, this ) )
  , mDescription( new ReosParameterLongString( QObject::tr( "Description" ), false, this ) )
  , mUid( QUuid::createUuid().toString() )
  , mType( type )
{
  mName->setValue( name );
  mDescription->setValue( description );
  connectParameters();
}

ReosRainfallItem::ReosRainfallItem( const ReosEncodedElement &element, ReosRainfallItem::Type type ):
  mName( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, this ) )
  , mDescription( ReosParameterLongString::decode( element.getEncodedData( QStringLiteral( "description" ) ), false, this ) )
  , mType( type )
{

  element.getData( QStringLiteral( "uid" ), mUid );
  if ( mUid.isEmpty() )
    mUid = QUuid::createUuid().toString();

  connectParameters();
}

void ReosRainfallItem::connectParameters()
{
  connect( mName, &ReosParameter::valueChanged, this, [this] {emit changed( this );} );
  connect( mDescription, &ReosParameter::valueChanged, this, [this] {emit changed( this );} );
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
  for ( const ReosEncodedElement &childElem : std::as_const( encodedChildren ) )
  {
    if ( childElem.description() == QStringLiteral( "rainfall-serie-item" ) )
      addItem( new ReosRainfallGaugedRainfallItem( childElem ) );

    if ( childElem.description() == QStringLiteral( "idf-item" ) )
      addItem( new ReosRainfallIdfCurvesItem( childElem ) );

    if ( childElem.description() == QStringLiteral( "chicago-rainfall-item" ) )
      addItem( new ReosRainfallChicagoItem( childElem ) );

    if ( childElem.description() == QStringLiteral( "double-triangle-rainfall-item" ) )
      addItem( new ReosRainfallDoubleTriangleItem( childElem ) );
  }
}

ReosRainfallItem *ReosRainfallItem::addItem( ReosRainfallItem *item )
{
  mChildItems.emplace_back( item );
  item->mParent = this;
  connect( item, &ReosRainfallItem::changed, this, &ReosRainfallItem::onChildChanged );
  return mChildItems.back().get();
}

ReosZoneItem::ReosZoneItem( const QString &name, const QString &descritpion ): ReosRainfallItem( name, descritpion, Zone )
{}

ReosZoneItem::ReosZoneItem( const ReosEncodedElement &element ): ReosRainfallItem( element, Zone )
{
  if ( element.description() != QStringLiteral( "zone-item" ) )
    return;

  QList<ReosEncodedElement> encodedChildren = element.getListEncodedData( QStringLiteral( "children" ) );
  for ( const ReosEncodedElement &childElem : std::as_const( encodedChildren ) )
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

ReosRainfallGaugedRainfallItem::ReosRainfallGaugedRainfallItem( const QString &name, const QString &description, ReosSerieRainfall *data ):
  ReosRainfallSerieRainfallItem( name, description )
  , mData( data )
{
  if ( !mData )
    mData = new ReosSerieRainfall( this );
  else
    mData->setParent( this );
}

ReosRainfallGaugedRainfallItem::ReosRainfallGaugedRainfallItem( const ReosEncodedElement &element ):
  ReosRainfallSerieRainfallItem( element )
{
  if ( element.description() != QStringLiteral( "rainfall-serie-item" ) )
    return;

  mData = ReosSerieRainfall::decode( element.getEncodedData( "data" ), this );
}

ReosSerieRainfall *ReosRainfallGaugedRainfallItem::data() const
{
  return mData;
}

ReosEncodedElement ReosRainfallGaugedRainfallItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "rainfall-serie-item" ) );
  ReosRainfallItem::encodeBase( element );

  element.addEncodedData( QStringLiteral( "data" ), mData->encode() );
  return element;
}

bool ReosRainfallGaugedRainfallItem::accept( ReosRainfallItem * ) const
{
  return false;
}


ReosRainfallIdfCurvesItem::ReosRainfallIdfCurvesItem( const QString &name, const QString &description ):
  ReosRainfallDataItem( name, description )
{}

ReosRainfallIdfCurvesItem::ReosRainfallIdfCurvesItem( const ReosEncodedElement &element ):
  ReosRainfallDataItem( element )
{
  if ( element.description() != QStringLiteral( "idf-item" ) )
    return;

  QList<ReosEncodedElement> encodedChildren = element.getListEncodedData( QStringLiteral( "children" ) );
  for ( const ReosEncodedElement &childElem : std::as_const( encodedChildren ) )
  {
    if ( childElem.description() == QStringLiteral( "intensity-duration-item" ) )
      addItem( new ReosRainfallIntensityDurationCurveItem( childElem ) );
  }
}

bool ReosRainfallIdfCurvesItem::accept( ReosRainfallItem *item ) const
{
  ReosRainfallDataItem *dataItem = qobject_cast<ReosRainfallDataItem *>( item );

  return dataItem && dataItem->dataType() == QStringLiteral( "id-curve" );
}

ReosIntensityDurationFrequencyCurves *ReosRainfallIdfCurvesItem::data() const
{
  return mData;
}

ReosEncodedElement ReosRainfallIdfCurvesItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "idf-item" ) );
  ReosRainfallItem::encodeBase( element );
  return element;
}

void ReosRainfallIdfCurvesItem::setupData()
{
  if ( mData )
    mData->deleteLater();
  mData = new ReosIntensityDurationFrequencyCurves( this );

  for ( int i = 0; i < childrenCount(); ++i )
  {
    ReosRainfallIntensityDurationCurveItem *item = qobject_cast<ReosRainfallIntensityDurationCurveItem *>( itemAt( i ) );
    if ( item && item->data() )
    {
      item->data()->setupFormula( ReosIdfFormulaRegistery::instance() );
      mData->addCurve( item->data(), item->name() );
    }
  }
}

int ReosRainfallIdfCurvesItem::placeIdCurveItem( ReosRainfallIntensityDurationCurveItem *item )
{
  int i = 0;
  bool found = false;
  while ( i < childrenCount() && !found )
  {
    ReosRainfallIntensityDurationCurveItem *otherItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>( itemAt( i ) );
    found = ( !otherItem || item->data()->returnPeriod()->value() <= otherItem->data()->returnPeriod()->value() ) ;
    if ( !found )
      ++i;
  }

  insertChild( i, item );

  return i;
}

ReosIntensityDurationCurve *ReosRainfallIdfCurvesItem::curve( int i )
{
  if ( i < 0 || i >= childrenCount() )
    return nullptr;

  ReosRainfallIntensityDurationCurveItem *item = qobject_cast<ReosRainfallIntensityDurationCurveItem *>( itemAt( i ) );
  if ( !item )
    return nullptr;

  return item->data();
}

ReosRainfallIntensityDurationCurveItem::ReosRainfallIntensityDurationCurveItem( const ReosDuration &returnPeriod, const QString &name, const QString &description ):
  ReosRainfallDataItem( name, description )
{
  mIntensityDurationCurve = new ReosIntensityDurationCurve( returnPeriod, this );
  connect( mIntensityDurationCurve, &ReosDataObject::dataChanged, this, [this] { emit ReosRainfallDataItem::changed( this );} );
}

ReosRainfallIntensityDurationCurveItem::ReosRainfallIntensityDurationCurveItem( const ReosEncodedElement &element ):
  ReosRainfallDataItem( element )
{
  if ( element.description() != QStringLiteral( "intensity-duration-item" ) )
    return;
  mIntensityDurationCurve = ReosIntensityDurationCurve::decode( element.getEncodedData( QStringLiteral( "curve" ) ), this );
  if ( ReosIdfFormulaRegistery::isInstantiate() )
    mIntensityDurationCurve->setupFormula( ReosIdfFormulaRegistery::instance() );
  connect( mIntensityDurationCurve, &ReosDataObject::dataChanged, this, [this] { emit ReosRainfallDataItem::changed( this );} );
}

QString ReosRainfallIntensityDurationCurveItem::name() const
{
  return data()->returnPeriod()->toString( 0 );
}

QList<ReosParameter *> ReosRainfallIntensityDurationCurveItem::parameters() const
{
  QList<ReosParameter *> ret = ReosRainfallItem::parameters();
  // for this ite we don"t want the name that is defined with the return period
  //no very clean but do the job
  for ( int i = 0; i < ret.count(); ++i )
  {
    if ( ret.at( i )->name() == tr( "Name" ) )
    {
      ret.removeAt( i );
      break;
    }
  }

  if ( mIntensityDurationCurve )
    ret.append( mIntensityDurationCurve->returnPeriod() );
  return ret;
}

ReosIntensityDurationCurve *ReosRainfallIntensityDurationCurveItem::data() const
{
  return mIntensityDurationCurve;
}

ReosEncodedElement ReosRainfallIntensityDurationCurveItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "intensity-duration-item" ) );
  ReosRainfallItem::encodeBase( element );
  element.addEncodedData( QStringLiteral( "curve" ), mIntensityDurationCurve->encode() );
  return element;
}

ReosRainfallChicagoItem::ReosRainfallChicagoItem( const QString &name, const QString &description ):
  ReosRainfallSerieRainfallItem( name, description )
{
  mData = new ReosChicagoRainfall( this );
  connect( mData, &ReosChicagoRainfall::newIntensityDuration, this, &ReosRainfallChicagoItem::setIntensityDurationCurveUniqueId );
  connectParameters();
}

ReosRainfallChicagoItem::ReosRainfallChicagoItem( const ReosEncodedElement &element ): ReosRainfallSerieRainfallItem( element )
{
  mData = ReosChicagoRainfall::decode( element.getEncodedData( QStringLiteral( "chicago-rainfall-data" ) ) );
  QString curveItemUniqueId;
  if ( element.getData( QStringLiteral( "curve-item-unique-id" ), curveItemUniqueId ) )
  {
    mData->setIntensityDurationUid( curveItemUniqueId );
  }

  connect( mData, &ReosChicagoRainfall::newIntensityDuration, this, &ReosRainfallChicagoItem::setIntensityDurationCurveUniqueId );
  connectParameters();
}

ReosEncodedElement ReosRainfallChicagoItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "chicago-rainfall-item" ) );

  encodeBase( element );

  QString curveItemUniqueId;
  if ( !mCurveItem.isNull() )
    curveItemUniqueId = mCurveItem->uniqueId();
  element.addData( QStringLiteral( "curve-item-unique-id" ), curveItemUniqueId );

  element.addEncodedData( QStringLiteral( "chicago-rainfall-data" ), mData->encode() );

  return element;
}

void ReosRainfallChicagoItem::setupData()
{
  ReosRainfallSerieRainfallItem::setupData();

  if ( mCurveItem )
    data()->setIntensityDurationUid( mCurveItem->uniqueId() );

  connect( this, &ReosRainfallItem::changed, data(), [this]
  {
    if ( this->data() )
      this->data()->setName( this->name() );
  } );
}

void ReosRainfallChicagoItem::resolveDependencies()
{
  if ( ReosRainfallRegistery::isInstantiate() && mData )
  {
    mCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( mData->intensityDurationUid() ) );
    if ( mCurveItem )
      mData->setIntensityDurationCurve( mCurveItem->data() );
  }
}

void ReosRainfallChicagoItem::setIntensityDurationCurveUniqueId( const QString &uid )
{
  if ( !ReosRainfallRegistery::isInstantiate() )
    return;

  mCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>
               ( ReosRainfallRegistery::instance()->rainfallModel()->uniqueIdToItem( uid ) );
}


ReosRainfallDoubleTriangleItem::ReosRainfallDoubleTriangleItem( const QString &name, const QString &description ):
  ReosRainfallSerieRainfallItem( name, description )
{
  mData = new ReosDoubleTriangleRainfall( this );
  connect( mData, &ReosDoubleTriangleRainfall::newIntensityDuration, this, &ReosRainfallDoubleTriangleItem::setIntensityDurationCurveUniqueIds );
  connectParameters();
}

ReosRainfallDoubleTriangleItem::ReosRainfallDoubleTriangleItem( const ReosEncodedElement &element ): ReosRainfallSerieRainfallItem( element )
{
  mData = ReosDoubleTriangleRainfall::decode( element.getEncodedData( QStringLiteral( "double-triangle-rainfall-data" ) ) );
  QString curveItemIntenseUid;
  QString curveItemTotalUid;
  if ( element.getData( QStringLiteral( "intense-curve-item-unique-id" ), curveItemIntenseUid ) &&
       element.getData( QStringLiteral( "total-curve-item-unique-id" ), curveItemTotalUid ) )
  {
    mData->setIntensityDurationUniqueId( curveItemIntenseUid, curveItemTotalUid );
  }
  connect( mData, &ReosDoubleTriangleRainfall::newIntensityDuration, this, &ReosRainfallDoubleTriangleItem::setIntensityDurationCurveUniqueIds );
  connectParameters();
}

ReosEncodedElement ReosRainfallDoubleTriangleItem::encode() const
{
  ReosEncodedElement element( QStringLiteral( "double-triangle-rainfall-item" ) );

  encodeBase( element );

  QString curveItemIntenseUniqueId;
  QString curveItemTotalUniqueId;
  if ( !mIntenseCurveItem.isNull() )
    curveItemIntenseUniqueId = mIntenseCurveItem->uniqueId();
  if ( !mTotalCurveItem.isNull() )
    curveItemTotalUniqueId = mTotalCurveItem->uniqueId();

  element.addData( QStringLiteral( "intense-curve-item-unique-id" ), curveItemIntenseUniqueId );
  element.addData( QStringLiteral( "total-curve-item-unique-id" ), curveItemTotalUniqueId );
  element.addEncodedData( QStringLiteral( "double-triangle-rainfall-data" ), mData->encode() );

  return element;
}

void ReosRainfallDoubleTriangleItem::setupData()
{
  ReosRainfallSerieRainfallItem::setupData();

  if ( mIntenseCurveItem && mTotalCurveItem )
    mData->setIntensityDurationUniqueId( mIntenseCurveItem->uniqueId(), mTotalCurveItem->uniqueId() );
}

void ReosRainfallDoubleTriangleItem::resolveDependencies()
{
  if ( ReosRainfallRegistery::isInstantiate() && mData )
  {
    mIntenseCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>(
                          ReosRainfallRegistery::instance()->itemByUniqueId( mData->intensityDurationUniqueIdIntense() ) );
    mTotalCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>(
                        ReosRainfallRegistery::instance()->itemByUniqueId( mData->intensityDurationUniqueIdTotal() ) );
    if ( mIntenseCurveItem && mTotalCurveItem )
      mData->setIntensityDurationCurve( mIntenseCurveItem->data(), mTotalCurveItem->data() );
  }
}

void ReosRainfallDoubleTriangleItem::setIntensityDurationCurveUniqueIds( const QString &intenseUid, const QString &totalUid )
{
  if ( !ReosRainfallRegistery::isInstantiate() )
    return;

  mIntenseCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>
                      ( ReosRainfallRegistery::instance()->rainfallModel()->uniqueIdToItem( intenseUid ) );

  mTotalCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>
                    ( ReosRainfallRegistery::instance()->rainfallModel()->uniqueIdToItem( totalUid ) );
}

ReosRainfallSerieRainfallItem::ReosRainfallSerieRainfallItem( const QString &name, const QString &description ):
  ReosRainfallDataItem( name, description )
{}

ReosRainfallSerieRainfallItem::ReosRainfallSerieRainfallItem( const ReosEncodedElement &element ):
  ReosRainfallDataItem( element )
{}

QString ReosRainfallSerieRainfallItem::rainfallInformation() const
{
  QString ret;
  ret.append( tr( "Name: %1" ).arg( name() ) );
  ret.append( "\n" );
  ret.append( tr( "Type: %1" ).arg( dataType() ) );
  ret.append( "\n" );
  QPair<QDateTime, QDateTime> timeExtent = data()->timeExtent();
  ReosDuration duration( timeExtent.first.msecsTo( timeExtent.second ) );
  duration.setAdaptedUnit();
  ret.append( tr( "Duration: %1" ).arg( duration.toString( 2 ) ) );
  ret.append( "\n" );
  ret.append( tr( "Time Step: %1" ).arg( data()->timeStep()->value().toString( 2 ) ) );
  ret.append( "\n" );
  double cumul = data()->valueWithMode( data()->valueCount(), ReosTimeSerieConstantInterval::Cumulative );
  ret.append( tr( "Cumulative height: %1 mm" ).arg( QString::number( cumul, 'f', 2 ) ) );
  return ret;
}

void ReosRainfallSerieRainfallItem::setupData()
{
  if ( !data() )
    return;

  data()->setAddCumulative( true );
  data()->setName( name() );

  connect( this, &ReosRainfallItem::changed, data(), [this]
  {
    if ( this->data() )
      this->data()->setName( this->name() );
  } );
}
