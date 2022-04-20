/***************************************************************************
  reoshydraulicscheme.cpp - ReosHydraulicScheme

 ---------------------
 begin                : 24.10.2021
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
#include "reoshydraulicscheme.h"
#include "reoshydraulicnetwork.h"
#include "reoswatershedmodule.h"
#include "reoscalculationcontext.h"

#include "reosmeteorologicmodel.h"

ReosHydraulicScheme::ReosHydraulicScheme( ReosHydraulicSchemeCollection *collection )
  : ReosDataObject( collection )
  , mSchemeName( new ReosParameterString( tr( "Scheme name" ), false, this ) )
  , mStartTime( new ReosParameterDateTime( tr( "Start time" ), this ) )
  , mEndTime( new ReosParameterDateTime( tr( "End time" ), this ) )
{
  init();
}

ReosHydraulicScheme::ReosHydraulicScheme( const ReosEncodedElement &element, ReosHydraulicSchemeCollection *collection, const ReosHydraulicNetworkContext &context )
  : ReosDataObject( collection )
  , mSchemeName( ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, tr( "Scheme name" ), this ) )
  , mStartTime( ReosParameterDateTime::decode( element.getEncodedData( QStringLiteral( "start-time" ) ), false, tr( "Start time" ), this ) )
  , mEndTime( ReosParameterDateTime::decode( element.getEncodedData( QStringLiteral( "end-time" ) ), false, tr( "End time" ), this ) )
{
  if ( context.network() && context.watershedModule() && context.watershedModule()->meteoModelsCollection() )
  {
    ReosMeteorologicModelsCollection *meteoModelCollect = context.watershedModule()->meteoModelsCollection();

    QString meteoModelId;
    element.getData( QStringLiteral( "meteo-model-id" ), meteoModelId );
    mMeteoModel = meteoModelCollect->meteorologicModel( meteoModelId );

    if ( !mMeteoModel && meteoModelCollect->modelCount() > 0 )
      mMeteoModel = meteoModelCollect->meteorologicModel( 0 );
  }

  element.getData( QStringLiteral( "elements-config" ), mElementsConfig );

  ReosDataObject::decode( element );

  init();
}

void ReosHydraulicScheme::init()
{
  connect( mStartTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mEndTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosMeteorologicModel *ReosHydraulicScheme::meteoModel() const
{
  return mMeteoModel;
}

void ReosHydraulicScheme::setMeteoModel( ReosMeteorologicModel *meteoModel )
{
  mMeteoModel = meteoModel;

  emit dataChanged();
}

void ReosHydraulicScheme::saveElementConfig( const QString &elementId, const ReosEncodedElement &encodedElement )
{
  mElementsConfig[elementId] = encodedElement.bytes();
}

ReosEncodedElement ReosHydraulicScheme::restoreElementConfig( const QString &elementId )
{
  if ( mElementsConfig.contains( elementId ) )
  {
    ReosEncodedElement ret( mElementsConfig.value( elementId ) );
    if ( ret.description() == elementId )
      return ret;
  }

  return ReosEncodedElement( elementId );
}

ReosCalculationContext ReosHydraulicScheme::calculationContext() const
{
  ReosCalculationContext context;
  context.setMeteorologicModel( meteoModel() );
  context.setSimulationStartTime( startTime()->value() );
  context.setSimulationEndTime( endTime()->value() );
  context.setSchemeId( id() );

  return context;
}

ReosParameterString *ReosHydraulicScheme::schemeName() const
{
  return mSchemeName;
}

ReosParameterDateTime *ReosHydraulicScheme::startTime() const
{
  return mStartTime;
}

ReosParameterDateTime *ReosHydraulicScheme::endTime() const
{
  return mEndTime;
}

ReosEncodedElement ReosHydraulicScheme::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydraulic-scheme" ) );

  element.addEncodedData( QStringLiteral( "name" ), mSchemeName->encode() );
  element.addEncodedData( QStringLiteral( "start-time" ), mStartTime->encode() );
  element.addEncodedData( QStringLiteral( "end-time" ), mEndTime->encode() );
  element.addData( QStringLiteral( "meteo-model-id" ), mMeteoModel->id() );

  element.addData( QStringLiteral( "elements-config" ), mElementsConfig );

  ReosDataObject::encode( element );

  return element;
}

ReosHydraulicScheme *ReosHydraulicScheme::decode( const ReosEncodedElement &element, ReosHydraulicSchemeCollection *parent, const ReosHydraulicNetworkContext &context )
{
  if ( element.description() != QStringLiteral( "hydraulic-scheme" ) )
    return nullptr;

  return new ReosHydraulicScheme( element, parent, context );
}

ReosHydraulicSchemeCollection::ReosHydraulicSchemeCollection( QObject *parent )
  : QAbstractListModel( parent )
{
}


ReosEncodedElement ReosHydraulicSchemeCollection::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydraulic-scheme-collection" ) );
  QList<ReosEncodedElement> encodedList;

  for ( ReosHydraulicScheme *scheme : mHydraulicSchemes )
    encodedList.append( scheme->encode() );

  element.addListEncodedData( QStringLiteral( "schemes" ), encodedList );
  return element;
}

void ReosHydraulicSchemeCollection::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != QStringLiteral( "hydraulic-scheme-collection" ) )
    return;

  const QList<ReosEncodedElement> encodedList = encodedElement.getListEncodedData( QStringLiteral( "schemes" ) );

  for ( const ReosEncodedElement &elem : encodedList )
  {
    ReosHydraulicScheme *scheme = ReosHydraulicScheme::decode( elem, this, context );
    if ( scheme )
      mHydraulicSchemes.append( scheme );
  }
}

QModelIndex ReosHydraulicSchemeCollection::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosHydraulicSchemeCollection::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosHydraulicSchemeCollection::rowCount( const QModelIndex & ) const
{
  return mHydraulicSchemes.count();
}

int ReosHydraulicSchemeCollection::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosHydraulicSchemeCollection::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( role == Qt::DisplayRole && index.row() < mHydraulicSchemes.count() )
    return mHydraulicSchemes.at( index.row() )->schemeName()->value();

  return QVariant();
}

void ReosHydraulicSchemeCollection::addScheme( ReosHydraulicScheme *scheme )
{
  beginResetModel();
  scheme->setParent( this );
  mHydraulicSchemes.append( scheme );
  endResetModel();
}

void ReosHydraulicSchemeCollection::removeScheme( int index )
{
  beginResetModel();
  mHydraulicSchemes.at( index )->deleteLater();
  mHydraulicSchemes.removeAt( index );
  endResetModel();
}

void ReosHydraulicSchemeCollection::clear()
{
  beginResetModel();
  for ( ReosHydraulicScheme *scheme : std::as_const( mHydraulicSchemes ) )
    scheme->deleteLater();
  mHydraulicSchemes.clear();
  endResetModel();
}

int ReosHydraulicSchemeCollection::schemeCount() const
{
  return mHydraulicSchemes.count();
}

ReosHydraulicScheme *ReosHydraulicSchemeCollection::scheme( int index )
{
  if ( index < 0 || index > mHydraulicSchemes.count() )
    return nullptr;

  return mHydraulicSchemes.at( index );
}

