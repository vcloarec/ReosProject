/***************************************************************************
  reoshydrograph.cpp
 ---------------------
 begin                : 19.5.2021
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

#include "reoshydrograph.h"


QColor ReosHydrograph::color() const
{
  return mColor;
}

void ReosHydrograph::setColor( const QColor &color )
{
  mColor = color;
}

ReosEncodedElement ReosHydrograph::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydrograph" ) );
  ReosTimeSerieVariableTimeStep::baseEncode( element );
  element.addData( QStringLiteral( "color" ), mColor );
  return element;
}

ReosHydrograph *ReosHydrograph::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "hydrograph" ) )
    return nullptr;

  std::unique_ptr<ReosHydrograph> ret = std::make_unique<ReosHydrograph>( parent );
  element.getData( QStringLiteral( "color" ), ret->mColor );
  ret->decodeBase( element );

  return ret.release();
}

void ReosHydrographStore::addHydrograph( ReosHydrograph *hydrograph )
{
  hydrograph->setParent( this );
  mHydrographs.append( hydrograph );

  emit dataChanged();
}

void ReosHydrographStore::removeHydrograph( int index )
{
  delete mHydrographs.takeAt( index );
  emit dataChanged();
}

int ReosHydrographStore::hydrographCount() const
{
  return mHydrographs.count();
}

QStringList ReosHydrographStore::hydrographNames() const
{
  QStringList ret;
  for ( const ReosHydrograph *hyd : mHydrographs )
    ret.append( hyd->name() );

  return ret;
}

ReosHydrograph *ReosHydrographStore::hydrograph( int index ) const
{
  if ( index >= 0 && index < mHydrographs.count() )
    return mHydrographs.at( index );

  return nullptr;
}

ReosEncodedElement ReosHydrographStore::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydrograph-store" ) );

  QList<QByteArray> encodedHydrographs;
  encodedHydrographs.reserve( mHydrographs.count() );
  for ( const ReosHydrograph *hyd : mHydrographs )
    encodedHydrographs.append( hyd->encode().bytes() );

  element.addData( "hydrographs", encodedHydrographs );

  return element;
}

void ReosHydrographStore::decode( const ReosEncodedElement &element )
{
  qDeleteAll( mHydrographs );
  mHydrographs.clear();

  if ( element.description() != QStringLiteral( "hydrograph-store" ) )
    return;

  QList<QByteArray> encodedHydrographs;
  if ( !element.getData( "hydrographs", encodedHydrographs ) )
    return;

  mHydrographs.reserve( encodedHydrographs.count() );
  for ( const QByteArray &bytes : std::as_const( encodedHydrographs ) )
    mHydrographs.append( ReosHydrograph::decode( ReosEncodedElement( bytes ), this ) );

}
