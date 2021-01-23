/***************************************************************************
  reosparameter.cpp - ReosParameter

 ---------------------
 begin                : 22.1.2021
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
#include "reosparameter.h"


ReosParameter::ReosParameter( const QString &name, QObject *parent ):
  QObject( parent )
  , mName( name )
{

}

QString ReosParameter::name() const
{
  return mName;
}

bool ReosParameter::isDerivable() const
{
  return mIsDerivable;
}

bool ReosParameter::isDerived() const
{
  return mIsDerived;
}

void ReosParameter::encode( ReosEncodedElement &element ) const
{
  element.addData( QStringLiteral( "name" ), mName );
  element.addData( QStringLiteral( "is-derived" ), mIsDerived );
}

void ReosParameter::decode( const ReosEncodedElement &element, bool isDerivable )
{
  mIsDerivable = isDerivable;

  if ( !element.getData( QStringLiteral( "name" ), mName ) )
    return;

  if ( !element.getData( QStringLiteral( "is-derived" ), mIsDerived ) )
    return;
}


ReosParameterArea::ReosParameterArea( const QString &name, QObject *parent ):
  ReosParameter( name, parent )
{}

void ReosParameterArea::setValue( const ReosArea &area )
{
  mValue = area;
  mIsDerived = false;
}

void ReosParameterArea::setDerivedValue( const ReosArea &area )
{
  mValue = area;
  mIsDerived = true;
  emit valueChanged();
}

void ReosParameterArea::changeUnit( ReosArea::Unit unit )
{
  mValue.setUnit( unit );
}

ReosEncodedElement ReosParameterArea::encode() const
{
  ReosEncodedElement element( QStringLiteral( "area-parameter" ) );
  ReosParameter::encode( element );

  element.addEncodedData( QStringLiteral( "area-value" ), mValue.encode() );

  return element;
}

ReosParameterArea *ReosParameterArea::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterArea *ret = new ReosParameterArea( QString(), parent );

  if ( element.description() != QStringLiteral( "area-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  ret->mValue = ReosArea::decode( element.getEncodedData( QStringLiteral( "area-value" ) ) );

  return ret;
}

void ReosParameter::setDerivable( bool b )
{
  mIsDerivable = b;
}

ReosParameterSlope::ReosParameterSlope( const QString &name, QObject *parent ):
  ReosParameter( name, parent )
{

}

void ReosParameterSlope::setValue( double slope )
{
  mSlope = slope;
  mIsDerived = false;
  emit valueChanged();
}

void ReosParameterSlope::setDerivedValue( double slope )
{
  mSlope = slope;
  mIsDerived = true;
  emit valueChanged();
}

ReosEncodedElement ReosParameterSlope::encode() const
{
  ReosEncodedElement element( QStringLiteral( "slope-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "slope-value" ), mSlope );

  return element;
}

ReosParameterSlope *ReosParameterSlope::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterSlope *ret = new ReosParameterSlope( tr( "Average slope" ), parent );

  if ( element.description() != QStringLiteral( "slope-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  if ( !element.getData( QStringLiteral( "slope-value" ), ret->mSlope ) )
    ret->mSlope = 0;

  return ret;
}
