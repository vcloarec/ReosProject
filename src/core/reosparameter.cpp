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


ReosParameter::ReosParameter( const QString &name, bool derivable, QObject *parent ):
  QObject( parent )
  , mIsDerived( derivable )
  , mName( name )
  , mIsDerivable( derivable )
{

}

ReosParameter::ReosParameter( const QString &name, QObject *parent ):
  QObject( parent )
  , mName( name )
  , mIsDerivable( false )
{
}

QString ReosParameter::name() const
{
  return mName;
}

void ReosParameter::setName( const QString &name )
{
  mName = name;
}

bool ReosParameter::isDerivable() const
{
  return mIsDerivable;
}

bool ReosParameter::isDerived() const
{
  return mIsDerived;
}

void ReosParameter::askForDerivation()
{
  emit needCalculation();
}

void ReosParameter::updateIfNecessary()
{
  if ( mIsDerived )
    askForDerivation();
}

void ReosParameter::encode( ReosEncodedElement &element ) const
{
  element.addData( QStringLiteral( "name" ), mName );
  element.addData( QStringLiteral( "is-derived" ), mIsDerived );
  element.addData( QStringLiteral( "is-valid" ), mIsValid );
}

void ReosParameter::decode( const ReosEncodedElement &element, bool isDerivable )
{
  mIsDerivable = isDerivable;

  if ( !element.getData( QStringLiteral( "name" ), mName ) )
    return;

  if ( !element.getData( QStringLiteral( "is-derived" ), mIsDerived ) )
    return;

  if ( !element.getData( QStringLiteral( "is-valid" ), mIsValid ) )
    return;
}

bool ReosParameter::isEditable() const
{
    return mIsEditable;
}

void ReosParameter::setIsEditable(bool isEditable)
{
    mIsEditable = isEditable;
}


ReosParameterArea::ReosParameterArea( const QString &name, bool derivable, QObject *parent ):
    ReosParameter( name, derivable, parent )
{}

ReosParameterArea::ReosParameterArea( const QString &name, QObject *parent ):
  ReosParameter( name, false, parent )
{}

void ReosParameterArea::setValue( const ReosArea &area )
{
  mValue = area;
  mIsDerived = false;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameterArea::setDerivedValue( const ReosArea &area )
{
  mValue = area;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameterArea::changeUnit( ReosArea::Unit unit )
{
  mValue.setUnit( unit );
}

QString ReosParameterArea::toString( int precision ) const
{
  if ( isValid() )
    return mValue.toString( precision );
  else
  {
    return QString( '-' );
  }
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

ReosParameterArea *ReosParameterArea::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterArea *ret = decode( element, isDerivable, parent );
  ret->setName( name );
  return ret;
}

void ReosParameter::setDerivable( bool b )
{
  mIsDerivable = b;
}

ReosParameterSlope::ReosParameterSlope( const QString &name, bool derivable, QObject *parent ):
  ReosParameter( name, derivable, parent )
{}

ReosParameterSlope::ReosParameterSlope( const QString &name, QObject *parent ):
  ReosParameter( name, parent )
{}

void ReosParameterSlope::setValue( double slope )
{
  mSlope = slope;
  mIsDerived = false;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameterSlope::setDerivedValue( double slope )
{
  mSlope = slope;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

QString ReosParameterSlope::toString( int precision ) const
{
  if ( isValid() )
  {
    if ( int( mSlope * 1000 ) == 0 )
      return ReosParameter::doubleToString( mSlope * 1000, precision ) + QString( ' ' ) + QChar( 0x2030 ) ;
    else
      return ReosParameter::doubleToString( mSlope * 1000, precision ) + QString( ' ' ) + QString( '%' ) ;
  }
  else
    return QString( '-' );
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

ReosParameterSlope *ReosParameterSlope::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterSlope *ret = decode( element, isDerivable, parent );
  ret->setName( name );
  return ret;
}

ReosParameterString::ReosParameterString( const QString &name, bool derivable, QObject *parent ):
  ReosParameter( name, derivable, parent )
{
}

ReosParameterString::ReosParameterString( const QString &name, QObject *parent ):
  ReosParameter( name, false, parent )
{

}

void ReosParameterString::setValue( const QString &string )
{
  mValue = string;
  mIsDerived = false;
  mIsValid = true;
  emit valueChanged();
}

QString ReosParameterString::toString( int ) const
{
  if ( isValid() )
    return mValue;
  else
  {
    return QString( '-' );
  }
}

ReosEncodedElement ReosParameterString::encode() const
{
  ReosEncodedElement element( QStringLiteral( "string-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "string-value" ), mValue );

  return element;
}

ReosParameterString *ReosParameterString::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterString *ret = new ReosParameterString( QString(), parent );

  if ( element.description() != QStringLiteral( "string-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  element.getData( QStringLiteral( "string-value" ), ret->mValue );

  return ret;
}

ReosParameterString *ReosParameterString::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterString *ret = decode( element, isDerivable, parent );
  ret->setName( name );
  return ret;
}

ReosParameterDuration::ReosParameterDuration( const QString &name, bool derivable, QObject *parent ):
  ReosParameter( name, derivable, parent )
{}

ReosParameterDuration::ReosParameterDuration( const QString &name, QObject *parent ):
  ReosParameter( name,  parent )
{}

void ReosParameterDuration::setValue( const ReosDuration &duration )
{
  mDuration = duration;
  mIsDerived = false;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameterDuration::setDerivedValue( const ReosDuration &duration )
{
  mDuration = duration;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameterDuration::changeUnit( ReosDuration::Unit unit )
{
  mDuration.setUnit( unit );
  emit unitChanged();
}

ReosDuration ReosParameterDuration::value() const {return mDuration;}

QString ReosParameterDuration::toString( int precision ) const
{
  if ( isValid() )
  {
    return mDuration.toString( precision );
  }
  else
    return QString( '-' );
}

ReosEncodedElement ReosParameterDuration::encode() const
{
  ReosEncodedElement element( QStringLiteral( "duration-parameter" ) );
  ReosParameter::encode( element );

  element.addEncodedData( QStringLiteral( "duration-value" ), mDuration.encode() );

  return element;
}

ReosParameterDuration *ReosParameterDuration::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterDuration *ret = new ReosParameterDuration( QString(), parent );

  if ( element.description() != QStringLiteral( "duration-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  ret->mDuration = ReosDuration::decode( element.getEncodedData( QStringLiteral( "duration-value" ) ) );

  return ret;
}

ReosParameterDuration *ReosParameterDuration::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterDuration *ret = decode( element, isDerivable, parent );
  ret->setName( name );

  return ret;
}

ReosParameterDateTime::ReosParameterDateTime( const QString &name, QObject *parent ):
  ReosParameter( name, parent )
{
  mDateTime = QDateTime( QDate( QDate::currentDate().year(), 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC );
  mIsValid = true;
}

void ReosParameterDateTime::setValue( const QDateTime &dt )
{
  mDateTime = dt;
  mIsDerived = false;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameterDateTime::setDerivedValue( const QDateTime &dt )
{
  mDateTime = dt;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

QString ReosParameterDateTime::toString( int ) const
{
  if ( isValid() )
  {
    return mDateTime.toString();
  }
  else
    return QString( '-' );
}

ReosEncodedElement ReosParameterDateTime::encode() const
{
  ReosEncodedElement element( QStringLiteral( "datetime-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "date-time-value" ), mDateTime );

  return element;
}

ReosParameterDateTime *ReosParameterDateTime::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )

{
  ReosParameterDateTime *ret = new ReosParameterDateTime( tr( "Date/Time" ), parent );

  if ( element.description() != QStringLiteral( "datetime-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  if ( !element.getData( QStringLiteral( "date-time-value" ), ret->mDateTime ) )
    ret->mDateTime = QDateTime();

  return ret;
}

ReosParameterDateTime *ReosParameterDateTime::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterDateTime *ret = decode( element, isDerivable, parent );
  ret->setName( name );

  return ret;
}

ReosParameterDouble::ReosParameterDouble( const QString &name,  bool derivable, QObject *parent ):
  ReosParameter( name, derivable, parent )
{}

ReosParameterDouble::ReosParameterDouble( const QString &name, QObject *parent ):
  ReosParameter( name, false, parent )
{}


QString ReosParameterDouble::toString( int precision ) const
{
  if ( isValid() )
  {
    int p = 2;
    if ( precision >= 0 )
      p = precision;
    else if ( mDisplayPrecision >= 0 )
      p = mDisplayPrecision;
    return ReosParameter::doubleToString( mValue, p );
  }
  else
  {
    return QString( '-' );
  }
}

void ReosParameterDouble::setValue( double value )
{
  mIsDerived = false;
  mValue = value;
  mIsValid = true;
  emit valueChanged();
}

void ReosParameter::setInvalid()
{
  mIsValid = false;
  emit valueChanged();
}

double ReosParameter::stringToDouble( const QString &string, bool *ok )
{
  double value = QLocale().toDouble( string, ok );
  if ( !( *ok ) )
    value = string.toDouble( ok );

  return value;
}

QString ReosParameter::doubleToString( double value, int precision )
{
  return QLocale().toString( value, 'f', precision );
}

bool ReosParameterDouble::setValueWithString( const QString &value )
{
  bool ok = false;
  double v = ReosParameter::stringToDouble( value, &ok );
  if ( !ok )
    return false;

  QString digits = value.split( QLocale().decimalPoint() ).last();
  if ( digits == value )
    digits = value.split( '.' ).last();

  if ( digits != value )
    mDisplayPrecision = digits.count();

  mValue = v;
  mIsValid = true;
  emit valueChanged();
  return true;
}

void ReosParameterDouble::setDerivedValue( double value )
{
  mValue = value;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

ReosEncodedElement ReosParameterDouble::encode() const
{
  ReosEncodedElement element( QStringLiteral( "double-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "double-value" ), mValue );
  element.addData( QStringLiteral( "double-display-precision" ), mDisplayPrecision );

  return element;
}

ReosParameterDouble *ReosParameterDouble::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterDouble *ret = new ReosParameterDouble( QString(), parent );

  if ( element.description() != QStringLiteral( "double-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  element.getData( QStringLiteral( "double-value" ), ret->mValue );
  element.getData( QStringLiteral( "double-display-precision" ), ret->mDisplayPrecision );

  return ret;
}

ReosParameterDouble *ReosParameterDouble::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterDouble *ret = decode( element, isDerivable, parent );
  ret->setName( name );
  return ret;
}

bool ReosParameter::isValid() const
{
  return mIsValid;
}

ReosParameterBoolean::ReosParameterBoolean( const QString &name, bool derivable, QObject *parent ):
  ReosParameter( name, derivable, parent )
{}

ReosParameterBoolean::ReosParameterBoolean( const QString &name, QObject *parent ):
  ReosParameter( name, false, parent )
{}

QString ReosParameterBoolean::toString( int ) const
{
  if ( isValid() )
  {
    return mValue ? tr( "True" ) : tr( "False" );
  }
  else
  {
    return QString( '-' );
  }
}

void ReosParameterBoolean::setValue( bool value )
{
  mIsDerived = false;
  mValue = value;
  mIsValid = true;
  emit valueChanged();
}


void ReosParameterBoolean::setDerivedValue( bool value )
{
  mValue = value;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

ReosEncodedElement ReosParameterBoolean::encode() const
{
  ReosEncodedElement element( QStringLiteral( "boolean-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "boolean-value" ), mValue );

  return element;
}

ReosParameterBoolean *ReosParameterBoolean::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterBoolean *ret = new ReosParameterBoolean( QString(), parent );

  if ( element.description() != QStringLiteral( "boolean-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  element.getData( QStringLiteral( "boolean-value" ), ret->mValue );

  return ret;
}

ReosParameterBoolean *ReosParameterBoolean::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterBoolean *ret = decode( element, isDerivable, parent );
  ret->setName( name );
  return ret;
}

ReosParameterLongString::ReosParameterLongString( const QString &name, bool derivable, QObject *parent ):
  ReosParameterString( name, derivable, parent )
{}

ReosParameterLongString::ReosParameterLongString( const QString &name, QObject *parent ):
  ReosParameterString( name, parent )
{}

ReosEncodedElement ReosParameterLongString::encode() const
{
  ReosEncodedElement element( QStringLiteral( "long-string-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "string-value" ), mValue );

  return element;
}

ReosParameterLongString *ReosParameterLongString::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterLongString *ret = new ReosParameterLongString( QString(), parent );

  if ( element.description() != QStringLiteral( "long-string-parameter" ) &&
       element.description() != QStringLiteral( "string-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  element.getData( QStringLiteral( "string-value" ), ret->mValue );

  return ret;
}

ReosParameterLongString *ReosParameterLongString::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterLongString *ret = decode( element, isDerivable, parent );
  ret->setName( name );

  return ret;
}

ReosParameterInteger::ReosParameterInteger( const QString &name, bool derivable, QObject *parent ):
  ReosParameter( name, derivable, parent )
{}

ReosParameterInteger::ReosParameterInteger( const QString &name, QObject *parent ):
  ReosParameter( name, false, parent )
{}

void ReosParameterInteger::setValue( int value )
{
  mIsDerived = false;
  mValue = value;
  mIsValid = true;
  emit valueChanged();
}

bool ReosParameterInteger::setValueWithString( const QString &value )
{
  bool ok = false;
  double v = value.toInt( &ok );
  if ( !ok )
    return false;

  mValue = v;
  mIsValid = true;
  emit valueChanged();
  return true;
}

void ReosParameterInteger::setDerivedValue( int value )
{
  mValue = value;
  mIsDerived = true;
  mIsValid = true;
  emit valueChanged();
}

QString ReosParameterInteger::toString( int ) const
{
  return QString::number( mValue );
}

ReosEncodedElement ReosParameterInteger::encode() const
{
  ReosEncodedElement element( QStringLiteral( "integer-parameter" ) );
  ReosParameter::encode( element );

  element.addData( QStringLiteral( "integer-value" ), mValue );

  return element;
}

ReosParameterInteger *ReosParameterInteger::decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent )
{
  ReosParameterInteger *ret = new ReosParameterInteger( QString(), parent );

  if ( element.description() != QStringLiteral( "integer-parameter" ) )
    return ret;

  ret->ReosParameter::decode( element, isDerivable );

  element.getData( QStringLiteral( "integer-value" ), ret->mValue );

  return ret;
}

ReosParameterInteger *ReosParameterInteger::decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent )
{
  ReosParameterInteger *ret = decode( element, isDerivable, parent );
  ret->setName( name );
  return ret;
}
