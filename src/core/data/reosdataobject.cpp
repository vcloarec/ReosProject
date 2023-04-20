/***************************************************************************
  reosdataobject.cpp - ReosDataObject

 ---------------------
 begin                : 4.2.2021
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
#include "reosdataobject.h"

#include <QUuid>

#include "reosencodedelement.h"

ReosDataObject::ReosDataObject( QObject *parent )
  : QObject( parent )
  , mUid( QUuid::createUuid().toString() )
{}

QString ReosDataObject::name() const
{
  return mName;
}

QString ReosDataObject::id() const
{
  return type() + QString( ':' ) + mUid;
}

void ReosDataObject::encode( ReosEncodedElement &element ) const
{
  element.addData( QStringLiteral( "object-name" ), mName );
  element.addData( QStringLiteral( "UID" ), mUid );
}

void ReosDataObject::decode( const ReosEncodedElement &element )
{
  element.getData( QStringLiteral( "object-name" ), mName );
  element.getData( QStringLiteral( "UID" ), mUid );
  if ( mUid.isEmpty() )
    QUuid::createUuid().toString();
  mIsObsolete = false;
}

void ReosDataObject::setName( const QString &name )
{
  mName = name;
  emit nameChanged( mName );
}

void ReosDataObject::registerUpstreamData( ReosDataObject *data )
{
  connect( data, &ReosDataObject::dataChanged, this, &ReosDataObject::setObsolete );
  connect( data, &ReosDataObject::isSetObsolete, this, &ReosDataObject::setObsolete );
}

void ReosDataObject::deregisterUpstreamData( ReosDataObject *data )
{
  disconnect( data, &ReosDataObject::dataChanged, this, &ReosDataObject::setObsolete );
  disconnect( data, &ReosDataObject::isSetObsolete, this, &ReosDataObject::setObsolete );
}

void ReosDataObject::setActualized() const
{
  mIsObsolete = false;
}

void ReosDataObject::setObsolete()
{
  mIsObsolete = true;
  emit isSetObsolete();
}

bool ReosDataObject::isObsolete() const
{
  return mIsObsolete;
}
