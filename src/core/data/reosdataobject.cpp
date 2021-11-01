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

#include "reosencodedelement.h"

ReosDataObject::ReosDataObject( QObject *parent ): QObject( parent ) {}

QString ReosDataObject::name() const
{
  return mName;
}

void ReosDataObject::encode( ReosEncodedElement &element ) const
{
  element.addData( QStringLiteral( "object-name" ), mName );
}

void ReosDataObject::decode( const ReosEncodedElement &element )
{
  element.getData( QStringLiteral( "object-name" ), mName );
}

void ReosDataObject::setName( const QString &name )
{
  mName = name;
}
