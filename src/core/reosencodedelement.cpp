/***************************************************************************
                      reosencodedelement.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosencodedelement.h"

QDataStream::Version ReosEncodedElement::sVersion = QDataStream::Qt_DefaultCompiledVersion;

ReosEncodedElement::ReosEncodedElement( const QByteArray &byteArray )
{
  QDataStream stream( byteArray );
  stream.setVersion( sVersion );
  stream >> mDescription;
  stream >> mData;
}

void ReosEncodedElement::addEncodedData( const QString &key, const ReosEncodedElement &element )
{
  mData[key] = element.bytes();
}

ReosEncodedElement ReosEncodedElement::getEncodedData( const QString &key ) const
{
  if ( !mData.contains( key ) )
    return ReosEncodedElement( QStringLiteral( "invalid" ) );

  QByteArray ba = mData[key];
  return ReosEncodedElement( ba );
}

void ReosEncodedElement::addListEncodedData( const QString &key, const QList<ReosEncodedElement> &list )
{
  QByteArray byteArray;
  QDataStream stream( &byteArray, QIODevice::WriteOnly );
  stream.setVersion( sVersion );
  QList<QByteArray> listByte;
  for ( const ReosEncodedElement &elem : std::as_const( list ) )
    listByte.append( elem.bytes() );

  stream << listByte;

  mData[key] = byteArray;
}

QList<ReosEncodedElement> ReosEncodedElement::getListEncodedData( const QString &key ) const
{
  QList<ReosEncodedElement> list;
  if ( !mData.contains( key ) )
    return list;
  QDataStream stream( mData[key] );
  stream.setVersion( sVersion );
  QList<QByteArray> listByte;
  stream >> listByte;

  for ( const QByteArray &ba : std::as_const( listByte ) )
    list.append( ReosEncodedElement( ba ) );

  return list;
}

QByteArray ReosEncodedElement::bytes() const
{
  QByteArray byteArray;
  QDataStream stream( &byteArray, QIODevice::WriteOnly );
  stream.setVersion( sVersion );
  stream << mDescription;
  stream << mData;
  return byteArray;
}

bool ReosEncodedElement::hasEncodedData() const
{
  return !mData.empty();
}

bool ReosEncodedElement::hasEncodedData( const QString &key ) const
{
  return mData.contains( key );
}

void ReosEncodedElement::setSerialisationVersion( QDataStream::Version version )
{
  sVersion = version;
}

