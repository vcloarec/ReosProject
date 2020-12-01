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


ReosEncodedElement::ReosEncodedElement(QString description): mDescription( description )
{}

ReosEncodedElement::ReosEncodedElement( const QByteArray &byteArray )
{
    QDataStream stream( byteArray );
  stream >> mDescription;
  stream >> mData;
}

QByteArray ReosEncodedElement::encode()
{
  QByteArray byteArray;
  QDataStream stream( &byteArray, QIODevice::WriteOnly );
  stream << mDescription;
  stream << mData;
  return byteArray;
}
