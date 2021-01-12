/***************************************************************************
                      reosencodedelement.h
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

#ifndef REOSENCODEDELEMENT_H
#define REOSENCODEDELEMENT_H

#include <QByteArray>
#include <QMap>
#include <QString>
#include <QDataStream>

class ReosEncodedElement
{
  public:
    ReosEncodedElement( QString mDescription );
    ReosEncodedElement( const QByteArray &byteArray );

    const QString selfDescription() const {return mDescription;}

    template<typename T>
    void addData( QString key, T &&value )
    {
      QByteArray byteArray;
      QDataStream stream( &byteArray, QIODevice::WriteOnly );
      stream << value;
      mData[key] = byteArray;
    }


    template<typename T>
    bool getData( QString key, T &value ) const
    {
      if ( !mData.contains( key ) )
        return false;

      QDataStream stream( mData[key] );
      stream >> value;
      return true;
    }

    QByteArray encode() const;


  private:
    QMap<QString, QByteArray> mData;
    QString mDescription;

};




#endif // REOSENCODEDELEMENT_H
