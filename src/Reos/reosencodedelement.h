/***************************************************************************
                      reosencodedelement.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
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
    ReosEncodedElement( QString description ): description( description )
    {}

    ReosEncodedElement( const QByteArray &byteArray );

    const QString selfDescription() const {return description;}


    template<typename T>
    void addData( QString key, T &&value )
    {
      QByteArray byteArray;
      QDataStream stream( &byteArray, QIODevice::WriteOnly );
      stream << value;
      data[key] = byteArray;
    }


    template<typename T>
    bool getData( QString key, T &value ) const
    {
      if ( !data.contains( key ) )
        return false;

      QDataStream stream( data[key] );
      stream >> value;
      return true;
    }

    QByteArray encode();


  private:
    QMap<QString, QByteArray> data;
    QString description;

};




#endif // REOSENCODEDELEMENT_H
