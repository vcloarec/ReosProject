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

#include "reoscore.h"

/**
 * Class used to store data under byte array
 */
class REOSCORE_EXPORT ReosEncodedElement
{
  public:
    //! Constructor of an empty encoded element with only the description
    ReosEncodedElement( const QString &description ): mDescription( description )
    {}

    //! Contructor of an encoded element from a \a byte array than contains data and description
    ReosEncodedElement( const QByteArray &byteArray );

    //! Returns the desctiption of the encoded element
    QString description() const {return mDescription;}

    //! Adds data \a value with a \a key to the encoded element
    void addEncodedData( const QString &key, const ReosEncodedElement &element );

    ReosEncodedElement getEncodedData( const QString &key ) const;

    //! Adds data \a value with a \a key to the encoded element
    template<typename T>
    void addData( const QString &key, T &&value )
    {
      QByteArray byteArray;
      QDataStream stream( &byteArray, QIODevice::WriteOnly );
      stream << value;
      mData[key] = byteArray;
    }

    //! Gets the data \a value with the \a key from the encoded element
    template<typename T>
    bool getData( QString key, T &value ) const
    {
      if ( !mData.contains( key ) )
        return false;

      QDataStream stream( mData[key] );
      stream >> value;
      return true;
    }

    void addListEncodedData( const QString &key, const QList<ReosEncodedElement> &list );
    QList<ReosEncodedElement> getListEncodedData( const QString &key ) const;

    //! Returns byte of the encoded element that can be store in files or in another encoded element
    QByteArray bytes() const;

    bool hasEncodedData() const;
    bool hasEncodedData( const QString &key ) const;

  private:
    QMap<QString, QByteArray> mData;
    QString mDescription;

};

#endif // REOSENCODEDELEMENT_H
