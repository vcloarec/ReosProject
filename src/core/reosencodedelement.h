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
#include <QDir>

#include "reoscore.h"

//! Class that sore information about the context of encoding
class REOSCORE_EXPORT ReosEncodeContext
{
  public:
    //! Returns the path to encode depending of the context
    QString pathToEncode( const QString &filePath ) const;

    //! Resolves the path depending of the context
    QString resolvePath( const QString &path ) const;

    /**
     * Sets the base direcory used for relative pathes
     *
     * \see setEncodeRelativePath()
     */
    void setBaseDir( const QDir &newBaseDir );

    /**
     * Sets whether pathes are relative
     *
     * \see setBaseDir()
     */
    void setEncodeRelativePath( bool isRelative );

  private:
    QDir mBaseDir;
    bool mEncodeRelativePath = true;
};

/**
 * Class used to store data under byte array
 */
class REOSCORE_EXPORT ReosEncodedElement
{
  public:

    //! Constructor of an empty encoded element with only the description
    explicit ReosEncodedElement() = default;

    //! Constructor of an empty encoded element with only the description
    explicit ReosEncodedElement( const QString &description );

    //! Contructor of an encoded element from a \a byte array than contains data and description
    explicit ReosEncodedElement( const QByteArray &byteArray );

    //! Returns byte of the encoded element that can be store in files or in another encoded element
    QByteArray bytes() const;

    //! Returns the desctiption of the encoded element
    QString description() const {return mDescription;}

    //! Adds data \a value with a \a key to the encoded element
    void addEncodedData( const QString &key, const ReosEncodedElement &element );

    ReosEncodedElement getEncodedData( const QString &key ) const;

#ifndef SIP_RUN
    //! Adds data \a value with a \a key to the encoded element
    template<typename T>
    void addData( const QString &key, T &&value )
    {
      QByteArray byteArray;
      QDataStream stream( &byteArray, QIODevice::WriteOnly );
      stream.setVersion( sVersion );
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
      stream.setVersion( sVersion );
      stream >> value;
      return true;
    }

    void addListEncodedData( const QString &key, const QList<ReosEncodedElement> &list );
    QList<ReosEncodedElement> getListEncodedData( const QString &key ) const;

    bool hasEncodedData() const;
    bool hasEncodedData( const QString &key ) const;

#endif // not SIP_RUN
    static void setSerialisationVersion( QDataStream::Version version );

  private:
    QMap<QString, QByteArray> mData;
    QString mDescription;

    static QDataStream::Version sVersion;

};

#endif // REOSENCODEDELEMENT_H
