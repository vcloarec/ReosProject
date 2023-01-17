/***************************************************************************
  reoshdf5.h - ReosHdf5

 ---------------------
 begin                : 16.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHDF5_H
#define REOSHDF5_H

#include "reoscore.h"
#include <QString>
#include "hdf5.h"


class REOSCORE_EXPORT ReosHdf5Attribute
{
  public:
    ~ReosHdf5Attribute();

    ReosHdf5Attribute( const ReosHdf5Attribute &other );
    ReosHdf5Attribute &operator=( const ReosHdf5Attribute &other );

    bool isValid() const;
    QString readString( size_t preSize = 512 ) const;

  private:
    ReosHdf5Attribute( const QString &name, hid_t group );

    hid_t id = 0;
    int ref = 0;

    friend class ReosHdf5Group;
};


class REOSCORE_EXPORT ReosHdf5Group
{
  public:
    ~ReosHdf5Group();

    ReosHdf5Group( const ReosHdf5Group &other );
    ReosHdf5Group &operator=( const ReosHdf5Group &other );

    bool isValid() const;
    ReosHdf5Attribute attribute( const QString &name ) const;

  private:
    ReosHdf5Group( const QString &path, hid_t file );

    hid_t id;
    int ref = 0;

    friend class ReosHdf5File;
};


class REOSCORE_EXPORT ReosHdf5File
{
  public:
    explicit ReosHdf5File( const QString &fileName );
    ~ReosHdf5File();

    bool isValid() const;
    bool pathExists( const QString &path ) const;
    ReosHdf5Group createGroup( const QString &path ) const;

  private:
    hid_t id;
};


#endif // REOSHDF5_H
