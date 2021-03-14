/***************************************************************************
                      reossettings.h
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

#ifndef REOSSETTINGS_H
#define REOSSETTINGS_H

#include <QSettings>

class ReosSettings
{
  public:
    ReosSettings();

    static void setPath( QString path );

    void setValue( const QString &key, const QVariant &value );

    QVariant value( const QString &key, const QVariant &defaultValue = QVariant() ) const;

    QString fileName() const;

    bool contains( const QString &key ) const;

  private:
    QSettings settings;

    static bool pathSet;

};


#endif // REOSSETTINGS_H
