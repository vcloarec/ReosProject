/***************************************************************************
                      reossettings.cpp
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

#include "reossettings.h"


ReosSettings::ReosSettings()
    //:settings("settings.ini",QSettings::IniFormat)
{

}

void ReosSettings::setPath(QString path)
{
    if (!pathSet)
    {
        QSettings::setPath(QSettings::IniFormat,QSettings::UserScope,path);
    }
}

void ReosSettings::setValue(const QString &key, const QVariant &value)
{
    settings.setValue(key,value);
}

QVariant ReosSettings::value(const QString &key, const QVariant &defaultValue) const
{
    return settings.value(key,defaultValue);
}

QString ReosSettings::fileName() const
{
    return settings.fileName();
}

bool ReosSettings::contains(const QString &key) const
{
    return settings.contains(key);
}

bool ReosSettings::pathSet=false;
