/***************************************************************************
  reostelemacsteeringfile.h - ReosTelemacSteeringFile

 ---------------------
 begin                : 2.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSTELEMACSTEERINGFILE_H
#define REOSTELEMACSTEERINGFILE_H

#include <QString>
#include <QMap>
#include <QObject>
#include <memory.h>

class ReosTelemacSteeringFile
{
  public:
    ReosTelemacSteeringFile( const QString &path );
    void open();
    void save( const QString &path = QString() );

    void setKey( const QString &key, const QString &value );
    QString value( const QString &key ) const;
    void addComment( const QString &comment );

    int keyCount() const;
    int lineCount() const;

    bool isValid() const;

  private:
    class SteeringLine
    {
      public:
        SteeringLine( const QString &line );
        SteeringLine( const QString &key, const QString &value );

        QString key() const;
        QString value() const;

        void setValue( const QString &value );

      private:
        QString mKey;
        QString mValue;
    };

    QString mPath;
    std::vector<std::unique_ptr<SteeringLine>> mLines;
    QMap<QString, SteeringLine *> mLinesMap;
};


#endif // REOSTELEMACSTEERINGFILE_H
