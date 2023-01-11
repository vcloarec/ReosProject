/***************************************************************************
                      reos_testutils.h
                     --------------------------------------
Date                 : 04-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOS_TESTUTILS_H
#define REOS_TESTUTILS_H

#include <QObject>
#include <QEventLoop>
#include <string>
#include <math.h>

class ReosProcess;

const char *data_path();

std::string test_file( std::string basename );

std::string tmp_file( std::string basename );

QString testFile( const QString &baseName );

QString tempFile( const QString &baseName );

bool equal( double a, double b, double precision );

bool equal( const QPolygonF &poly1, const QPolygonF &poly2 );

void simulateEventLoop( int durationMs );

class ModuleProcessControler: public QObject
{
  public:
    explicit ModuleProcessControler( ReosProcess *process );

    void waitForFinished();
    void reset();

  private slots:
    void processFinished();

  private:
    ReosProcess *mProcess;
    bool mProcessFinished = false;
    QEventLoop mEventLoop;
};

#endif // REOS_TESTUTILS_H
