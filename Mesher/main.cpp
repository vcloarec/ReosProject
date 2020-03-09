/***************************************************************************
                      main.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "mainwindow.h"
#include <QApplication>

int main( int argc, char *argv[] )
{

  QgsApplication a( argc, argv, true );

  QCoreApplication::setOrganizationName( QStringLiteral( "ReosProject" ) );
  QCoreApplication::setApplicationName( QStringLiteral( "Mesher" ) );
  QSettings::setDefaultFormat( QSettings::IniFormat );


  a.init();

  MainWindow w;
  w.showMaximized();

  return a.exec();
}
