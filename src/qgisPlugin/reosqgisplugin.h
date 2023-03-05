/***************************************************************************
  reosqgisplugin.h - ReosQgisPlugin

 ---------------------
 begin                : 3.3.2023
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
#ifndef REOSQGISPLUGIN_H
#define REOSQGISPLUGIN_H

#include <QObject>
#include <QApplication>
#include <qgisplugin.h>

class QAction;
class ReosGisEngine;
class ReosWatershedModule;
class ReosModule;
class ReosMap;

class ReosQgisPlugin : public QObject, public QgisPlugin
{
    Q_OBJECT
  public:
    explicit ReosQgisPlugin( QgisInterface *iface );

    void initGui() override;
    void unload() override;

  private:
    QgisInterface *mIface = nullptr;
    QAction *mMenuAction = nullptr;
    ReosGisEngine *mGisEngine = nullptr;
    ReosModule *mRootModule = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    ReosMap *mMap = nullptr;
};


static const QString sName = QApplication::translate( "ReosQgisPlugin", "Reos Plugin" );
static const QString sDescription = QApplication::translate( "ReosQgisPlugin", "Reos in QGIS" );
static const QString sCategory = QApplication::translate( "ReosQgisPlugin", "Vector" );
static const QString sPluginVersion = QApplication::translate( "ReosQgisPlugin", "Version 0.1" );
static const QgisPlugin::PluginType sPluginType = QgisPlugin::UI;
static const QString sPluginIcon = QStringLiteral( ":/geometrychecker/icons/geometrychecker.svg" );

#endif // REOSQGISPLUGIN_H
