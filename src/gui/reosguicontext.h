/***************************************************************************
  reosguicontext.h - ReosGuiContext

 ---------------------
 begin                : 28.12.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSGUICONTEXT_H
#define REOSGUICONTEXT_H

#include <QString>
#include <QStack>
#include <QList>
#include <QMap>

#include "reosgui.h"

class ReosMap;
class ReosMapItem;
class QWidget;
class QAction;
class QToolBar;;

class REOSGUI_EXPORT ReosGuiContext
{
  public:
    explicit ReosGuiContext( QWidget *parent = nullptr );
    ReosGuiContext( const ReosGuiContext &other, QWidget *parent );
    ReosGuiContext( const ReosGuiContext &other );

    ReosMap *map() const;

    void setMap( ReosMap *map );

    QWidget *parent() const;

    ReosMapItem *mapItems( const QString &description ) const;
    void addMapItems( ReosMapItem *mapItems );

    void addAction( QAction *action );
    QList<QAction * > actions() const;

    void addActionToMainToolBar( const QString &key, QAction *action ) const;
    void addMainToolBar( const QString key, QToolBar *stylingToolBar );

  private:
    ReosMap *mMap = nullptr;
    QWidget *mParent = nullptr;
    QStack<ReosMapItem *> mMapItems;
    QList<QAction *> mActions;
    QMap<QString, QToolBar *> mToolBars;

};

#endif // REOSGUICONTEXT_H
