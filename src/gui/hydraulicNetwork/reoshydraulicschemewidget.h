/***************************************************************************
  reoshydraulicschemewidget.h - ReosHydraulicSchemeWidget

 ---------------------
 begin                : 25.3.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICSCHEMEWIDGET_H
#define REOSHYDRAULICSCHEMEWIDGET_H

#include <QWidget>
#include <QWidgetAction>
#include <QListView>

#include "reoshydraulicnetwork.h"

class ReosHydraulicScheme;
class ReosHydraulicNetworkContext;
class ReosHydraulicNetwork;
class ReosMeteorologicModelsCollection;

namespace Ui
{
  class ReosHydraulicSchemeWidget;
}

class ReosHydraulicSchemeWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicSchemeWidget( const ReosHydraulicNetworkContext &context, QWidget *parent = nullptr );
    ~ReosHydraulicSchemeWidget();

    void setScheme( ReosHydraulicScheme *scheme );

    void hideName();

  signals:
    void meteoModelChange( const QString &meteoName );

  private slots:
    void onMeteoModelChange();

  private:
    Ui::ReosHydraulicSchemeWidget *ui;
    ReosHydraulicScheme *mScheme = nullptr;
    ReosMeteorologicModelsCollection *mMeteoCollection = nullptr;
};


class ReosHydraulicSchemeListView : public QListView
{
  public:
    ReosHydraulicSchemeListView( QWidget *parent );

    void setSchemeCollection( ReosHydraulicSchemeCollection *collection );

    ReosHydraulicScheme *currentScheme() const;
    void setCurrentScheme( const QString &schemeId );

  private:
    ReosHydraulicSchemeCollection *mCollection = nullptr;
};

#endif // REOSHYDRAULICSCHEMEWIDGET_H
