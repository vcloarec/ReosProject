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

#include "reoshydraulicnetwork.h"

class ReosHydraulicScheme;
class ReosHydraulicNetworkContext;
class ReosHydraulicNetwork;

namespace Ui
{
  class ReosHydraulicSchemeWidget;
}

class ReosHydraulicSchemeWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicSchemeWidget( ReosHydraulicScheme *scheme, const ReosHydraulicNetworkContext &context, QWidget *parent = nullptr );
    ~ReosHydraulicSchemeWidget();

    void setScheme( ReosHydraulicScheme *scheme );

  private slots:
    void onMeteoModelChange();

  private:
    Ui::ReosHydraulicSchemeWidget *ui;
    ReosHydraulicScheme *mScheme = nullptr;
    ReosHydraulicNetworkContext mContext;
};


class ReosHydraulicSchemeWidgetAction : public QWidgetAction
{
    Q_OBJECT
  public:
    ReosHydraulicSchemeWidgetAction( ReosHydraulicNetwork *network, QObject *parent = nullptr );

    void setCurrentScheme( ReosHydraulicScheme *scheme );

  protected:
    QWidget *createWidget( QWidget *parent ) override;

  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosHydraulicScheme *mScheme = nullptr;
    ReosHydraulicSchemeWidget *mWidget = nullptr;
};

#endif // REOSHYDRAULICSCHEMEWIDGET_H
