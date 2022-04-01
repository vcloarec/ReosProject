/***************************************************************************
  reostelemacsimulationeditwidget.h - ReosTelemacSimulationEditWidget

 ---------------------
 begin                : 31.3.2022
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
#ifndef REOSTELEMACSIMULATIONEDITWIDGET_H
#define REOSTELEMACSIMULATIONEDITWIDGET_H

#include <QWidget>
#include "reostelemac2dsimulation.h"
#include "reoshydraulic2dsimulationwidget.h"

namespace Ui
{
  class ReosTelemacSimulationEditWidget;
}

class ReosTelemacSimulationEditWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosTelemacSimulationEditWidget( ReosTelemac2DSimulation *simulation, QWidget *parent = nullptr );
    ~ReosTelemacSimulationEditWidget();

  private:
    Ui::ReosTelemacSimulationEditWidget *ui;
};

class ReosTelemacSimulationEditWidgetFactory : public ReosHydraulicSimulationWidgetFactory
{
  public:
    QString key() const {return ReosTelemac2DSimulation::staticKey();}

    QWidget *simulationSettingsWidget( ReosHydraulicSimulation *simulation, QWidget *parent );
};

#endif // REOSTELEMACSIMULATIONEDITWIDGET_H
