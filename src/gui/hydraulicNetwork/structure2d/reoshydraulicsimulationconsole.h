/***************************************************************************
  reoshydraulicsimulationconsole.h - ReosHydraulicSimulationConsole

 ---------------------
 begin                : 30.3.2022
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
#ifndef REOSHYDRAULICSIMULATIONCONSOLE_H
#define REOSHYDRAULICSIMULATIONCONSOLE_H

#include <QPointer>

#include "reosactionwidget.h"

namespace Ui
{
  class ReosHydraulicSimulationConsole;
}

class ReosHydraulicStructure2D;
class ReosGuiContext;
class ReosSimulationProcess;

class ReosHydraulicSimulationConsole : public ReosStackedPageWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicSimulationConsole( ReosSimulationProcess *process, const ReosGuiContext &context );
    ~ReosHydraulicSimulationConsole();

  private slots:
    void receiveInformation( const QString &info );
    void onStopSimulation();

  private:
    Ui::ReosHydraulicSimulationConsole *ui;
    QPointer<ReosSimulationProcess> mProcess;
};

#endif // REOSHYDRAULICSIMULATIONCONSOLE_H
