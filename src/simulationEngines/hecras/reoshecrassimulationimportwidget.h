/***************************************************************************
  reoshecrassimulationimportwidget.h - ReosHecRasSimulationImportWidget

 ---------------------
 begin                : 06.10.2022
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
#ifndef REOSHECRASSIMULATIONIMPORTWIDGET_H
#define REOSHECRASSIMULATIONIMPORTWIDGET_H

#include <QWidget>
#include "reosimporthydraulicstructuredialog.h"

class ReosHydraulicStructure2D;

namespace Ui
{
  class ReosHecRasSimulationImportWidget;
}

class ReosHecRasSimulationImportWidget : public ReosImportHydraulicStructureWidget
{
    Q_OBJECT

  public:
    ReosHecRasSimulationImportWidget( QWidget *parent = nullptr );

    ReosHydraulicStructure2D *importStructure2D( const ReosHydraulicNetworkContext &context ) const override;

  private slots:
    void onProjectFileButtonPressed();

  private:
    Ui::ReosHecRasSimulationImportWidget *ui = nullptr;
};

#endif //REOSHECRASSIMULATIONIMPORTWIDGET_H
