/***************************************************************************
  reosimporthydraulicstructuredialog.cpp - ReosImportHydraulicStructureDialog

 ---------------------
 begin                : 7.10.2022
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
#include "reosimporthydraulicstructuredialog.h"
#include "ui_reosimporthydraulicstructuredialog.h"

#include "reoshydraulicsimulation.h"
#include "reoshydraulic2dsimulationwidget.h"
#include "reoshydraulicnetwork.h"

ReosImportHydraulicStructureDialog::ReosImportHydraulicStructureDialog( QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosImportHydraulicStructureDialog )
{
  ui->setupUi( this );

  QMap<QString, QString> mEngines =
    ReosSimulationEngineRegistery::instance()->availableEngine( ReosSimulationEngineFactory::ImportStructure2D );

  for ( auto it = mEngines.constBegin(); it != mEngines.constEnd(); ++it )
  {
    ui->mComboImportSource->addItem( it.value(), it.key() );
  }

  onEngineChanged();
}

ReosImportHydraulicStructureDialog::~ReosImportHydraulicStructureDialog()
{
  delete ui;
}

void ReosImportHydraulicStructureDialog::createStructure2d(const ReosHydraulicNetworkContext& context) const
{
    if (!mCurrentEngineWidget)
        return;

    mCurrentEngineWidget->importStructure2D(context);
}

void ReosImportHydraulicStructureDialog::onEngineChanged()
{
    const QString key = ui->mComboImportSource->currentData().toString();

    if (mCurrentEngineWidget)
    {
        ui->mSourceWidget->layout()->removeWidget(mCurrentEngineWidget);
        delete mCurrentEngineWidget;
    }
    mCurrentEngineWidget = ReosHydraulicSimulationWidgetRegistery::instance()->createImportWidget(key, this);
    ui->mSourceWidget->layout()->addWidget(mCurrentEngineWidget);
}
