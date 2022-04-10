/***************************************************************************
  reoshydraulicsimulationconsole.cpp - ReosHydraulicSimulationConsole

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
#include "reoshydraulicsimulationconsole.h"
#include "ui_reoshydraulicsimulationconsole.h"

#include<QMessageBox>

#include "reosguicontext.h"
#include "reoshydraulicstructure2d.h"

ReosHydraulicSimulationConsole::ReosHydraulicSimulationConsole( ReosSimulationProcess *process, const ReosGuiContext &context )
  :  ReosStackedPageWidget( context.parent() )
  ,  ui( new Ui::ReosHydraulicSimulationConsole )
  , mProcess( process )
{
  ui->setupUi( this );
  ui->mProgressBar->setMaximum( 100 );
  if ( process )
  {
    ui->mProgressBar->setValue( process->currentProgression() );
    ui->mStopButton->setEnabled( !mProcess.isNull() && !mProcess->isFinished() );
    connect( process, &ReosProcess::sendInformation, this, &ReosHydraulicSimulationConsole::receiveInformation );

    connect( ui->mStopButton, &QPushButton::clicked, this, &ReosHydraulicSimulationConsole::onStopSimulation );
    connect( mProcess, &ReosProcess::finished, ui->mStopButton, [this] {ui->mStopButton->setEnabled( false );} );
    connect( ui->mButtonBack, &QToolButton::clicked, this, &ReosHydraulicSimulationConsole::backToPreviousPage );
  }
  else
  {
    ui->mProgressBar->setValue( 0 );
  }
}

ReosHydraulicSimulationConsole::~ReosHydraulicSimulationConsole()
{
  delete ui;
}

void ReosHydraulicSimulationConsole::receiveInformation( const QString &info )
{
  ui->mText->append( info );

  if ( !mProcess.isNull() )
    ui->mProgressBar->setValue( mProcess->currentProgression() );
}

void ReosHydraulicSimulationConsole::onStopSimulation()
{
  if ( QMessageBox::question( this, tr( "Cancel simulation" ), tr( "Do you want to cancel this simulation" ) ) == QMessageBox::Yes )
  {
    if ( !mProcess.isNull() )
      mProcess->stop( true );
    emit backToPreviousPage();
  }
}
