/***************************************************************************
  reoshydraulic2dsimulationwidget.cpp - ReosHydraulic2DSimulationWidget

 ---------------------
 begin                : 29.3.2022
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
#include "reoshydraulic2dsimulationwidget.h"
#include "ui_reoshydraulic2dsimulationwidget.h"

#include <QDir>
#include <QLibrary>

#include "reoshydraulicsimulation.h"
#include "reoshydraulicstructure2d.h"
#include "reossimulationinitialcondition.h"
#include "reosformwidget.h"

ReosHydraulic2DSimulationWidget::ReosHydraulic2DSimulationWidget( ReosHydraulicStructure2D *structure, QWidget *parent )
  : QWidget( parent )
  , ui( new Ui::ReosHydraulic2DSimulationWidget )
  , mStructure( structure )
{
  ui->setupUi( this );

  updateSimulationCombo();
  setCurrentSimulation( mStructure->currentSimulation() );
  connect( ui->mAddSimulationButton, &QToolButton::clicked, this, &ReosHydraulic2DSimulationWidget::onAddSimulation );
  connect( ui->mExistingSimulationCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHydraulic2DSimulationWidget::onSimulationIndexChanged );

  connect( mStructure, &ReosHydraulicStructure2D::currentSimulationChanged, this, [this]
  {
    ui->mExistingSimulationCombo->blockSignals( true );
    ui->mExistingSimulationCombo->setCurrentIndex( mStructure->currentSimulationIndex() );
    ui->mExistingSimulationCombo->blockSignals( false );
    setCurrentSimulation( mStructure->currentSimulation() );
  } );
}

ReosHydraulic2DSimulationWidget::~ReosHydraulic2DSimulationWidget()
{
  delete ui;
}

void ReosHydraulic2DSimulationWidget::onAddSimulation()
{
  ReosFormDialog *dial = new ReosFormDialog( this );

  QComboBox *engineCombo = new QComboBox;
  QMap<QString, QString> engines = ReosSimulationEngineRegistery::instance()->availableEngine();
  for ( auto it = engines.begin(); it != engines.end(); ++it )
    engineCombo->addItem( it.value(), it.key() );

  dial->addWidget( engineCombo );
  ReosParameterString simulationName( tr( "Simulation name" ), false );
  QString name = tr( "Simulation %1" );
  QStringList existingNames = mStructure->simulationNames();
  int ind = existingNames.count() + 1;
  while ( existingNames.contains( name.arg( ind ) ) )
    ind++;

  simulationName.setValue( name.arg( ind ) );
  dial->addParameter( &simulationName );

  if ( dial->exec() )
  {
    if ( mStructure->addSimulation( engineCombo->currentData().toString() ) )
    {
      mStructure->currentSimulation()->setName( simulationName.value() );
      setCurrentSimulation( mStructure->currentSimulation() );
      updateSimulationCombo();
    }
  }

  dial->deleteLater();
}

void ReosHydraulic2DSimulationWidget::onSimulationIndexChanged( int newIndex )
{
  mStructure->setCurrentSimulation( newIndex );
  setCurrentSimulation( mStructure->currentSimulation() );
}

void ReosHydraulic2DSimulationWidget::setCurrentSimulation( ReosHydraulicSimulation *simulation )
{
  if ( mCurrentEditingWidget )
  {
    ui->mEditWidgetLayout->removeWidget( mCurrentEditingWidget );
    mCurrentEditingWidget->deleteLater();
  }

  mCurrentEditingWidget = ReosHydraulicSimulationWidgetRegistery::instance()->createEditingWidget( simulation, this );
  if ( mCurrentEditingWidget )
    ui->mEditWidgetLayout->addWidget( mCurrentEditingWidget );
}

void ReosHydraulic2DSimulationWidget::updateSimulationCombo()
{
  ui->mExistingSimulationCombo->clear();
  ui->mExistingSimulationCombo->addItems( mStructure->simulationNames() );
  ui->mExistingSimulationCombo->setCurrentIndex( mStructure->currentSimulationIndex() );

}

ReosHydraulicSimulationWidgetRegistery *ReosHydraulicSimulationWidgetRegistery::sInstance = nullptr;

ReosHydraulicSimulationWidgetRegistery::ReosHydraulicSimulationWidgetRegistery()
{

}

QWidget *ReosHydraulicSimulationWidgetRegistery::createEditingWidget( ReosHydraulicSimulation *simulation, QWidget *parent )
{
  if ( !simulation )
    return nullptr;

  auto it = mFactories.find( simulation->key() );
  if ( it == mFactories.end() )
    return nullptr;

  return it->second->simulationSettingsWidget( simulation, parent );
}

QDialog *ReosHydraulicSimulationWidgetRegistery::createConfigurationDialog( const QString &key, QWidget *parent )
{
  auto it = mFactories.find( key );
  if ( it == mFactories.end() )
    return nullptr;

  return it->second->engineConfigurationDialog( parent );
}

ReosHydraulicSimulationWidgetRegistery *ReosHydraulicSimulationWidgetRegistery::instance()
{
  if ( !sInstance )
  {
    sInstance = new ReosHydraulicSimulationWidgetRegistery();
    sInstance->loadDynamicLibrary();
  }

  return sInstance;
}

void ReosHydraulicSimulationWidgetRegistery::registerEngineFactory( ReosHydraulicSimulationWidgetFactory *factory )
{
  mFactories[factory->key()] = std::unique_ptr<ReosHydraulicSimulationWidgetFactory>( factory );
}

void ReosHydraulicSimulationWidgetRegistery::loadDynamicLibrary()
{
  QString enginesPath = QCoreApplication::applicationDirPath();
  QDir enginesDir( enginesPath );
  if ( enginesDir.cd( QStringLiteral( REOS_SIMULATION_ENGINES ) ) )
    enginesPath = enginesDir.absolutePath();
  else
  {
    enginesPath = REOS_SIMULATION_ENGINES;
    enginesDir = QDir( enginesPath );
  }

  enginesDir.setSorting( QDir::Name | QDir::IgnoreCase );
  enginesDir.setFilter( QDir::Files | QDir::NoSymLinks );

#if defined(Q_OS_WIN) || defined(__CYGWIN__)
  enginesDir.setNameFilters( QStringList( "*.dll" ) );
#else
  enginesDir.setNameFilters( QStringList( QStringLiteral( "*.so" ) ) );
#endif

  typedef ReosHydraulicSimulationWidgetFactory *factory_function( );

  const QFileInfoList files = enginesDir.entryInfoList();
  for ( const QFileInfo &file : files )
  {
    QLibrary library( file.filePath() );
    if ( library.load() )
    {
      QFunctionPointer fcp = library.resolve( "simulationWidgetFactory" );
      factory_function *func = reinterpret_cast<factory_function *>( fcp );

      if ( func )
      {
        ReosHydraulicSimulationWidgetFactory *simulationWidgetFactory = func();
        registerEngineFactory( simulationWidgetFactory );
      }
    }
  }
}
