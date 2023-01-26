#include "reoshecrassimulationeditwidget.h"
#include "ui_reoshecrassimulationeditwidget.h"
#include "ui_reoshecrasengineconfigurationdialog.h"

#include <QPushButton>
#include <QMessageBox>

#include "reoshecrassimulationimportwidget.h"
#include "reoshecrassimulation.h"
#include "reoshecrascontroller.h"
#include "reossettings.h"
#include "reosnetworkcompatibilitydialog.h"

ReosHecRasSimulationEditWidget::ReosHecRasSimulationEditWidget( ReosHecRasSimulation *simulation, QWidget *parent )
  : QWidget( parent )
  , ui( new Ui::ReosHecRasSimulationEditWidget )
  , mSimulation( simulation )
{
  ui->setupUi( this );

  ReosHecRasSimulation *sim = qobject_cast<ReosHecRasSimulation *>( simulation );
  if ( sim )
  {
    const QStringList planIds = sim->project()->planIds();
    for ( const QString &id : planIds )
      ui->mPlansComboBox->addItem( sim->project()->planTitle( id ), id );

    ui->mPlansComboBox->setCurrentIndex( ui->mPlansComboBox->findData( simulation->currentPlan() ) );
  }

  connect( ui->mPlansComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onPlanChanged );
  onPlanChanged();

  ui->mComputeInterval->setConfig( ReosHecRasIntervalComputationCombo::Small );
  ui->mOutputInterval->setConfig( ReosHecRasIntervalComputationCombo::Big );
  ui->mDetailedInterval->setConfig( ReosHecRasIntervalComputationCombo::Big );
  ui->mMappingInterval->setConfig( ReosHecRasIntervalComputationCombo::Big );

  ui->mComputeInterval->setInterval( simulation->computeInterval() );
  ui->mOutputInterval->setInterval( simulation->outputInterval() );
  ui->mDetailedInterval->setInterval( simulation->detailedInterval() );
  ui->mMappingInterval->setInterval( simulation->mappingInterval() );
  ui->mMinInputInterval->setInterval( simulation->minimumInterval() );

  connect( ui->mComputeInterval, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onIntervalChanged );
  connect( ui->mOutputInterval, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onIntervalChanged );
  connect( ui->mDetailedInterval, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onIntervalChanged );
  connect( ui->mMappingInterval, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onIntervalChanged );
  connect( ui->mMinInputInterval, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onIntervalChanged );
}

ReosHecRasSimulationEditWidget::~ReosHecRasSimulationEditWidget()
{
  delete ui;
}

void ReosHecRasSimulationEditWidget::onPlanChanged()
{
  const QString prevPlan = mSimulation->project()->currentPlanId();

  const QString newPlanId = ui->mPlansComboBox->currentData().toString();

  const ReosHydraulicNetworkElementCompatibilty compatibility = mSimulation->checkPlanCompability( newPlanId );
  if ( !compatibility.isCompatible )
  {
    ReosGuiContext context( this );
    ReosNetworkCompatibilityDialog *diag =
      new ReosNetworkCompatibilityDialog( tr( "The selected HEC-RAS plan is incompatible with the state"
                                          " of the hydraulic network for the following reason(s):" ),
                                          compatibility,
                                          tr( "If you continue, some elements of the network could be altered or removed definitively.\n"
                                              "Do you want to continue ?" ),
                                          context );
    if ( !diag->exec() )
    {
      ui->mPlansComboBox->blockSignals( true );
      ui->mPlansComboBox->setCurrentIndex( ui->mPlansComboBox->findData( prevPlan ) );
      ui->mPlansComboBox->blockSignals( false );
      diag->deleteLater();
      return;
    }

    diag->deleteLater();
  }
  const ReosHecRasPlan &plan = mSimulation->project()->plan( newPlanId );
  const QString currentGeometryId = plan.geometryFile();
  const ReosHecRasGeometry &geometry = mSimulation->project()->geometry( currentGeometryId );

  mSimulation->setCurrentPlan( newPlanId );
  ui->mGeometryLabel->setText( geometry.title() );
}

void ReosHecRasSimulationEditWidget::onIntervalChanged()
{
  mSimulation->setComputeInterval( ui->mComputeInterval->currentInterval() );
  mSimulation->setOutputInterval( ui->mOutputInterval->currentInterval() );
  mSimulation->setDetailledInterval( ui->mDetailedInterval->currentInterval() );
  mSimulation->setMappingInterval( ui->mMappingInterval->currentInterval() );
  mSimulation->setMinimumInterval( ui->mMinInputInterval->currentInterval() );
}

ReosImportHydraulicStructureWidget *ReosHecRasSimulationEditWidgetFactory::simulationImportWidget( QWidget *parent ) const
{
  return new ReosHecRasSimulationImportWidget( parent );
}

QString ReosHecRasSimulationEditWidgetFactory::key() const { return ReosHecRasSimulation::staticKey(); }

QWidget *ReosHecRasSimulationEditWidgetFactory::simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, const ReosGuiContext &guiContext ) const
{
  return new ReosHecRasSimulationEditWidget( qobject_cast<ReosHecRasSimulation *>( simulation ), guiContext.parent() );
}

QDialog *ReosHecRasSimulationEditWidgetFactory::engineConfigurationDialog( QWidget *parent ) const
{
  return new ReosHecrasConfigurationEngineDialog( parent );
}

REOSEXTERN ReosHydraulicSimulationWidgetFactory *simulationWidgetFactory()
{
  return new ReosHecRasSimulationEditWidgetFactory;
}

ReosHecrasConfigurationEngineDialog::ReosHecrasConfigurationEngineDialog( QWidget *parent )
  : QDialog( parent )
  , ui( new Ui::ReosHecrasConfigurationEngineDialog )
{
  ui->setupUi( this );

  QStringList versions = ReosHecRasController::availableVersion();
  if ( versions.isEmpty() )
  {
    ui->mComboVersion->addItem( tr( "No version found" ) );
    ui->mComboVersion->setCurrentIndex( 0 );
    ui->buttonBox->button( QDialogButtonBox::Ok )->setEnabled( false );
  }
  else
  {
    ui->mComboVersion->addItems( versions );
    ReosSettings settings;
    int currentIndex = ui->mComboVersion->findText(
                         settings.value( QStringLiteral( "/engine/hecras/version" ) ).toString() );

    if ( currentIndex == -1 )
      ui->mComboVersion->setCurrentIndex( versions.count() - 1 );
    else
      ui->mComboVersion->setCurrentIndex( currentIndex );
  }
}

ReosHecrasConfigurationEngineDialog::~ReosHecrasConfigurationEngineDialog()
{
  delete ui;
}

void ReosHecrasConfigurationEngineDialog::onAccepted()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/engine/hecras/version" ), ui->mComboVersion->currentText() );
}

ReosHecRasIntervalComputationCombo::ReosHecRasIntervalComputationCombo( QWidget *parent ) : QComboBox( parent )
{
  const QList<ReosDuration> durations = ReosHecRasPlan::computationIntervals().keys();
  for ( int i = 0; i < durations.count(); ++i )
    addItem( durations.at( i ).toString( 0 ), i );
}

void ReosHecRasIntervalComputationCombo::setConfig( Configuration config )
{
  clear();
  int start = 0 ;
  int end = 0;
  const QList<ReosDuration> durations = ReosHecRasPlan::computationIntervals().keys();
  switch ( config )
  {
    case Small:
      start = 0;
      end = durations.count() - 3;
      break;
    case Big:
      start = 5;
      end = durations.count();
      break;
  }

  for ( int i = start; i < end; ++i )
    addItem( durations.at( i ).toString( 0 ), i );
}

ReosDuration ReosHecRasIntervalComputationCombo::currentInterval() const
{
  const QList<ReosDuration> durations = ReosHecRasPlan::computationIntervals().keys();
  return ReosDuration( durations.at( currentData().toInt() ) );
}

void ReosHecRasIntervalComputationCombo::setInterval( const ReosDuration &duration )
{
  const QList<ReosDuration> durations = ReosHecRasPlan::computationIntervals().keys();
  int i = durations.indexOf( duration );
  int index = findData( i );
  setCurrentIndex( index );
}
