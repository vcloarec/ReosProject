#include "reoshecrassimulationeditwidget.h"
#include "ui_reoshecrassimulationeditwidget.h"
#include "ui_reoshecrasengineconfigurationdialog.h"

#include <QPushButton>

#include "reoshecrassimulationimportwidget.h"
#include "reoshecrassimulation.h"
#include "reoshecrascontroller.h"
#include "reossettings.h"

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

  ui->mComputeInterval->setInterval( simulation->computeInterval() );
  ui->mOutputInterval->setInterval( simulation->outputInterval() );
  ui->mDetailedInterval->setInterval( simulation->detailedInterval() );
  ui->mMappingInterval->setInterval( simulation->mappingInterval() );
  ui->mMinInputInterval->setInterval( simulation->minimumInterval() );

  connect( ui->mComputeInterval, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onIntervalChanged );
}

ReosHecRasSimulationEditWidget::~ReosHecRasSimulationEditWidget()
{
  delete ui;
}

void ReosHecRasSimulationEditWidget::onPlanChanged()
{
  const QString currentPlanId = ui->mPlansComboBox->currentData().toString();
  const ReosHecRasPlan &plan = mSimulation->project()->plan( currentPlanId );
  const QString currentGeometryId = plan.geometryFile();
  const ReosHecRasGeometry &geometry = mSimulation->project()->geometry( currentGeometryId );

  mSimulation->setCurrentPlan( currentPlanId );
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
