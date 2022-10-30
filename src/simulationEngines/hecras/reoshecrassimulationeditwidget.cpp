#include "reoshecrassimulationeditwidget.h"
#include "ui_reoshecrassimulationeditwidget.h"

#include "reoshecrassimulationimportwidget.h"
#include "reoshecrassimulation.h"

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

    const QString currentPlan = sim->project()->currentPlanId();
    ui->mPlansComboBox->setCurrentIndex( ui->mPlansComboBox->findData( currentPlan ) );
  }

  connect( ui->mPlansComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHecRasSimulationEditWidget::onPlanChanged );
  onPlanChanged();
}

ReosHecRasSimulationEditWidget::~ReosHecRasSimulationEditWidget()
{
  delete ui;
}

void ReosHecRasSimulationEditWidget::onPlanChanged()
{
  const QString currentPlanId = ui->mPlansComboBox->currentData().toString();
  const QString currentGeometryId = mSimulation->project()->plan( currentPlanId ).geometryFile();
  const ReosHecRasGeometry &geometry = mSimulation->project()->geometry( currentGeometryId );

  ui->mGeometryLabel->setText( geometry.title() );
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

REOSEXTERN ReosHydraulicSimulationWidgetFactory *simulationWidgetFactory()
{
  return new ReosHecRasSimulationEditWidgetFactory;
}
