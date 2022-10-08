#include "ReosHecRasSimulationImportWidget.h"
#include "ui_reoshecrassimulationimport.h"

#include "reoshecrassimulation.h"

ReosHecRasSimulationImportWidget::ReosHecRasSimulationImportWidget(QWidget* parent)
	:QWidget(parent)
	,ui (new Ui::ReosHecRasSimulationImportWidget)
{
	ui->setupUi(this);

	ui->mVersionCombo->addItems(ReosHecrasController::availableVersion());
}

QString ReosHecRasSimulationEditWidgetFactory::key() const { return ReosHecRasSimulation::staticKey(); }

QDialog* ReosHecRasSimulationEditWidgetFactory::engineConfigurationDialog(QWidget* parent) const { return nullptr; }

QWidget* ReosHecRasSimulationEditWidgetFactory::simulationEngineDescription(QWidget* parent) const { return nullptr; }

QWidget* ReosHecRasSimulationEditWidgetFactory::simulationImportWidget(QWidget* parent) const 
{ 
	return new ReosHecRasSimulationImportWidget(parent); 
}

REOSEXTERN ReosHydraulicSimulationWidgetFactory* simulationWidgetFactory()
{
	return new ReosHecRasSimulationEditWidgetFactory;
}
