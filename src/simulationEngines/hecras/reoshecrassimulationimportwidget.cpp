#include "ReosHecRasSimulationImportWidget.h"
#include "ui_reoshecrassimulationimport.h"

#include <QFileDialog>

#include "reoshecrassimulation.h"
#include "reossettings.h"


ReosHecRasSimulationImportWidget::ReosHecRasSimulationImportWidget(QWidget* parent)
	:ReosImportHydraulicStructureWidget(parent)
	,ui (new Ui::ReosHecRasSimulationImportWidget)
{
	ui->setupUi(this);

	ui->mVersionCombo->addItems(ReosHecrasController::availableVersion());
	ui->mVersionCombo->setCurrentIndex(ui->mVersionCombo->count() - 1);

	connect(ui->mProjectFileButton, &QToolButton::clicked, this, &ReosHecRasSimulationImportWidget::onProjectFileButtonPressed);
}

void ReosHecRasSimulationImportWidget::importStructure2D(const ReosHydraulicNetworkContext& context) const
{
	const QString controllerVersion = ui->mVersionCombo->currentText();

	ReosHecRasStructureImporter importer(controllerVersion, ui->mProjectFileLineEdit->text());
	
	context.network()->addElement(ReosHydraulicStructure2D::create(&importer, context));

	return nullptr;
}

QString ReosHecRasSimulationEditWidgetFactory::key() const { return ReosHecRasSimulation::staticKey(); }

QDialog* ReosHecRasSimulationEditWidgetFactory::engineConfigurationDialog(QWidget* parent) const { return nullptr; }

QWidget* ReosHecRasSimulationEditWidgetFactory::simulationEngineDescription(QWidget* parent) const { return nullptr; }

ReosImportHydraulicStructureWidget* ReosHecRasSimulationEditWidgetFactory::simulationImportWidget(QWidget* parent) const
{ 
	return new ReosHecRasSimulationImportWidget(parent); 
}

void ReosHecRasSimulationImportWidget::onProjectFileButtonPressed()
{
	ReosSettings settings;
	const QString dirName = settings.value(QStringLiteral("ImportFile/directory")).toString();
	const QString fileName = QFileDialog::getOpenFileName(
		this,
		tr("Choose HEC-RAS project file"),
		dirName,
		tr("HEC-RAS project file *.prj"));

	if (!fileName.isEmpty())
	{
		ui->mProjectFileLineEdit->setText(fileName);
		QFileInfo fileInfo(fileName);
		settings.setValue(QStringLiteral("ImportFile/directory"), fileInfo.dir().path());
	}
		
}

REOSEXTERN ReosHydraulicSimulationWidgetFactory* simulationWidgetFactory()
{
	return new ReosHecRasSimulationEditWidgetFactory;
}
