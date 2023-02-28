#include "reoshecrassimulationimportwidget.h"
#include "ui_reoshecrassimulationimport.h"

#include <QFileDialog>

#include "reoshecrassimulation.h"
#include "reossettings.h"


ReosHecRasSimulationImportWidget::ReosHecRasSimulationImportWidget( QWidget *parent )
  : ReosImportHydraulicStructureWidget( parent )
  , ui( new Ui::ReosHecRasSimulationImportWidget )
{
  ui->setupUi( this );

  connect( ui->mProjectFileButton, &QToolButton::clicked, this, &ReosHecRasSimulationImportWidget::onProjectFileButtonPressed );
  connect( ui->mProjectFileLineEdit, &QLineEdit::textEdited, this, &ReosHecRasSimulationImportWidget::onFileNameChanged );
  connect( ui->mCheckBoxCreateScheme, &QCheckBox::clicked, this, [this]( bool checked )
  {
    ui->mCheckBoxRemoveScheme->setEnabled( checked );
  } );

  ui->mCheckBoxRemoveScheme->setEnabled( ui->mCheckBoxCreateScheme->isChecked() );
  onFileNameChanged();
}

ReosHydraulicStructure2D *ReosHecRasSimulationImportWidget::importStructure2D( const ReosHydraulicNetworkContext &context ) const
{
  ReosHecRasStructureImporterSource source( ui->mProjectFileLineEdit->text(), context );
  std::unique_ptr<ReosHecRasStructureImporter> importer( source.createImporter() );
  ReosHecRasStructureImporter::CreationOptions options;
  options.createSchemeWithPlan = ui->mCheckBoxCreateScheme->isChecked();
  options.removePreviousScheme = ui->mCheckBoxRemoveScheme->isChecked() && options.createSchemeWithPlan;
  importer->setCreationOption( options );

  return ReosHydraulicStructure2D::create( importer.get(), context );
}

void ReosHecRasSimulationImportWidget::onProjectFileButtonPressed()
{
  ReosSettings settings;
  const QString dirName = settings.value( QStringLiteral( "ImportFile/directory" ) ).toString();
  const QString fileName = QFileDialog::getOpenFileName(
                             this,
                             tr( "Choose HEC-RAS project file" ),
                             dirName,
                             tr( "HEC-RAS project file *.prj" ) );

  if ( !fileName.isEmpty() )
  {
    ui->mProjectFileLineEdit->setText( fileName );
    QFileInfo fileInfo( fileName );
    settings.setValue( QStringLiteral( "ImportFile/directory" ), fileInfo.dir().path() );
    onFileNameChanged();
  }

}

void ReosHecRasSimulationImportWidget::onFileNameChanged()
{
  ReosHecRasProject project( ui->mProjectFileLineEdit->text() );
  mIsValid = project.isValid();
  emit isValidated( mIsValid );
  ui->mCheckBoxCreateScheme->setEnabled( mIsValid );
  ui->mCheckBoxRemoveScheme->setEnabled( mIsValid && ui->mCheckBoxCreateScheme->isChecked() );
}

bool ReosHecRasSimulationImportWidget::isValid() const
{
  return mIsValid;
}
