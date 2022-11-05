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
}

void ReosHecRasSimulationImportWidget::importStructure2D( const ReosHydraulicNetworkContext &context ) const
{
  ReosHydraulicStructure2D::create( new ReosHecRasStructureImporter( ui->mProjectFileLineEdit->text() ), context );
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
  }

}
