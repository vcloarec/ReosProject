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
  ReosHecRasStructureImporter importer( ui->mProjectFileLineEdit->text() );

  context.network()->addElement( ReosHydraulicStructure2D::create( &importer, context ) );

  return;
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
