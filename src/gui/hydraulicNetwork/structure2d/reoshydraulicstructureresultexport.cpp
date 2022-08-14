#include "reoshydraulicstructureresultexport.h"
#include "ui_reoshydraulicstructureresultexport.h"

#include <QFileDialog>
#include <QPushButton>

#include "reoshydraulicstructure2d.h"
#include "reoshydraulicscheme.h"
#include "reoshydraulicnetwork.h"

ReosHydraulicStructureResultExport::ReosHydraulicStructureResultExport( ReosHydraulicStructure2D *structure,  const QString &currentSchemeId, QWidget *parent )
  : QDialog( parent ),
    ui( new Ui::ReosHydraulicStructureResultExport )
  , mStructure( structure )
  , mNetwork( structure ? structure->hydraulicNetworkContext().network() : nullptr )
{
  ui->setupUi( this );
  if ( structure )
  {
    ui->mSchemeListView->setSchemeCollection( structure->hydraulicNetworkContext().network()->hydraulicSchemeCollection() );
    ui->mSchemeListView->setCurrentScheme( currentSchemeId );
    onCurrentSchemeChange();
  }

  ui->mKeepLayer->setChecked( true );

  connect( ui->mSchemeListView->selectionModel(), &QItemSelectionModel::currentChanged, this, &ReosHydraulicStructureResultExport::onCurrentSchemeChange );
  connect( ui->mQGISFileButton, &QToolButton::clicked, this, &ReosHydraulicStructureResultExport::onFileButtonClicked );
  connect( ui->mQGISProjectFileLineEdit, &QLineEdit::textChanged, this, &ReosHydraulicStructureResultExport::onQGISFileChanged );

  onQGISFileChanged();
}

ReosHydraulicStructureResultExport::~ReosHydraulicStructureResultExport()
{
  delete ui;
}

void ReosHydraulicStructureResultExport::accept()
{
  mStructure->exportResultAsMeshInGisProject( ui->mQGISProjectFileLineEdit->text(), ui->mKeepLayer->isChecked() );

  QDialog::accept();
}

void ReosHydraulicStructureResultExport::onCurrentSchemeChange()
{
  ReosHydraulicScheme *currentScheme = ui->mSchemeListView->currentScheme();

  if ( !currentScheme )
    return;

  ReosHydraulicSimulation *sim = mStructure->simulation( currentScheme );

  if ( sim )
  {
    ui->mLabelEngine->setText( sim->engineName() );

    if ( mStructure->hasResults( currentScheme ) )
    {
      ui->mLabelLastRun->setText( QLocale().toString( mStructure->resultsRunDateTime( currentScheme->id() ), QLocale::ShortFormat ) );
      ui->mLabelStartTime->setText( QLocale().toString( currentScheme->startTime()->value(), QLocale::ShortFormat ) );
      ui->mLaberEndTime->setText( QLocale().toString( currentScheme->endTime()->value(), QLocale::ShortFormat ) );
      ui->mTimeStepCount->setText( QLocale().toString( mStructure->resultsTimeStepCount( currentScheme->id() ) ) );
    }
    else
    {
      ui->mLabelLastRun->setText( tr( "No result" ) );
      ui->mLabelStartTime->setText( "-" );
      ui->mLaberEndTime->setText( "-" );
      ui->mTimeStepCount->setText( "-" );
    }
  }
  else
  {
    ui->mLabelEngine->setText( tr( "No simulation defined" ) );
    ui->mLabelLastRun->setText( QString() );
    ui->mLabelLastRun->setText( tr( "No result" ) );
    ui->mLabelStartTime->setText( "-" );
    ui->mLaberEndTime->setText( "-" );
    ui->mTimeStepCount->setText( "-" );
  }
}

void ReosHydraulicStructureResultExport::onFileButtonClicked()
{
  const QString currentFile = ui->mQGISProjectFileLineEdit->text();
  const QString newFile = QFileDialog::getSaveFileName( this,
                          tr( "QGIS project file" ),
                          currentFile,
                          tr( "QGIS Project File (*.qgz)" ) );

  if ( newFile == currentFile || newFile.isEmpty() )
    return;

  ui->mQGISProjectFileLineEdit->setText( newFile );
}

void ReosHydraulicStructureResultExport::onQGISFileChanged()
{
  QFileInfo fileInfo( ui->mQGISProjectFileLineEdit->text() );
  QDir dir = fileInfo.dir();

  ui->buttonBox->button( QDialogButtonBox::Ok )->setEnabled( fileInfo.suffix() == QStringLiteral( "qgz" ) && dir.exists() );
}
