#include "reosprocesscontroler.h"
#include "ui_reosprocesscontroler.h"
#include <QPushButton>

ReosProcessControler::ReosProcessControler( ReosProcess *process, QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosProcessControler ),
  mProcess( process )
{
  ui->setupUi( this );

  connect( &mTimer, &QTimer::timeout, this, &ReosProcessControler::refresh );
  connect( this, &QDialog::rejected, this, &ReosProcessControler::onCancel );
  connect( ui->buttonBox->button( QDialogButtonBox::Cancel ), &QPushButton::clicked, this, &ReosProcessControler::onCancel );

  if ( mProcess )
  {
    ui->progressBar->setMaximum( mProcess->maxProgression() );
    connect( mProcess, &ReosProcess::finished, this, &ReosProcessControler::onFinished );
    connect( mProcess, &ReosProcess::sendInformation, ui->labelInformationText, &QLabel::setText );
  }

  ui->labelInformationText->setText( mProcess->currentInformation() );

  mTimer.setInterval( mRefreshInterval );
  mTimer.start();
}

ReosProcessControler::~ReosProcessControler()
{
  delete ui;
}

void ReosProcessControler::refresh()
{
  if ( !mProcess )
    return;

  ui->progressBar->setMaximum( mProcess->maxProgression() );
  ui->progressBar->setValue( mProcess->currentProgression() );

}

void ReosProcessControler::onCancel()
{
  if ( mProcess )
    mProcess->stopAsSoonAsPossible( true );
  ui->labelInformationText->setText( tr( "Operation canceled..." ) );
}

void ReosProcessControler::onFinished()
{
  close();
}
