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

  mTimer.setInterval( mRefreshInterval );
}

ReosProcessControler::~ReosProcessControler()
{
  delete ui;
}

void ReosProcessControler::closeEvent( QCloseEvent *event )
{
  event->accept();
}

int ReosProcessControler::exec()
{
  mProcess->startOnOtherThread();
  mTimer.start();
  int res = QDialog::exec();
  mTimer.stop();
  return res;
}

void ReosProcessControler::refresh()
{
  if ( !mProcess )
    return;

  ui->labelInformationText->setText( mProcess->currentInformation() );
  ui->progressBar->setMaximum( mProcess->maxProgression() );
  ui->progressBar->setValue( mProcess->currentProgression() );

}

void ReosProcessControler::onCancel()
{
  if ( mProcess )
    mProcess->stop( true );
  ui->labelInformationText->setText( tr( "Operation canceled..." ) );
}

void ReosProcessControler::onFinished()
{
  close();
}
