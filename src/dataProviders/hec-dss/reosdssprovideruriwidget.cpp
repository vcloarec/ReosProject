/***************************************************************************
  reosdssprovideruriwidget.cpp - ReosDssProviderUriWidget

 ---------------------
 begin                : 25.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosdssprovideruriwidget.h"
#include "ui_reosdssprovideruriwidget.h"

#include <QFileDialog>

#include "reosdssutils.h"
#include "reosdssprovider.h"
#include "reossettings.h"
#include "reosgriddedrainitem.h"

ReosDssProviderUriWidget::ReosDssProviderUriWidget( QWidget *parent ) :
  ReosDataProviderUriWidget( parent ),
  ui( new Ui::ReosDssProviderUriWidget )
{
  ui->setupUi( this );

  connect( ui->mFileButton, &QToolButton::clicked, this, &ReosDssProviderUriWidget::onFileButtonPressed );
}

ReosDssProviderUriWidget::~ReosDssProviderUriWidget()
{
  delete ui;
}

void ReosDssProviderUriWidget::setUri( const QString &uri )
{
  QFileInfo fileInfo( ReosDssProviderBase::fileNameFromUri( uri ) );
  QString fileName = fileInfo.dir().filePath( fileInfo.baseName() + QStringLiteral( ".dss" ) );
  ReosDssPath path = ReosDssProviderBase::dssPathFromUri( uri );
  ui->mFile->setText( fileName );
  setPath( path );
}

const QString ReosDssProviderUriWidget::uri() const
{
  ReosDssPath path;

  path.setGroup( ui->partA->text() );
  path.setLocation( ui->partB->text() );
  path.setParameter( ui->partC->text() );
  path.setStartDate( ui->partD->text() );
  path.setTimeInterval( ui->partE->text() );
  path.setVersion( ui->partF->text() );

  QString fileName = ui->mFile->text();

  return ReosDssProviderBase::createUri( fileName, path );
}

void ReosDssProviderUriWidget::setDataType( const QString &dataType )
{
  if ( dataType == ReosGriddedRainfall::staticType() )
  {
    ui->partA->setEnabled( true );
    ui->partB->setEnabled( true );
    ui->partC->setEnabled( false );
    ui->partD->setEnabled( false );
    ui->partE->setEnabled( false );
    ui->partF->setEnabled( true );
  }
}

void ReosDssProviderUriWidget::setPath( const ReosDssPath &path )
{
  ui->partA->setText( path.group() );
  ui->partB->setText( path.location() );
  ui->partC->setText( path.parameter() );
  ui->partD->setText( path.startDate() );
  ui->partE->setText( path.timeInterval() );
  ui->partF->setText( path.version() );
}

void ReosDssProviderUriWidget::onFileButtonPressed()
{
  QFileInfo info( ui->mFile->text() );
  QString path;
  if ( info.dir().exists() )
  {
    path = info.dir().path();
  }
  else
  {
    ReosSettings settings;
    if ( settings.contains( QStringLiteral( "Rainfall/fileDirectory" ) ) )
      path = settings.value( QStringLiteral( "Rainfall/fileDirectory" ) ).toString();
  }

  const QString fileName = QFileDialog::getSaveFileName(
                             this,
                             tr( "DSS File" ),
                             path,
                             tr( "DSS file (*.dss)" ) );

  if ( !fileName.isEmpty() )
    ui->mFile->setText( fileName );
}
