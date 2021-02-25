/***************************************************************************
  reosimportfromtextfile.cpp - ReosImportFromTextFile

 ---------------------
 begin                : 9.2.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosimportfromtextfile.h"
#include "ui_reosimportfromtextfile.h"

#include <QFileDialog>
#include <QMessageBox>

#include "reossettings.h"
#include "reostextfiledata.h"


ReosImportFromTextFile::ReosImportFromTextFile( ReosTextFileData *data, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosImportFromTextFile )
  , mData( data )
{
  ui->setupUi( this );

  ui->tableView->setModel( data );

  ui->tableView->horizontalHeader()->setSectionResizeMode( QHeaderView::ResizeToContents );
  ui->tableView->horizontalHeader()->setCascadingSectionResizes( true );

  connect( ui->toolButtonFile, &QToolButton::clicked, this, &ReosImportFromTextFile::onFileNameButton );
  connect( ui->lineEditFileName, &QLineEdit::textChanged, this, &ReosImportFromTextFile::onFileNameEdited );
  connect( ui->spinBoxHeaderLine, QOverload<int>::of( &QSpinBox::valueChanged ), this, &ReosImportFromTextFile::onHeaderLineChanged );
  connect( ui->spinBoxFirstLine, QOverload<int>::of( &QSpinBox::valueChanged ), this, &ReosImportFromTextFile::onFirstLineChanged );
  connect( ui->comboBoxDelimiter, QOverload<int>::of( &QComboBox::highlighted ), this, &ReosImportFromTextFile::onComboDelimitersChanged );
  connect( ui->comboBoxDelimiter, &QComboBox::editTextChanged, this, &ReosImportFromTextFile::onComboDelimitersChanged );
  connect( ui->toolButtonAddDelimiter, &QToolButton::clicked, this, &ReosImportFromTextFile::onAddDelimiters );
  connect( ui->toolButtonRemoveDelimiter, &QToolButton::clicked, this, &ReosImportFromTextFile::onRemoveDelimiters );

  onComboDelimitersChanged();
}

ReosImportFromTextFile::~ReosImportFromTextFile()
{
  delete ui;
}

QComboBox *ReosImportFromTextFile::createAvailableFieldComboBox( QWidget *parent )
{
  QComboBox *cmb = new QComboBox( parent );

  cmb->addItems( mData->headers() );

  connect( mData, &ReosTextFileData::headersChanged, cmb, [cmb]( const QStringList & headers )
  {
    cmb->clear();
    cmb->addItems( headers );
  } );

  return cmb;
}

void ReosImportFromTextFile::onFileNameButton()
{
  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "ImportFile/directory" ) ).toString();
  QString fileName = QFileDialog::getOpenFileName( this, tr( "Choose file" ), dir );

  if ( fileName.isEmpty() )
    return;

  QFileInfo fileInfo( fileName );
  if ( !fileInfo.exists() )
  {
    QMessageBox::critical( this, tr( "Import Text File" ), tr( "File does not exist!" ) );
    return;
  }

  settings.setValue( QStringLiteral( "ImportFile/directory" ), fileInfo.path() );

  ui->lineEditFileName->setText( fileName );
  ui->spinBoxHeaderLine->setFocus();

}

void ReosImportFromTextFile::onFileNameEdited()
{
  mData->setFileName( ui->lineEditFileName->text() );
}

void ReosImportFromTextFile::onHeaderLineChanged()
{
  int newHeaderLine = ui->spinBoxHeaderLine->value();

  if ( mData->headerLine() == newHeaderLine )
    return;

  if ( ui->spinBoxFirstLine->value() <= newHeaderLine )
  {
    ui->spinBoxFirstLine->blockSignals( true );
    ui->spinBoxFirstLine->setValue( newHeaderLine + 1 );
    ui->spinBoxFirstLine->blockSignals( false );
  }

  mData->setLines( newHeaderLine, ui->spinBoxFirstLine->value() );
}

void ReosImportFromTextFile::onFirstLineChanged()
{
  int newDataLine = ui->spinBoxFirstLine->value();
  if ( mData->firstDataLine() == newDataLine )
    return;

  if ( newDataLine <= ui->spinBoxHeaderLine->value() )
  {
    ui->spinBoxFirstLine->blockSignals( true );
    ui->spinBoxFirstLine->setValue( ui->spinBoxHeaderLine->value() + 1 );
    ui->spinBoxFirstLine->blockSignals( false );
  }
  mData->setLines( ui->spinBoxHeaderLine->value(), ui->spinBoxFirstLine->value() );
}

void ReosImportFromTextFile::onComboDelimitersChanged()
{
  const QStringList currentDelimiters = mData->delimiters();
  QString currentText = ui->comboBoxDelimiter->currentText();
  ui->toolButtonAddDelimiter->setEnabled( !currentDelimiters.contains( currentText ) && !currentText.isEmpty() );
  ui->toolButtonRemoveDelimiter->setEnabled( currentDelimiters.contains( ui->comboBoxDelimiter->currentText() ) && !currentText.isEmpty() );
}

void ReosImportFromTextFile::onAddDelimiters()
{
  QStringList current = mData->delimiters();
  current.append( ui->comboBoxDelimiter->currentText() );
  mData->setDelimiters( current );
  ui->labelDelimiters->setText( current.join( QStringLiteral( " | " ) ) );
  onComboDelimitersChanged();
}

void ReosImportFromTextFile::onRemoveDelimiters()
{
  QStringList current = mData->delimiters();
  current.removeOne( ui->comboBoxDelimiter->currentText() );
  mData->setDelimiters( current );
  ui->labelDelimiters->setText( current.join( QStringLiteral( " | " ) ) );
  onComboDelimitersChanged();
}
