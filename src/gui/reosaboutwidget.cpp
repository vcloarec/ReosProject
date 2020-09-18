/***************************************************************************
                      reosaboutwidget.cpp
                     --------------------------------------
Date                 : 23-05-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosaboutwidget.h"
#include "ui_reosaboutwidget.h"

ReosAboutWidget::ReosAboutWidget( QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosAboutWidget )
{
  ui->setupUi( this );
}

ReosAboutWidget::~ReosAboutWidget()
{
  delete ui;
}

void ReosAboutWidget::setSoftwareName( const QString &nom )
{
  ui->mLabelSoftName->setText( nom );
}

void ReosAboutWidget::setBan( const QPixmap &pixmap )
{
  ui->mLabelSoftName->setPixmap( pixmap );
}

void ReosAboutWidget::setVersion( const QString &version )
{
  ui->labelVersion->setText( version );
}

void ReosAboutWidget::setVersion( const ReosVersion &version )
{
  ui->mLabelSoftName->setText( version.getSoftName() );
  ui->labelVersion->setText( version.stringVersion() );
}


void ReosAboutWidget::setWebAddress( const QString &ad, const QString &from )
{
  QString text = QString( "<a href=\"https://%1%2\"> %1</a>" ).arg( ad ).arg( from );
  ui->mLabelWebAddress->setTextFormat( Qt::RichText );
  ui->mLabelWebAddress->setText( text );
  ui->mLabelWebAddress->setOpenExternalLinks( true );
}

void ReosAboutWidget::addLibrary( const QString &lib, const QString &version, const QString &webLink )
{
  QString text = lib;
  text.append( " " );
  text.append( version );
  text.append( "  :    " );
  if ( webLink != "" )
  {
    text.append( "<a href=\"http://" );
    text.append( webLink );
    text.append( "\"> " );
    text.append( webLink );
    text.append( "</a>" );

  }
  QLabel *label = new QLabel( text, this );
  label->setOpenExternalLinks( true );
  ui->biblioLayout->addWidget( label );
}

void ReosAboutWidget::setLicenceText( const QString &txt )
{
  ui->textBrowser->setText( txt );
}
