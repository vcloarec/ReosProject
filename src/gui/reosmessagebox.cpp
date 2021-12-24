/***************************************************************************
                      reosmessagebox.cpp
                     --------------------------------------
Date                 : 30-12-2018
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

#include "reosmessagebox.h"
#include "ui_reosmessagebox.h"
#include "QMessageBox"

ReosMessageBox::ReosMessageBox( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosMessageBox )
{
  ui->setupUi( this );
}

ReosMessageBox::~ReosMessageBox()
{
  delete ui;
}

void ReosMessageBox::receiveMessage( const ReosModule::Message &message, bool messageBox )
{
  QString messageText;

  switch ( message.type )
  {
    case ReosModule::Error:
      messageText.append( QTime::currentTime().toString() );
      messageText.append( " : " );
      messageText.append( tr( "Error: " ) );
      ui->textBrowser->setTextColor( Qt::red );
      break;
    case ReosModule::Warning:
      messageText.append( QTime::currentTime().toString() );
      messageText.append( " : " );
      messageText.append( tr( "Warning: " ) );
      ui->textBrowser->setTextColor( QColor( 200, 100, 0 ) );
      break;
    case ReosModule::Simple:
      messageText.append( QTime::currentTime().toString() );
      messageText.append( " : " );
      ui->textBrowser->setTextColor( QColor( 0, 150, 0 ) );
      break;
    case ReosModule::Order:
      messageText.append( "--> " );
      ui->textBrowser->setTextColor( Qt::black );
      break;
  }

  messageText.append( message.text );

  ui->textBrowser->append( messageText );

  if ( messageBox )
  {
    switch ( message.type )
    {
      case ReosModule::Simple:
      case ReosModule::Order:
        QMessageBox::information( this, QString(), messageText );
        break;
      case ReosModule::Warning:
        QMessageBox::warning( this, QString(), messageText );
        break;
      case ReosModule::Error:
        QMessageBox::critical( this, QString(), messageText );
        break;

    }
  }
}

void ReosMessageBox::clean()
{
  ui->textBrowser->clear();
}
