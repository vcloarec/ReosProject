/***************************************************************************
                      reosmessagebox.cpp
                     --------------------------------------
Date                 : 30-12-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
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



void ReosMessageBox::receiveMessage( const QString &mes, Type type )
{
  QString message;

  switch ( type )
  {
    case ReosMessageBox::error:
      message.append( QTime::currentTime().toString() );
      message.append( " : " );
      message.append( tr( "Erreur : " ) );
      ui->textBrowser->setTextColor( Qt::red );
      break;
    case ReosMessageBox::warning:
      message.append( QTime::currentTime().toString() );
      message.append( " : " );
      message.append( tr( "Attention : " ) );
      ui->textBrowser->setTextColor( QColor( 200, 100, 0 ) );
      break;
    case ReosMessageBox::message:
      message.append( QTime::currentTime().toString() );
      message.append( " : " );
      ui->textBrowser->setTextColor( QColor( 0, 150, 0 ) );
      break;
    case ReosMessageBox::order:
      message.append( "--> " );
      ui->textBrowser->setTextColor( Qt::black );
      break;
  }

  message.append( mes );

  ui->textBrowser->append( message );
}

void ReosMessageBox::clean()
{
  ui->textBrowser->clear();
}
