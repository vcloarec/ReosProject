/***************************************************************************
  reosnotificationbutton.cpp - ReosNotificationButton

 ---------------------
 begin                : 23.12.2021
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
#include "reosnotificationbutton.h"

#include <QTextBrowser>
#include <QWidgetAction>
#include <QMenu>

ReosNotificationButton::ReosNotificationButton( QWidget *parent ): QToolButton( parent )
{
  setToolButtonStyle( Qt::ToolButtonTextBesideIcon );
  QMenu *menu = new QMenu( this );
  setPopupMode( QToolButton::InstantPopup );
  setMenu( menu );

  QWidgetAction *wa = new QWidgetAction( this );
  mTextBrowser = new QTextBrowser( this );
  wa->setDefaultWidget( mTextBrowser );
  menu->addAction( wa );

  setAutoRaise( true );
  setIconSize( QSize( 24, 24 ) );
}

void ReosNotificationButton::setMessage( const ReosModule::Message &message )
{
  switch ( message.type )
  {
    case ReosModule::Simple:
      setText( QString() );
      setIcon( QIcon() );
      break;
    case ReosModule::Order:
      break;
    case ReosModule::Warning:
      setText( tr( "Warning" ) );
      setIcon( QIcon( QStringLiteral( ":/images/warning.svg" ) ) );
      break;
    case ReosModule::Error:
      setText( tr( "Error" ) );
      setIcon( QIcon( QStringLiteral( ":/images/error.svg" ) ) );
      break;
  }

  mTextBrowser->setText( message.text );
}
