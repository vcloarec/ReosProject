/***************************************************************************
  reosnetworkcompatibilitydialog.cpp - ReosNetworkCompatibilityDialog

 ---------------------
 begin                : 21.1.2023
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
#include "reosnetworkcompatibilitydialog.h"
#include "ui_reosnetworkcompatibilitydialog.h"

#include "reosguicontext.h"
#include "reoshydraulicnetwork.h"
#include "reosapplication.h"

ReosNetworkCompatibilityDialog::ReosNetworkCompatibilityDialog(
  const QString &introText,
  const ReosHydraulicNetworkElementCompatibilty compatibility,
  const QString &finalText,
  const ReosGuiContext &context )
  : QDialog( context.parent() )
  , ui( new Ui::ReosNetworkCompatibilityDialog )
{
  ui->setupUi( this );

  ui->mIntroLabel->setText( introText );
  ui->mFinalLabel->setText( finalText );
  ui->mTextBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );

  htmlText += QLatin1String( "<table class=\"list-view\">" );

  htmlText += QLatin1String( "<ul>" );
  for ( const QString &reason : compatibility.incompatibilityReasons )
  {
    htmlText += QLatin1String( "<li>" ) + reason + QLatin1String( "</li><br>" );
  }
  htmlText += QLatin1String( "</ul>" );
  ui->mTextBrowser->setText( htmlText );
}

ReosNetworkCompatibilityDialog::~ReosNetworkCompatibilityDialog()
{
  delete ui;
}
