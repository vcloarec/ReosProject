/***************************************************************************
                      reosversion.cpp
                     --------------------------------------
Date                 : 07-04-2019
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

#include "reosversionmessagebox.h"
#include "reosremoteinformation.h"


ReosVersionMessageBox::ReosVersionMessageBox( QWidget *parent, const ReosVersion &version, bool start ):
  QMessageBox( QMessageBox::Information, tr( "New version available" ), "", QMessageBox::Ok, parent ),
  mVersion( version ),
  mStart( start )
{
  setTextFormat( Qt::RichText );

  ReosRemoteInformation *remoteInformation = new ReosRemoteInformation( this );
  connect( remoteInformation, &ReosRemoteInformation::informationready, this, &ReosVersionMessageBox::onReceiveInformation );
  remoteInformation->requestInformation();
}

void ReosVersionMessageBox::onReceiveInformation( const QVariantMap &map )
{
  bool newVersion = false;
  bool criticalInfo = false;

  QString message;

  QVariantList critical;
  if ( map.contains( QStringLiteral( "criticalMessage" ) ) )
  {
    QVariant var = map.value( QStringLiteral( "criticalMessage" ) );
    if ( var.type() == QVariant::List )
    {
      critical = var.toList();
    }

    for ( const QVariant &var : std::as_const( critical ) )
    {
      if ( var.type() != QVariant::Map )
        continue;

      QVariantMap varMap = var.toMap();

      if ( !varMap.contains( QStringLiteral( "version" ) ) )
        continue;

      QStringList versionSplit = varMap.value( QStringLiteral( "version" ) ).toString().split( QString( '.' ) );

      if ( versionSplit.count() != 3 )
        continue;

      ReosVersion versionAvailable( mVersion.getSoftName(), versionSplit.at( 0 ).toInt(), versionSplit.at( 1 ).toInt(), versionSplit.at( 2 ).toInt() );
      if ( versionAvailable == mVersion )
      {
        setIcon( QMessageBox::Critical );
        message.append( varMap.value( QStringLiteral( "message" ) ).toString() );
        message.append( QStringLiteral( "<br><br>" ) );
        criticalInfo = true;
        break;
      }
    }
  }

  if ( map.contains( QStringLiteral( "availableVersion" ) ) )
  {
    QString versionText = map.value( QStringLiteral( "availableVersion" ) ).toString();

    QStringList versionSplit = versionText.split( "." );

    if ( versionSplit.count() > 2 )
    {
      ReosVersion versionAvailable( mVersion.getSoftName(), versionSplit.at( 0 ).toInt(), versionSplit.at( 1 ).toInt(), versionSplit.at( 2 ).toInt() );
      newVersion = versionAvailable > mVersion;
      if ( !newVersion && !criticalInfo && mStart )
      {
        deleteLater();
        return;
      }
    }

    if ( newVersion )
    {
      message.append( tr( "A new version is available.\n" ) );

      message.append( tr( "Visit : " ) );
      message.append( QStringLiteral( "<a href=\"%1\"> %1</a>" ) );

      message = message.arg( map.value( QStringLiteral( "downloadUrl" ) ).toString() );

      setText( message );

    }
    else
    {
      message.append( tr( "No new version available." ) );
    }
  }
  else
  {
    message.append( tr( "Unable to check new version." ) );
  }

  setText( message );

  exec();

  deleteLater();
}
