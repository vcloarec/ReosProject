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


ReosVersionMessageBox::ReosVersionMessageBox( QWidget *parent, const ReosVersion &version, bool start ):
  QMessageBox( QMessageBox::Information, tr( "New version available" ), "", QMessageBox::Ok, parent ),
  mVersion( version ),
  mStart( start )
{
  setTextFormat( Qt::RichText );

  mNetWorkAccess = new QNetworkAccessManager( this );

  connect( mNetWorkAccess, &QNetworkAccessManager::finished, this, &ReosVersionMessageBox::receiveNetWorkRequest );
  QString address = serverVersionAddress;
  address.append( version.getSoftName() ).append( ".txt" );
  mNetWorkAccess->get( QNetworkRequest( address ) );
}

QString ReosVersionMessageBox::getDefaultWebSite() const
{
  return mDefaultWebSite;
}

void ReosVersionMessageBox::setDefaultWebSite( const QString &value )
{
  mDefaultWebSite = value;
}

void ReosVersionMessageBox::receiveNetWorkRequest( QNetworkReply *reply )
{
  QTextStream textStream( reply );
  QString text = textStream.readAll();
  reply->deleteLater();
  qDebug() << text;

  bool newVersion = false;
  if ( text.count() == 0 )
  {
    deleteLater();
    return;
  }

  QStringList textSplit = text.split( "|" );
  QString versionText = textSplit.at( 0 );

  QStringList versionSplit = versionText.split( "." );

  if ( versionSplit.count() > 2 )
  {
    ReosVersion versionAvailable( mVersion.getSoftName(), versionSplit.at( 0 ).toInt(), versionSplit.at( 1 ).toInt(), versionSplit.at( 2 ).toInt() );
    newVersion = versionAvailable > mVersion;
    if ( !newVersion && mStart )
    {
      deleteLater();
      return;
    }

  }


  if ( newVersion )
  {
    QString message;
    message = tr( "A new version is available.\n" );

    message.append( tr( "Visit : " ) );
    message.append( "<a href=\"http://%1\"> %1</a>" );
    if ( textSplit.count() > 1 )
    {
      QString webSite = textSplit.at( 1 );
      message = message.arg( webSite );
    }
    else
    {
      message = message.arg( mDefaultWebSite );
    }
    setText( message );

  }
  else
  {
    setText( tr( "No new version is available." ) );
  }

  exec();
  deleteLater();
}
