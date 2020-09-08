/***************************************************************************
                      reosdocumentation.cpp
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

#include "reosdocumentation.h"


ReosDocumentation::ReosDocumentation( const ReosVersion &version, QObject *parent ): QObject( parent ),
  mVersion( version )
{

}

void ReosDocumentation::call()
{
  mNetWorkAccess = new QNetworkAccessManager( this );

  connect( mNetWorkAccess, &QNetworkAccessManager::finished, this, &ReosDocumentation::launchWebSite );
  QString address = serverDocumentationAddress;
  address.append( mVersion.getSoftName() ).append( ".txt" );
  mNetWorkAccess->get( QNetworkRequest( address ) );
}

void ReosDocumentation::launchWebSite( QNetworkReply *reply )
{
  QTextStream textStream( reply );
  QString text = textStream.readAll();


  QDesktopServices::openUrl( QUrl( text ) );
}
