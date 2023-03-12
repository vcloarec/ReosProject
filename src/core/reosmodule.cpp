/***************************************************************************
                      reosmodule.cpp
                     --------------------------------------
Date                 : 18-11-2018
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

#include <QtConcurrent>
#include <QFuture>

#include "reosmodule.h"
#include "reosprocess.h"

ReosModule::ReosModule( const QString &moduleName, QObject *parent )
  : QObject( parent )
  , mReosParent( qobject_cast<ReosModule *>( parent ) )
{
  if ( mReosParent )
  {
    if ( mReosParent->mReosChildren.contains( moduleName ) )
    {
      qDebug() << QStringLiteral( "This module already contains a child module with name %1" ).arg( moduleName );
      return;
    }
    mReosParent->mReosChildren.insert( moduleName, this );
    connect( this, &ReosModule::dirtied, mReosParent, &ReosModule::dirtied );
  }
}

ReosModule::~ReosModule()
{
  if ( mReosParent )
    mReosParent->mReosChildren.remove( this->moduleName() );
}

void ReosModule::warning( QString message, bool inMessageBox ) const
{
  sendMessage( message, Warning, inMessageBox );
}

void ReosModule::error( QString message, bool inMessageBox ) const
{
  sendMessage( message, Error, inMessageBox );
}

void ReosModule::message( QString message, bool inMessageBox ) const
{
  sendMessage( message, Simple, inMessageBox );
}

void ReosModule::order( QString message, bool inMessageBox ) const
{
  sendMessage( message, Order, inMessageBox );
}

void ReosModule::message( const ReosModule::Message &messageObject, bool inMessageBox )
{
  switch ( messageObject.type )
  {
    case ReosModule::Simple:
      message( messageObject.text, inMessageBox );
      break;
    case ReosModule::Order:
      order( messageObject.text, inMessageBox );
      break;
    case ReosModule::Warning:
      warning( messageObject.text, inMessageBox );
      break;
    case ReosModule::Error:
      error( messageObject.text, inMessageBox );
      break;
  }
}

void ReosModule::onMessageReceived( const QString &message, const ReosModule::MessageType &type, bool inMessageBox )
{
  sendMessage( message, type, inMessageBox );
}

void ReosModule::sendMessage( QString mes, MessageType type, bool messageBox ) const
{
  if ( mReosParent )
    mReosParent->sendMessage( mes, type, messageBox );
  else
  {
    emit emitMessage( {type, mes}, messageBox );
    switch ( type )
    {
      case Simple:
      case Order:
        break;
      case Warning:
      case Error:
        qDebug() << "/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/  " << mes;
        break;
    }
  }
}

void ReosModule::setProjectFileName( const QString &projectFileName )
{
  mProjectFileName = projectFileName;
}

QFileInfoList ReosModule::uselessFiles( bool clean ) const
{
  QFileInfoList ret;
  for ( ReosModule *mod : mReosChildren )
    ret.append( mod->uselessFiles( clean ) );
  return ret;
}

QList<QAction *> ReosModule::actions() const {return mGroupAction->actions();}

ReosModule *ReosModule::childModule( const QString &moduleName ) const
{
  return mReosChildren.value( moduleName, nullptr );
}

QStringList ReosModule::childModuleNames() const
{
  return mReosChildren.keys();
}

const QString ReosModule::projectFileName()
{
  if ( mReosParent )
    return mReosParent->projectFileName();

  return mProjectFileName;
}

void ReosModule::Message::prefixMessage( const QString &prefix )
{
  text.prepend( prefix );
}

void ReosModule::Message::addText( const QString &newText )
{
  if ( !text.isEmpty() )
  {
    text.append( "\n" );
    text.append( newText );
  }
  else
    text = newText;
}
