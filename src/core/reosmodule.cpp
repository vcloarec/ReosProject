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

ReosModule::ReosModule( QObject *parent ):
  QObject( parent ),  mGroupAction( new QActionGroup( this ) )
{
  mReosParent = qobject_cast<ReosModule *>( parent );
}

ReosModule::~ReosModule()
{
  if ( mReosParent )
    mReosParent->mReosChildren.removeOne( this );
}

void ReosModule::newCommand( QUndoCommand *command )
{
  if ( mUndoStack )
  {
    mUndoStack->push( command );
    return;
  }

  if ( mReosParent )
  {
    mReosParent->newCommand( command );
  }
  else
  {
    emit newCommandToUndoStack( command );
  }
}

void ReosModule::warning( QString message ) const
{
  sendMessage( message, Warning );
}

void ReosModule::error( QString message ) const
{
  sendMessage( message, Error );
}

void ReosModule::message( QString message ) const
{
  sendMessage( message, Message );
}

void ReosModule::order( QString message ) const
{
  sendMessage( message, Order );
}

void ReosModule::onMessageReceived( const QString &message, const ReosModule::MessageType &type )
{
  sendMessage( message, type );
}

void ReosModule::sendMessage( QString mes, MessageType type ) const
{
  if ( mReosParent )
    mReosParent->sendMessage( mes, type );
  else
    emit emitMessage( mes, type );
}

void ReosModule::startProcessOnOtherThread( ReosProcess *process )
{
  QFutureWatcher<void> *watcher = new QFutureWatcher<void>( this );
  connect( watcher, &QFutureWatcher<void>::finished, this, &ReosModule::processFinished );
  connect( watcher, &QFutureWatcher<void>::finished, watcher, &QObject::deleteLater );
  QFuture<void> future = QtConcurrent::run( process, &ReosProcess::start );
  watcher->setFuture( future );
}

void ReosModule::startProcessOnSameThread( ReosProcess *process )
{
  process->start();
  emit processFinished();
}

QList<QAction *> ReosModule::actions() const {return mGroupAction->actions();}

void ReosModule::redo()
{
  if ( mUndoStack )
    mUndoStack->redo();
}

void ReosModule::undo()
{
  if ( mUndoStack )
    mUndoStack->undo();
}

