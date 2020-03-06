/***************************************************************************
                      reosmodule.cpp
                     --------------------------------------
Date                 : 18-11-2018
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

#include "reosmodule.h"

ReosModule::ReosModule( QObject *parent, ReosInformationSender *messenger ):
  QObject( parent ), ReosInformationSender( messenger ), mGroupAction( new QActionGroup( this ) )
{

}

ReosModule::ReosModule( ReosModule *parent ): ReosModule( static_cast<QObject *>( parent ) )
{

  mReosParent = parent;
  mReosChildren.append( this );
}

ReosModule::~ReosModule()
{
  if ( getWidget() )
  {
    if ( ( !getWidget()->parentWidget() ) && ( !getWidget()->parent() ) )
      delete getWidget();
  }
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
  sendMessage( message, ReosMessageBox::warning );
}

void ReosModule::error( QString message ) const
{
  sendMessage( message, ReosMessageBox::error );
}

void ReosModule::message( QString message ) const
{
  sendMessage( message, ReosMessageBox::message );
}

void ReosModule::order( QString message ) const
{
  sendMessage( message, ReosMessageBox::order );
}

void ReosModule::sendMessage( QString mes, ReosMessageBox::Type type ) const
{
  if ( mReosParent )
    mReosParent->sendMessage( mes, type );
  else
    emit messageEmited( mes, type );
}

QList<QAction *> ReosModule::getActions() const {return mGroupAction->actions();}

void ReosModule::showWidget()
{
  if ( getWidget() )
    getWidget()->show();
}

void ReosModule::hideWidget()
{
  if ( getWidget() )
  {
    getWidget()->hide();
  }
  emit widgetVisibility( false );
}

ReosInformationSender::ReosInformationSender( ReosInformationSender *parent ): parent( parent ) {}

ReosInformationSender::~ReosInformationSender() {}

void ReosInformationSender::receive( int mes )
{
  sendToParent( mes );
}

void ReosInformationSender::sendToParent( int mes )
{
  if ( parent )
    parent->receive( mes );
}
