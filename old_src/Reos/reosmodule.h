/***************************************************************************
                      reosmodule.h
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

#ifndef REOSMODULE_H
#define REOSMODULE_H

#include <QObject>
#include <QUndoCommand>
#include <QActionGroup>
#include <QDebug>
#include <QTime>
#include <QToolBar>

#include "reosmessagebox.h"

class ReosInformationSender
{
  public:
    ReosInformationSender( ReosInformationSender *parent );
    virtual ~ReosInformationSender();

  protected:
    virtual void receive( int mes );
    void sendToParent( int mes );
  private:
    ReosInformationSender *parent;
};

class ReosModule : public QObject, public ReosInformationSender
{
    Q_OBJECT
  public:
    explicit ReosModule( QObject *parent = nullptr, ReosInformationSender *messenger = nullptr );
    explicit ReosModule( ReosModule *parent );
    virtual ~ReosModule();

    virtual QWidget *getWidget() const {return nullptr;}
    QList<QAction *> getActions() const;
    QMenu *getMenu() const {return mMenu;}
    QToolBar *getToolBar() const {return mToolBar;}

  signals:
    void newCommandToUndoStack( QUndoCommand *command );
    void activeUndoStack( QUndoStack *undoStack );
    void widgetVisibility( bool );
    void messageEmited( QString &message, ReosMessageBox::Type type ) const;

  public slots:
    virtual void showWidget();
    void hideWidget();

    void undo()
    {
      if ( mUndoStack )
        mUndoStack->undo();
    }

    void redo()
    {
      if ( mUndoStack )
        mUndoStack->redo();
    }

    ////////////////////////////////////////////
    /// \brief newCommand
    /// \param command
    /// Handle QUndoCommand, by default : if undoStack is present (mUndoStack !=nullptr, push the command to the undoStack.
    /// If not send to the parent the command if the parent is not null, if it is null, emit newCommandToUndoStack.
    ///
    virtual void newCommand( QUndoCommand *command );

    void warning( QString message ) const;
    void error( QString message ) const;
    void message( QString message ) const;
    void order( QString message ) const;

  protected:
    QActionGroup *mGroupAction = nullptr;
    QToolBar *mToolBar = nullptr;
    QMenu *mMenu = nullptr;
    QUndoStack *mUndoStack = nullptr;

    void sendMessage( QString mes, ReosMessageBox::Type type ) const;

  private:
    ReosModule *mReosParent = nullptr;
    QList<ReosModule *> mReosChildren;
};

#endif // REOSMODULE_H
