/***************************************************************************
                      reosmodule.h
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

#ifndef REOSMODULE_H
#define REOSMODULE_H

#include <QObject>
#include <QUndoCommand>
#include <QActionGroup>
#include <QTime>
#include <QToolBar>
#include <QFileInfoList>

#include "reoscore.h"

class ReosProcess;

class REOSCORE_EXPORT ReosModule : public QObject
{
    Q_OBJECT
  public:
    enum MessageType
    {
      Simple,
      Order,
      Warning,
      Error
    };

    struct REOSCORE_EXPORT Message
    {
      void prefixMessage( const QString &prefix );
      void addText( const QString &newText );

      MessageType type = Simple;
      QString text = QString();
    };

    explicit ReosModule( QObject *parent = nullptr );
    virtual ~ReosModule();

    QList<QAction *> actions() const;

    const QString projectFileName();

    void setProjectFileName( const QString &projectFileName );

    virtual QFileInfoList uselessFiles( bool clean ) const;

  signals:
    void newCommandToUndoStack( QUndoCommand *command );
    void activeUndoStack( QUndoStack *undoStack );
    void emitMessage( const Message &message, bool messageBox ) const;
    void dirtied();

  public slots:

    void undo();
    void redo();

    ////////////////////////////////////////////
    /// \brief newCommand
    /// \param command
    /// Handle QUndoCommand, by default : if undoStack is present (mUndoStack !=nullptr, push the command to the undoStack.
    /// If not send to the parent the command if the parent is not null, if it is null, emit newCommandToUndoStack.
    ///
    virtual void newCommand( QUndoCommand *command );

    void warning( QString message, bool inMessageBox = false ) const;
    void error( QString message, bool inMessageBox = false ) const;
    void message( QString message, bool inMessageBox = false ) const;
    void order( QString message, bool inMessageBox = false ) const;

    void message( const Message &messageObject, bool inMessageBox = false );

  private slots:
    void onMessageReceived( const QString &message, const MessageType &type, bool inMessageBox = false );

  protected:
    QActionGroup *mGroupAction = nullptr;
    QUndoStack *mUndoStack = nullptr;

    void sendMessage( QString mes, MessageType type, bool messageBox = false ) const;

  private:
    ReosModule *mReosParent = nullptr;
    QList<ReosModule *> mReosChildren;
    QString mProjectFileName;
};

#endif // REOSMODULE_H
