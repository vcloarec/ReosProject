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
#include <QPointer>

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

    ReosModule() = default;
    explicit ReosModule( const QString &moduleName, QObject *parent = nullptr );
    virtual ~ReosModule();

#ifndef SIP_RUN

    const QString projectFileName();
    void setProjectFileName( const QString &projectFileName );

    virtual QFileInfoList uselessFiles( bool clean ) const;

    QString moduleName() const {return mModuleName;}

    ReosModule *childModule( const QString &moduleName ) const;
    QStringList childModuleNames() const;

    /**
     * Returns a pointer to the child module of type T, nullptr is this module does not exist
     */
    template <typename T>
    T module() const
    {
      return qobject_cast<T>( childModule( std::remove_pointer<T>::type::staticName() ) );
    }

#endif //SIP_RUN
  signals:
    void emitMessage( const Message &message, bool messageBox ) const;

#ifndef SIP_RUN
    void dirtied();

  public slots:
    void warning( QString message, bool inMessageBox = false ) const;
    void error( QString message, bool inMessageBox = false ) const;
    void message( QString message, bool inMessageBox = false ) const;
    void order( QString message, bool inMessageBox = false ) const;

    void message( const Message &messageObject, bool inMessageBox = false );

  private slots:
    void onMessageReceived( const QString &message, const MessageType &type, bool inMessageBox = false );

  protected:
    void sendMessage( QString mes, MessageType type, bool messageBox = false ) const;

#endif //#ifndef SIP_RUN

  private:
    QPointer<ReosModule> mReosParent;
    QMap < QString, QPointer<ReosModule>> mReosChildren;
    QString mProjectFileName;
    QString mModuleName;
};


#endif // REOSMODULE_H
