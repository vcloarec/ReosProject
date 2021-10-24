/***************************************************************************
                      reosprocess.h
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

#ifndef REOSPROCESS_H
#define REOSPROCESS_H

#include <memory>
#include <QString>
#include <QObject>
#include <QMutex>

#include "reoscore.h"

#define MAX_THREAD 0u

/**
 * Abstract class that represent a process (long calculation). this class has convenient method to handle feedback and bring the process in other thread
 * Processes can be nested.
*/
class REOSCORE_EXPORT ReosProcess : public QObject
{
    Q_OBJECT
  public:
    virtual ~ReosProcess();

    virtual void stop( bool b );
    bool isStop() const;
    bool isSuccessful() const;
    bool isFinished() const;

    virtual void start() = 0;
    //! Return the current progression on the process
    virtual int currentProgression() const;

    //! Returns the max progression value
    virtual int maxProgression() const;

    //! Sets the maximum progression value
    virtual void setMaxProgression( int value );

    QString currentInformation() const;
    void setInformation( const QString &info );

    //! Start the process on another thread
    void startOnOtherThread();

    //! Static method used to start a process (used in std::thread API)
    static void processStart( ReosProcess *p );

    void setSuccesful( bool b );
    void setCurrentProgression( int value );

    static unsigned maximumThreads();

  signals:
    void sendInformation( const QString & );

  protected slots:
    bool finish();

  protected:
    bool mIsSuccessful = false;

    //! Sets a the current sub process, do not take ownership and caller must set nullptr before deleting the subprocess
    void setSubProcess( ReosProcess *subProcess );

  private:
    int mMaxProgression;
    int mCurrentProgression;
    bool mStop = false;
    QString mCurrentInformation;
    bool mIsFinished = false;

    mutable QMutex mMutexProgression;
    mutable QMutex mMutexInformation;

    ReosProcess *mParentProcess = nullptr;
    ReosProcess *mCurrentSubProcess = nullptr;

    void setParentProcess( ReosProcess *parent );

  signals:
    void finished( QPrivateSignal );
};

#endif // REOSPROCESS_H
