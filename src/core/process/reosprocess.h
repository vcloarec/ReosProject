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

/**
 * Abstract class that represent a process (long calculation). this class has convenient method to handle feedback and bring the process in other thread
 * Processes can be nested.
*/
class REOSCORE_EXPORT ReosProcess : public QObject
{
    Q_OBJECT
  public:
    virtual ~ReosProcess();

    //! Returns the max progression value
    int maxProgression() const;

    //! Return the current progression on the process
    int currentProgression() const;

    QString currentInformation() const;

    virtual void stop( bool b );
    bool isStop();

    bool isSuccessful() const;
    void setSuccesful( bool b );

    bool isFinished() const;

    static void processStart( ReosProcess *p );
    virtual void start() = 0;

    //! Sets a the current sub process
    void setSubProcess( ReosProcess *subProcess );

    //! Sets the maximum progression value
    void setMaxProgression( int value );
    void setCurrentProgression( int value );
    void setInformation( const QString &info );

    void startOnOtherThread();

  signals:
    void sendInformation( const QString & );

  protected slots:
    bool finish();

  protected:
    bool mIsSuccessful = false;

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
