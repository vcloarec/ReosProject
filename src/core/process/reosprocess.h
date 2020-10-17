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
#include <mutex>
#include <QString>
#include <QObject>


/**
 * Abstract class that represent a process (log calculation). this class has convenient method to handle, feedback and bring the process in other thread
 * Processes can be nested.
*/
class ReosProcess
{
  public:
    virtual ~ReosProcess();

    int maxProgession() const;
    void setMaxProgession( int value );

    int currentProgression();

    virtual void stopAsSoonAsPossible( bool b );

    bool isSuccessful() const;

    static void processStart( ReosProcess *p );

    virtual void start() = 0;

  protected:

    void setCurrentProgression( int value );
    bool isStopped() const {return mStopWithoutMutex;}
    bool isStopAsked();
    void stop( bool b ) {mStopWithoutMutex = b;}
    bool mIsSuccessful = false;

  private:
    int mMaxProgession;
    int mCurrentProgression;
    bool mStopWithoutMutex = false;
    bool mStopWithMutex = false;

    std::mutex mMutexProgression;
    std::mutex mMutexStop;
};

#endif // REOSPROCESS_H
