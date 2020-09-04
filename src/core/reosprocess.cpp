/***************************************************************************
                      reosprocess.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosprocess.h"

ReosProcess::~ReosProcess() {}

int ReosProcess::maxProgession() const
{
  return mMaxProgession;
}

void ReosProcess::setMaxProgession( int value )
{
  mMaxProgession = value;
}



int ReosProcess::currentProgression()
{
  int progress;
  mMutexProgression.lock();
  progress = mCurrentProgression;
  mMutexProgression.unlock();
  return progress;
}



void ReosProcess::setCurrentProgression( int value )
{
  mMutexProgression.lock();
  mCurrentProgression = value;
  mMutexProgression.unlock();
}

void ReosProcess::stopAsSoonAsPossible( bool b )
{
  mMutexStop.lock();
  mStopWithMutex = b;
  mMutexStop.unlock();
}

bool ReosProcess::isStopAsked()
{
  bool stp;
  mMutexStop.lock();
  stp = mStopWithMutex;
  mMutexStop.unlock();
  return stp;
}

bool ReosProcess::isSuccessful() const
{
  return mIsSuccessful;
}

static void processStart( ReosProcess *p )
{
  p->start();
}
