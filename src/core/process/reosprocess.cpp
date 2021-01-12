/***************************************************************************
                      reosprocess.cpp
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

#include "reosprocess.h"
#include <QMutexLocker>

ReosProcess::~ReosProcess() {}

int ReosProcess::maxProgression() const
{
  QMutexLocker locker( &mMutexProgression );
  return mMaxProgression;
}

void ReosProcess::setMaxProgression( int value )
{
  QMutexLocker locker( &mMutexProgression );
  if ( mParentProcess )
    mParentProcess->setMaxProgression( value );
  mMaxProgression = value;
}

int ReosProcess::currentProgression() const
{
  QMutexLocker locker( &mMutexProgression );
  return mCurrentProgression;
}

QString ReosProcess::currentInformation() const
{
  QMutexLocker locker( &mMutexInformation );
  return mCurrentInformation;
}

void ReosProcess::setCurrentProgression( int value )
{
  QMutexLocker locker( &mMutexProgression );
  if ( mParentProcess )
    mParentProcess->setCurrentProgression( value );
  mCurrentProgression = value;
}

bool ReosProcess::finish()
{
  emit finished();
  return isSuccessful();
}

void ReosProcess::setInformation( const QString &info )
{
  QMutexLocker locker( &mMutexInformation );
  mCurrentInformation = info;
  emit sendInformation( info );
}

void ReosProcess::stopAsSoonAsPossible( bool b )
{
  QMutexLocker locker( &mMutexStop );
  mStopWithMutex = b;
  if ( mCurrentSubProcess )
    mCurrentSubProcess->stopAsSoonAsPossible( b );
}

bool ReosProcess::isStopAsked()
{
  QMutexLocker locker( &mMutexStop );
  return mStopWithMutex;
}

void ReosProcess::setParentProcess( ReosProcess *parent )
{
  mParentProcess = parent;
}

bool ReosProcess::isSuccessful() const
{
  return mIsSuccessful;
}

void ReosProcess::processStart( ReosProcess *p )
{
  p->start();
}

void ReosProcess::setSubProcess( ReosProcess *subProcess )
{
  mCurrentSubProcess = subProcess;
  subProcess->setParentProcess( this );
}
