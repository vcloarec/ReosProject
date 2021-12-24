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
#include<QFutureWatcher>
#include <QtConcurrent>

ReosProcess::~ReosProcess() {}

int ReosProcess::maxProgression() const
{
  QMutexLocker locker( &mMutexProgression );
  if ( mCurrentSubProcess )
    return mCurrentSubProcess->maxProgression();
  else
    return mMaxProgression;
}

void ReosProcess::setMaxProgression( int value )
{
  QMutexLocker locker( &mMutexProgression );
  mMaxProgression = value;
}

int ReosProcess::currentProgression() const
{
  QMutexLocker locker( &mMutexProgression );
  if ( mCurrentSubProcess )
    return mCurrentSubProcess->currentProgression();
  else
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
  mCurrentProgression = value;
}

unsigned ReosProcess::maximumThreads()
{
  unsigned maxThread = std::max( std::thread::hardware_concurrency() - 1, 1u );
  if ( MAX_THREAD > 0u )
    maxThread = std::min( MAX_THREAD, maxThread );

  return maxThread;
}

void ReosProcess::notify( ReosModule::Message &message )
{
  mMessage = message;
}

void ReosProcess::startOnOtherThread()
{
  QFutureWatcher<void> *watcher = new QFutureWatcher<void>( this );
  connect( watcher, &QFutureWatcher<void>::finished, this, &ReosProcess::finish );
  connect( watcher, &QFutureWatcher<void>::finished, watcher, &QObject::deleteLater );
  QFuture<void> future = QtConcurrent::run( this, &ReosProcess::start );//https://doc.qt.io/qt-5/qtconcurrentrun.html#using-member-functions
  watcher->setFuture( future );
}

bool ReosProcess::finish()
{
  if ( !mIsFinished )
  {
    emit finished( QPrivateSignal() );
  }
  mIsFinished = true;
  return mIsSuccessful;
}

ReosModule::Message ReosProcess::message() const
{
  return mMessage;
}

void ReosProcess::setInformation( const QString &info )
{
  QMutexLocker locker( &mMutexInformation );
  mCurrentInformation = info;
  emit sendInformation( info );
}

void ReosProcess::stop( bool b )
{
  mStop = b;
  if ( mCurrentSubProcess )
    mCurrentSubProcess->stop( b );
}

bool ReosProcess::isStop() const
{
  return mStop;
}

void ReosProcess::setParentProcess( ReosProcess *parent )
{
  mParentProcess = parent;
}

bool ReosProcess::isSuccessful() const
{
  return mIsSuccessful;
}

void ReosProcess::setSuccesful( bool b )
{
  mIsSuccessful = b;
}

bool ReosProcess::isFinished() const
{
  return mIsFinished;
}

void ReosProcess::processStart( ReosProcess *p )
{
  p->start();
}

void ReosProcess::setSubProcess( ReosProcess *subProcess )
{
  QMutexLocker locker( &mMutexProgression );
  mCurrentSubProcess = subProcess;
  if ( subProcess )
    subProcess->setParentProcess( this );
}
