/***************************************************************************
  reostemporalcontroller_p.cpp - ReosTemporalController_p

 ---------------------
 begin                : 18.6.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostemporalcontroller_p.h"

#include <QTimer>

ReosTemporalController_p::ReosTemporalController_p( QObject *parent )
  : QgsTemporalController( parent )
{
  mTimer = new QTimer( this );
  connect( mTimer, &QTimer::timeout, this, &ReosTemporalController_p::timerTimeout );

  mCurrentTime.setDate( QDate::currentDate() );
  mCurrentTime.setTime( QTime( 0, 0, 0 ) );

  mTimeStep = ReosDuration( 1, ReosDuration::minute );
}

void ReosTemporalController_p::play()
{
  mAnimationState = Forward;
  updateTimer();
}

void ReosTemporalController_p::pause()
{
  mAnimationState = Idle;
  mTimer->stop();
}

void ReosTemporalController_p::playBack()
{
  mAnimationState = Reverse;
  updateTimer();
}

void ReosTemporalController_p::nextStep()
{
  if ( mCurrentTime >= mEndTime )
  {
    if ( mIsLoop )
      mCurrentTime = mStartTime;
    else
    {
      pause();
      mCurrentTime = mEndTime;
      emit stopped();
    }
  }
  else
    mCurrentTime = mCurrentTime.addMSecs( ( mTimeStep * mTimeStepFactor ).valueMilliSecond() );

  const QgsDateTimeRange timerange( mCurrentTime, mCurrentTime.addMSecs( ( mTimeStep * mTimeStepFactor ).valueMilliSecond() ) );
  emit updateTemporalRange( timerange );
}

void ReosTemporalController_p::prevStep()
{
  if ( mCurrentTime <= mStartTime )
  {
    if ( mIsLoop )
      mCurrentTime = mEndTime;
    else
    {
      pause();
      mCurrentTime = mStartTime;
      emit stopped();
    }
  }
  else
    mCurrentTime = mCurrentTime.addMSecs( -( mTimeStep * mTimeStepFactor ).valueMilliSecond() );

  const QgsDateTimeRange timerange( mCurrentTime, mCurrentTime.addMSecs( ( mTimeStep * mTimeStepFactor ).valueMilliSecond() ) );
  emit updateTemporalRange( timerange );
}

void ReosTemporalController_p::timerTimeout()
{
  switch ( mAnimationState )
  {
    case ReosTemporalController_p::Forward:
      nextStep();
      break;
    case ReosTemporalController_p::Reverse:
      prevStep();
      break;
    case ReosTemporalController_p::Idle:
      break;
  }
}

ReosDuration ReosTemporalController_p::timeStep() const
{
  return mTimeStep;
}

QDateTime ReosTemporalController_p::startTime() const
{
  return mStartTime;
}

void ReosTemporalController_p::updateTimer()
{
  ReosDuration timerStep = mTimeStep / mSpeedFactor;

  if ( timerStep.valueMilliSecond() > 0 && mTimeStep.valueMilliSecond() > 0 && timerStep.valueMilliSecond() < 200 )
  {
    mTimeStepFactor = 200 / timerStep.valueMilliSecond();
    timerStep = ReosDuration( timerStep.valueMilliSecond() / mTimeStepFactor, ReosDuration::millisecond );
  }
  else
    mTimeStepFactor = 1;

  switch ( mAnimationState )
  {
    case ReosTemporalController_p::Forward:
    case ReosTemporalController_p::Reverse:
      mTimer->stop();
      mTimer->start( timerStep.valueMilliSecond() );
      break;
    case ReosTemporalController_p::Idle:
      break;
  }
}

void ReosTemporalController_p::setTimeStep( const ReosDuration &timeStep )
{
  mTimeStep = timeStep;
  updateTimer();
}

int ReosTemporalController_p::timeStepCount() const
{
  return durationExtent().numberOfFullyContainedIntervals( mTimeStep );
}

ReosDuration ReosTemporalController_p::durationExtent() const
{
  return ReosDuration( mStartTime.msecsTo( mEndTime ) );
}

void ReosTemporalController_p::setIsLoop( bool isLoop )
{
  mIsLoop = isLoop;
}

double ReosTemporalController_p::speedFactor() const
{
  return mSpeedFactor;
}

void ReosTemporalController_p::setSpeedFactor( double speedFactor )
{
  mSpeedFactor = speedFactor;
  updateTimer();
}

void ReosTemporalController_p::setTemporalExtent( const QDateTime &startTime, const QDateTime &endTime )
{
  mCurrentTime = startTime;
  mStartTime = startTime;
  mEndTime = endTime;
  const QgsDateTimeRange timerange( mCurrentTime, mCurrentTime.addMSecs( mTimeStep.valueMilliSecond() ) );
  emit updateTemporalRange( timerange );
}

void ReosTemporalController_p::setCurrentTime( const QDateTime &time )
{
  if ( time > mEndTime )
    mCurrentTime = mEndTime;
  else if ( time < mStartTime )
    mCurrentTime = mStartTime;
  else
    mCurrentTime = time;

  const QgsDateTimeRange timerange( mCurrentTime, mCurrentTime.addMSecs( mTimeStep.valueMilliSecond() ) );
  emit updateTemporalRange( timerange );
}
