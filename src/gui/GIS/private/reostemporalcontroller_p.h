/***************************************************************************
  reostemporalcontroller_p.h - ReosTemporalController_p

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
#ifndef REOSTEMPORALCONTROLLER_P_H
#define REOSTEMPORALCONTROLLER_P_H

#include <qgstemporalcontroller.h>

#include "reosduration.h"

class QTimer;

class ReosTemporalController_p : public QgsTemporalController
{
    Q_OBJECT
  public:
    //! Represents the current animation state.
    enum AnimationState
    {
      Forward, //!< Animation is playing forward.
      Reverse, //!< Animation is playing in reverse.
      Idle, //!< Animation is paused.
    };

    ReosTemporalController_p( QObject *parent = nullptr );

    double speedFactor() const;
    void setSpeedFactor( double speedFactor );

    void setTimeStep( const ReosDuration &timeStep );

    int timeStepCount() const;
    ReosDuration durationExtent() const;

    QDateTime startTime() const;

  public slots:
    void setTemporalExtent( const QDateTime &startTime, const QDateTime &endTime );
    void setCurrentTime( const QDateTime &time );
    void setIsLoop( bool isLoop );

    void play();
    void pause();
    void playBack();
    void nextStep();
    void prevStep();

  signals:
    void stopped() const;

  private slots:
    void timerTimeout();

  private:
    QDateTime mCurrentTime;
    QDateTime mStartTime;
    QDateTime mEndTime;
    bool mIsLoop = false;
    QTimer *mTimer = nullptr;
    ReosDuration mTimeStep;
    double mSpeedFactor = 60;
    AnimationState mAnimationState = Idle;


    void updateTimer();
};

#endif // REOSTEMPORALCONTROLLER_P_H
