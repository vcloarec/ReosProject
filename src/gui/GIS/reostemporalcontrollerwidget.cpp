/***************************************************************************
  reostemporalcontrollerwidget.cpp - ReosTemporalControllerWidget

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
#include "reostemporalcontrollerwidget.h"
#include "ui_reostemporalcontrollerwidget.h"

#include <QWidgetAction>
#include <QMenu>

#include "reostemporalcontroller_p.h"
#include "reosspeedwidget.h"
#include "reossettings.h"


ReosTemporalControllerWidget::ReosTemporalControllerWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosTemporalControllerWidget )
{
  ui->setupUi( this );

  ReosSettings settings;

  mTemporalController = new ReosTemporalController_p( this );


  QMenu *speedMenu = new QMenu( this );
  QWidgetAction *widgetAction = new QWidgetAction( speedMenu );
  ReosSpeedWidget *speedWidget = new ReosSpeedWidget( this );
  speedWidget->setSpeedFactor( settings.value( QStringLiteral( "/temporal_controler/speed_factor" ), 1 ).toDouble() );
  widgetAction->setDefaultWidget( speedWidget );
  speedMenu->addAction( widgetAction );
  connect( speedWidget, &ReosSpeedWidget::speedFactorChanged, this, &ReosTemporalControllerWidget::speedFactorChanged );
  ui->mSpeedButton->setMenu( speedMenu );

  speedWidget->setSpeedFactor( mTemporalController->speedFactor() );

  connect( mTemporalController, &ReosTemporalController_p::updateTemporalRange, this, [this]( const QgsDateTimeRange & range )
  {
    setCurrentTime( range.begin() );
  } );

  connect( mTemporalController, &ReosTemporalController_p::stopped, this, [this]
  {
    ui->mPauseButton->setChecked( true );
    activatePause();
  } );

  connect( ui->mPlayForwardButton, &QToolButton::clicked, this, [this]
  {
    if ( ui->mPlayForwardButton->isChecked() )
      activatePlay();
  } );

  connect( ui->mPlayBackButton, &QToolButton::clicked, this, [this]
  {
    if ( ui->mPlayBackButton->isChecked() )
      activatePlayBack();
  } );

  connect( ui->mPauseButton, &QToolButton::clicked, this, [this]
  {
    if ( ui->mPauseButton->isChecked() )
      activatePause();
  } );

  connect( ui->mLoopButton, &QToolButton::toggled, mTemporalController, &ReosTemporalController_p::setIsLoop );

  connect( ui->mNextToolButton, &QToolButton::clicked, mTemporalController, &ReosTemporalController_p::nextStep );
  connect( ui->mPreviousToolButton, &QToolButton::clicked, mTemporalController, &ReosTemporalController_p::prevStep );
  connect( ui->mCurrentTime, &QDateTimeEdit::dateTimeChanged, mTemporalController, &ReosTemporalController_p::setCurrentTime );
  connect( ui->mSlider, &QSlider::sliderMoved, this, &ReosTemporalControllerWidget::onSliderMoved );
}

ReosTemporalControllerWidget::~ReosTemporalControllerWidget()
{
  delete ui;
}

QObject *ReosTemporalControllerWidget::temporalController()
{
  return mTemporalController;
}

void ReosTemporalControllerWidget::speedFactorChanged( double speedFactor )
{
  mTemporalController->setSpeedFactor( speedFactor );
}

void ReosTemporalControllerWidget::onSliderMoved( int value )
{
  const QDateTime time = mTemporalController->startTime().addSecs( value );
  mTemporalController->setCurrentTime( time );
}

void ReosTemporalControllerWidget::activatePlay()
{
  mTemporalController->play();
  ui->mPlayBackButton->setChecked( false );
  ui->mPauseButton->setChecked( false );
}

void ReosTemporalControllerWidget::activatePlayBack()
{
  mTemporalController->playBack();
  ui->mPlayForwardButton->setChecked( false );
  ui->mPauseButton->setChecked( false );
}

void ReosTemporalControllerWidget::activatePause()
{
  mTemporalController->pause();
  ui->mPlayForwardButton->setChecked( false );
  ui->mPlayBackButton->setChecked( false );
}

void ReosTemporalControllerWidget::setCurrentTime( const QDateTime &time )
{
  ui->mCurrentTime->blockSignals( true );
  ui->mCurrentTime->setDateTime( time );
  ui->mCurrentTime->blockSignals( false );

  int max = int( mTemporalController->durationExtent().valueSecond() );
  int current = static_cast<int>( mTemporalController->startTime().secsTo( time ) );
  ui->mSlider->blockSignals( true );
  ui->mSlider->setRange( 0, max );
  ui->mSlider->setValue( current );
  ui->mSlider->blockSignals( false );
}
