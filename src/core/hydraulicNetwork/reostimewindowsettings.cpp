/***************************************************************************
  reostimewindowsettings.cpp - ReosTimeWindowSettings

 ---------------------
 begin                : 2.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostimewindowsettings.h"

ReosTimeWindowSettings::ReosTimeWindowSettings( QObject *parent )
  : ReosDataObject( parent )
  , mAutomaticallyDefined( new ReosParameterBoolean( tr( "Defined automatically" ) ) )
  , mStartOffset( new ReosParameterDuration( tr( "Start offset" ), this ) )
  , mEndOffset( new ReosParameterDuration( tr( "End offset" ), this ) )
  , mUserStartTime( new ReosParameterDateTime( tr( "User start time" ), this ) )
  , mUserEndTime( new ReosParameterDateTime( tr( "User end time" ), this ) )
{
  mAutomaticallyDefined->setValue( true );
  mStartOffset->setValue( ReosDuration( 0, ReosDuration::hour ) );
  mStartOffset->changeUnit( ReosDuration::hour );
  mEndOffset->setValue( ReosDuration( 0, ReosDuration::hour ) );
  mEndOffset->changeUnit( ReosDuration::hour );
  connect( mStartOffset, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mEndOffset, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUserStartTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUserEndTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosTimeWindow ReosTimeWindowSettings::timeWindow( const ReosTimeWindow &automaticTimeWindow ) const
{
  if ( mAutomaticallyDefined->value() )
  {
    QDateTime startOrigin;
    switch ( mOriginStart )
    {
      case Begin:
        startOrigin = automaticTimeWindow.start();
        break;
      case End:
        startOrigin = automaticTimeWindow.end();
        break;
    }
    QDateTime endOrigin;
    switch ( mOriginEnd )
    {
      case Begin:
        endOrigin = automaticTimeWindow.start();
        break;
      case End:
        endOrigin = automaticTimeWindow.end();
        break;
    }
    return ReosTimeWindow( startOrigin.addMSecs( mStartOffset->value().valueMilliSecond() )
                           , endOrigin.addMSecs( mEndOffset->value().valueMilliSecond() ) );
  }
  else
  {
    return ReosTimeWindow( mUserStartTime->value(), mUserEndTime->value() );
  }
}

ReosParameterDuration *ReosTimeWindowSettings::startOffset() const
{
  return mStartOffset;
}

ReosParameterDuration *ReosTimeWindowSettings::endOffset() const
{
  return mEndOffset;
}

ReosParameterDateTime *ReosTimeWindowSettings::userStartTime() const
{
  return mUserStartTime;
}

ReosParameterDateTime *ReosTimeWindowSettings::userEndTime() const
{
  return mUserEndTime;
}

ReosParameterBoolean *ReosTimeWindowSettings::automaticallyDefined() const
{
  return mAutomaticallyDefined;
}

ReosEncodedElement ReosTimeWindowSettings::encode() const
{
  ReosEncodedElement element( QStringLiteral( "time-window-settings" ) );
  element.addData( QStringLiteral( "automatically-defined" ), mAutomaticallyDefined->value() );
  element.addEncodedData( QStringLiteral( "start-offset" ), mStartOffset->value().encode() );
  element.addData( QStringLiteral( "start-offset-origin" ), mOriginStart );
  element.addEncodedData( QStringLiteral( "end-offset" ), mEndOffset->value().encode() );
  element.addData( QStringLiteral( "end-offset-origin" ), mOriginEnd );
  element.addData( QStringLiteral( "user-sart-time" ), mUserStartTime->value() );
  element.addData( QStringLiteral( "user-end-time" ), mUserEndTime->value() );
  return element;
}

void ReosTimeWindowSettings::decode( const ReosEncodedElement &element )
{
  blockSignals( true );
  bool autoDefined = true;
  element.getData( QStringLiteral( "automatically-defined" ), autoDefined );
  mAutomaticallyDefined->setValue( autoDefined );
  mStartOffset->setValue( ReosDuration::decode( element.getEncodedData( QStringLiteral( "start-offset" ) ) ) );
  mEndOffset->setValue( ReosDuration::decode( element.getEncodedData( QStringLiteral( "end-offset" ) ) ) );
  int so = 0;
  if ( element.getData( QStringLiteral( "start-offset-origin" ), so ) )
    mOriginStart = static_cast<OffsetOrigin>( so );
  int eo = 0;
  if ( element.getData( QStringLiteral( "end-offset-origin" ), eo ) )
    mOriginEnd = static_cast<OffsetOrigin>( eo );
  QDateTime ust;
  element.getData( QStringLiteral( "user-sart-time" ), ust );
  mUserStartTime->setValue( ust );
  QDateTime uet;
  element.getData( QStringLiteral( "user-end-time" ), uet );
  mUserEndTime->setValue( uet );
  blockSignals( false );
  emit dataChanged();
}

ReosTimeWindowSettings::OffsetOrigin ReosTimeWindowSettings::originStart() const
{
  return mOriginStart;
}

void ReosTimeWindowSettings::setOriginStart( OffsetOrigin newOriginStart )
{
  mOriginStart = newOriginStart;
  emit dataChanged();
}

ReosTimeWindowSettings::OffsetOrigin ReosTimeWindowSettings::originEnd() const
{
  return mOriginEnd;
}

void ReosTimeWindowSettings::setOriginEnd( OffsetOrigin newOriginEnd )
{
  mOriginEnd = newOriginEnd;
  emit dataChanged();
}
