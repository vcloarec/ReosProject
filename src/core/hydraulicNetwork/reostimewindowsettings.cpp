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
  , mUseExternalDefinedTimeWindow( new ReosParameterBoolean( tr( "Use external time windows" ) ) )
  , mAutomaticallyDefined( new ReosParameterBoolean( tr( "Defined automatically" ) ) )
  , mStartOffset( new ReosParameterDuration( QString(), this ) )
  , mEndOffset( new ReosParameterDuration( QString(), this ) )
  , mUserStartTime( new ReosParameterDateTime( tr( "User start time" ), this ) )
  , mUserEndTime( new ReosParameterDateTime( tr( "User end time" ), this ) )
{
  mUseExternalDefinedTimeWindow->setValue( true );
  mAutomaticallyDefined->setValue( true );
  mStartOffset->setValue( ReosDuration( 0, ReosDuration::hour ) );
  mStartOffset->changeUnit( ReosDuration::hour );
  mEndOffset->setValue( ReosDuration( 0, ReosDuration::hour ) );
  mEndOffset->changeUnit( ReosDuration::hour );

  connect( mUseExternalDefinedTimeWindow, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mAutomaticallyDefined, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mStartOffset, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mEndOffset, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUserStartTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUserEndTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosTimeWindow ReosTimeWindowSettings::timeWindow( const ReosTimeWindow &input ) const
{
  if ( mAutomaticallyDefined->value() )
  {
    QDateTime startOrigin;
    switch ( mOriginStart )
    {
      case Begin:
        startOrigin = input.start();
        break;
      case End:
        startOrigin = input.end();
        break;
    }
    QDateTime endOrigin;
    switch ( mOriginEnd )
    {
      case Begin:
        endOrigin = input.start();
        break;
      case End:
        endOrigin = input.end();
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

ReosParameterDuration *ReosTimeWindowSettings::startOffsetParameter() const
{
  return mStartOffset;
}

ReosParameterDuration *ReosTimeWindowSettings::endOffsetParameter() const
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
  element.addData( QStringLiteral( "externally-defined" ), mUseExternalDefinedTimeWindow->value() );
  element.addData( QStringLiteral( "automatically-defined" ), mAutomaticallyDefined->value() );
  element.addData( QStringLiteral( "combine-method" ), mCombineMethod );
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
  bool externallyDefined = true;
  element.getData( QStringLiteral( "externally-defined" ), externallyDefined );
  mUseExternalDefinedTimeWindow->setValue( externallyDefined );
  bool autoDefined = true;
  element.getData( QStringLiteral( "automatically-defined" ), autoDefined );
  mAutomaticallyDefined->setValue( autoDefined );
  int cm = 0;
  if ( element.getData( QStringLiteral( "combine-method" ), cm ) )
    mCombineMethod = static_cast<ReosTimeWindowSettings::CombineMethod>( cm );
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

ReosTimeWindowSettings::CombineMethod ReosTimeWindowSettings::combineMethod() const
{
  return mCombineMethod;
}

void ReosTimeWindowSettings::setCombineMethod( CombineMethod newCombineMethod )
{
  mCombineMethod = newCombineMethod;
  emit dataChanged();
}

ReosParameterBoolean *ReosTimeWindowSettings::useExternalDefinedTimeWindow() const
{
  return mUseExternalDefinedTimeWindow;
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

void ReosTimeWindowSettings::setStartOffset( const ReosDuration &startOffset )
{
  mStartOffset->setValue( startOffset );
}

void ReosTimeWindowSettings::setEndOffset( const ReosDuration &endOffset )
{
  mEndOffset->setValue( endOffset );
}

void ReosTimeWindowSettings::setUserStartTime( const QDateTime &startTime )
{
  mUserStartTime->setValue( startTime );
}

void ReosTimeWindowSettings::setUserEndTime( const QDateTime &endTime )
{
  mUserEndTime->setValue( endTime );
}

void ReosTimeWindowSettings::setAutomaticallyDefined( bool b )
{
  mAutomaticallyDefined->setValue( b );
}

void ReosTimeWindowSettings::setUseExternalDefinedTimeWindow( bool b )
{
  mUseExternalDefinedTimeWindow->setValue( b );
}
