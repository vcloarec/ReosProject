/***************************************************************************
  reostimewindowsettings.h - ReosTimeWindowSettings

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
#ifndef REOSTIMEWINDOWSETTINGS_H
#define REOSTIMEWINDOWSETTINGS_H


#include "reosdataobject.h"
#include "reosparameter.h"

class REOSCORE_EXPORT ReosTimeWindowSettings : public ReosDataObject
{
    Q_OBJECT
  public:
    enum OffsetOrigin
    {
      Begin,
      End,
    };

    enum CombineMethod
    {
      Intersection,
      Union,
    };

    explicit ReosTimeWindowSettings( QObject *parent = nullptr );

    //! Returns a time window defined by the settings, using input if time window is defined automatically
    ReosTimeWindow timeWindow( const ReosTimeWindow &input ) const;

    OffsetOrigin originStart() const;
    void setOriginStart( OffsetOrigin newOriginStart );

    OffsetOrigin originEnd() const;
    void setOriginEnd( OffsetOrigin newOriginEnd );

    void setStartOffset( const ReosDuration &startOffset );
    void setEndOffset( const ReosDuration &endOffset );
    void setUserStartTime( const QDateTime &startTime );
    void setUserEndTime( const QDateTime &endTime );

    void setAutomaticallyDefined( bool b );
    void setUseExternalDefinedTimeWindow( bool b );

    CombineMethod combineMethod() const;
    void setCombineMethod( CombineMethod newCombineMethod );

#ifndef SIP_RUN
    ReosParameterDuration *startOffsetParameter() const;
    ReosParameterDuration *endOffsetParameter() const;
    ReosParameterDateTime *userStartTime() const;
    ReosParameterDateTime *userEndTime() const;
    ReosParameterBoolean *automaticallyDefined() const;
    ReosParameterBoolean *useExternalDefinedTimeWindow() const;

    void decode( const ReosEncodedElement &element );
    ReosEncodedElement encode() const;
#endif //No SIP_RUN

  private:
    ReosParameterBoolean *mUseExternalDefinedTimeWindow = nullptr;
    ReosParameterBoolean *mAutomaticallyDefined = nullptr;
    CombineMethod mCombineMethod = Intersection;
    ReosParameterDuration *mStartOffset = nullptr;
    ReosParameterDuration *mEndOffset = nullptr;
    OffsetOrigin mOriginStart = Begin;
    OffsetOrigin mOriginEnd = End;


    ReosParameterDateTime *mUserStartTime = nullptr;
    ReosParameterDateTime *mUserEndTime = nullptr;
};
#endif // REOSTIMEWINDOWSETTINGS_H
