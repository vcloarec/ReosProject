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

class ReosTimeWindowSettings : public ReosDataObject
{
  public:
    enum OffsetOrigin
    {
      Begin,
      End
    };

    explicit ReosTimeWindowSettings( QObject *parent );

    virtual ReosTimeWindow timeWindow( const ReosTimeWindow &automaticTimeWindow ) const;

    ReosParameterDuration *startOffset() const;
    ReosParameterDuration *endOffset() const;

    OffsetOrigin originStart() const;
    void setOriginStart( OffsetOrigin newOriginStart );

    OffsetOrigin originEnd() const;
    void setOriginEnd( OffsetOrigin newOriginEnd );

    ReosParameterDateTime *userStartTime() const;
    ReosParameterDateTime *userEndTime() const;
    ReosParameterBoolean *automaticallyDefined() const;

    ReosEncodedElement encode() const;

    void decode( const ReosEncodedElement &element );

  protected:
    ReosParameterBoolean *mAutomaticallyDefined = nullptr;
    ReosParameterDuration *mStartOffset = nullptr;
    ReosParameterDuration *mEndOffset = nullptr;
    OffsetOrigin mOriginStart = Begin;
    OffsetOrigin mOriginEnd = End;

    ReosParameterDateTime *mUserStartTime = nullptr;
    ReosParameterDateTime *mUserEndTime = nullptr;
};
#endif // REOSTIMEWINDOWSETTINGS_H
