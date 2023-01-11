/***************************************************************************
  reoscalculationcontext.cpp - ReosCalculationContext

 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoscalculationcontext.h"

ReosCalculationContext::ReosCalculationContext()
{

}

ReosMeteorologicModel *ReosCalculationContext::meteorologicModel() const
{
  if ( mMeteoModel.isNull() )
    return nullptr;

  return mMeteoModel;
}

void ReosCalculationContext::setMeteorologicModel( ReosMeteorologicModel *meteoModel )
{
  mMeteoModel = meteoModel;
}

QString ReosCalculationContext::schemeId() const
{
  return mSchemeId;
}

void ReosCalculationContext::setSchemeId( const QString &schemeId )
{
  mSchemeId = schemeId;
}

const ReosTimeWindow &ReosCalculationContext::timeWindow() const
{
  return mTimeWindow;
}

void ReosCalculationContext::setTimeWindow( const ReosTimeWindow &newTimeWindow )
{
  mTimeWindow = newTimeWindow;
}
