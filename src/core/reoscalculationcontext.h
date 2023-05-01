/***************************************************************************
  reoscalculationcontext.h - ReosCalculationContext

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
#ifndef REOSCALCULATIONCONTEXT_H
#define REOSCALCULATIONCONTEXT_H

#include <QPointer>

#include "reoscore.h"
#include "reosmeteorologicmodel.h"

class REOSCORE_EXPORT ReosCalculationContext
{
  public:
    ReosCalculationContext();

    ReosMeteorologicModel *meteorologicModel() const SIP_SKIP;
    void setMeteorologicModel( ReosMeteorologicModel *meteoModel ) SIP_SKIP;

    QString schemeId() const;
    void setSchemeId( const QString &schemeId );

    const ReosTimeWindow &timeWindow() const;
    void setTimeWindow( const ReosTimeWindow &newTimeWindow );

  private:
    QString mSchemeId;
    QPointer<ReosMeteorologicModel> mMeteoModel;
    ReosTimeWindow mTimeWindow;
};

#endif // REOSCALCULATIONCONTEXT_H
