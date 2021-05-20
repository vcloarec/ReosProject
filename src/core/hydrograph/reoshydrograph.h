/***************************************************************************
  reoshydrograph.h

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
#ifndef REOSHYDROGRAPH_H
#define REOSHYDROGRAPH_H

#include <QColor>

#include "reostimeserie.h"

//! Class that represents a hydrograph
class REOSCORE_EXPORT ReosHydrograph : public ReosTimeSerieVariableTimeStep
{
  public:
    ReosHydrograph( QObject *parent = nullptr ): ReosTimeSerieVariableTimeStep( parent ) {}

    QString type() const override {return QStringLiteral( "hydrograph" );}
    QColor color() const override;
    void setColor( const QColor &color );

  private:
    QColor mColor;
};

#endif // REOSHYDROGRAPH_H
