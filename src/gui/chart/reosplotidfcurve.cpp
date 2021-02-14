/***************************************************************************
  reosplotidfcurve.cpp - ReosPlotIdfCurve

 ---------------------
 begin                : 5.2.2021
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
#include "reosplotidfcurve.h"
#include "reosidfplot_p.h"

ReosPlotIdfCurve::ReosPlotIdfCurve( ReosIntensityDurationCurve *curve,  const QString &name )
{
  mPlotItem = new ReosIdfPlot_p( curve );
  mPlotItem->setTitle( name );
  mPlotItem->setItemAttribute( QwtPlotItem::AutoScale, true );
  connect( curve, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
  connect( curve, &ReosDataObject::dataChanged, this, &ReosPlotItem::fullExtent );
}

void ReosPlotIdfCurve::setColors( const QColor &color )
{
  item()->setColor( color );
}

ReosIdfPlot_p *ReosPlotIdfCurve::item()
{
  return static_cast<ReosIdfPlot_p *>( mPlotItem );
}
