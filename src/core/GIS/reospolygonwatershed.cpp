/***************************************************************************
  reospolygonwatershed.h - ReosPolygonWatershed

---------------------
begin                : 26.8.2026
copyright            : (C) 2026 by Vincent Cloarec
email                : vcloarec at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************/

#include "reospolygonwatershed.h"
#include "reospolygonwatershed_p.h"

ReosPolygonWatershed::ReosPolygonWatershed()
{}

ReosPolygonWatershed *ReosPolygonWatershed::createPolygonWatershed( const QString &wktCrs )
{
  return new ReosPolygonWatershed_p( wktCrs );
}

QPolygonF ReosPolygonWatershed::searchPolygon( const ReosSpatialPosition &, bool ) const
{
  return QPolygonF();
}
