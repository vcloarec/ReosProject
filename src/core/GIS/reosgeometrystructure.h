/***************************************************************************
  reosgeometrystructure.h - ReosGeometryStructure

 ---------------------
 begin                : 5.2.2022
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
#ifndef REOSGEOMETRYSTRUCTURE_H
#define REOSGEOMETRYSTRUCTURE_H

#include "reosdataobject.h"

class ReosMapExtent;

class ReosGeometryStructureVertex
{
  public:
    virtual ~ReosGeometryStructureVertex() = default;
};

class ReosGeometryStructure : public ReosDataObject
{
  public:
    virtual ~ReosGeometryStructure() = default;
    virtual QObject *data() = 0;

    //! Returns the extent of the structure in \a crs coordinate system
    virtual ReosMapExtent extent( const QString &crs ) const = 0;
};


#endif // REOSGEOMETRYSTRUCTURE_H
