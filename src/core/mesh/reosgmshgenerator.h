/***************************************************************************
  reosgmshgenerator.h - ReosGmeshGenerator

 ---------------------
 begin                : 14.1.2022
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
#ifndef REOSGMSHGENERATOR_H
#define REOSGMSHGENERATOR_H

#include "reosmeshgenerator.h"

class ReosGmshGenerator : public ReosMeshGenerator
{
  public:

    ReosMeshFrameData generatedMesh( bool *ok ) const override;
    void setGeometryStructure( ReosPolylinesStructure *structure, const QString &crs ) override;
};


#endif // REOSGMSHGENERATOR_H
