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

#include <memory>
#include <functional>

#include "reosmeshgenerator.h"
#include "reosparameter.h"
#include "reospolygonstructure.h"

class ReosGmshResolutionController: public ReosMeshResolutionController
{
  public:
    ReosGmshResolutionController( QObject *parent = nullptr, const QString &wktCrs = QString() );

    double sizeFallBack( int dim, int tag, double x, double y, double z, double lc );
    ReosParameterDouble *defaultSize() const;

    ReosPolygonStructure *resolutionPolygons();

  private:
    ReosParameterDouble  *mDefaultSize;
    std::unique_ptr<ReosPolygonStructure> mPolygonStructure = nullptr;


};

class ReosGmshGenerator : public ReosMeshGenerator
{
  public:
    ReosGmshGenerator()
    {
    }

    ReosMeshFrameData generatedMesh( bool *ok ) const override;
    void setGeometryStructure( ReosPolylinesStructure *structure, const QString &crs ) override;
    void setResolutionController( ReosMeshResolutionController *resolutionControler ) override
    {
      mSizeControler = static_cast<ReosGmshResolutionController *>( resolutionControler );
    }

  private:
    ReosGmshResolutionController *mSizeControler = nullptr;
};


#endif // REOSGMSHGENERATOR_H
