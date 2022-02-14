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

    enum Algorithm
    {
      MeshAdapt,
      Automatic,
      InitialMesh,
      Delaunay,
      FrontalDelaunay,
      BAMG,
      FrontalDelaunayForQuads,
      PackingOfParallelograms
    };

    ReosGmshGenerator( QObject *parent = nullptr );
    ReosGmshGenerator( const ReosEncodedElement &element, QObject *parent = nullptr );

    ReosMeshGeneratorProcess *generatedMesh(
      ReosPolylinesStructure *structure,
      ReosMeshResolutionController *resolutionControler,
      bool *ok ) const override;


    ReosEncodedElement encode() const;

  private:
    Algorithm mAlgorithm = FrontalDelaunay;

};

class ReosMeshGeneratorGmshProcess: public ReosMeshGeneratorProcess
{
  public:
    ReosMeshGeneratorGmshProcess( ReosPolylinesStructure *structure,
                                  ReosMeshResolutionController *resolutionControler,
                                  ReosGmshGenerator::Algorithm alg );

    void start();

    ReosMeshFrameData meshResult() const {return mResult;}

  private:
    ReosMeshFrameData mResult;
    ReosPolylinesStructure::Data mData;
    std::unique_ptr<ReosMeshResolutionController> mResolutionControler;
    ReosGmshGenerator::Algorithm mAlgorithm = ReosGmshGenerator::FrontalDelaunay;

    double sizeFallBack( int dim, int tag, double x, double y, double z, double lc );
};


#endif // REOSGMSHGENERATOR_H
