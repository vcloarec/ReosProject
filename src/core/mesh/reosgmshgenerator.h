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

#include <QMutex>
#include <QThread>
#include <QDebug>

#include "reosmodule.h"
#include "reosmeshgenerator.h"
#include "reosparameter.h"
#include "reospolylinesstructure.h"
#include "reospolygonstructure.h"


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

    ReosMeshGeneratorProcess *getGenerateMeshProcess( ReosPolylinesStructure *structure, ReosMeshResolutionController *resolutionControler ) const override;

    ReosEncodedElement encode() const override;

  private:
    Algorithm mAlgorithm = FrontalDelaunay;

};

class ReosGmshEngine : public ReosModule
{
    Q_OBJECT
  public:
    static ReosGmshEngine *instance();

    ReosMeshFrameData generateMesh( const ReosPolylinesStructure::Data &data,
                                    ReosMeshResolutionController *resolutionControler,
                                    ReosGmshGenerator::Algorithm alg );

    static void instantiate( QObject *parent );

  signals:
    void startGenerate();

  private:
    ReosGmshEngine( QObject *parent );
    QMutex mMutex;
    static ReosGmshEngine *sInstance;
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
};


#endif // REOSGMSHGENERATOR_H
