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

#include "reosmodule.h"
#include "reosmeshgenerator.h"
#include "reosparameter.h"
#include "reospolylinesstructure.h"
#include "reospolygonstructure.h"


class REOSCORE_EXPORT ReosGmshGenerator : public ReosMeshGenerator
{
    Q_OBJECT
  public:

    enum Algorithm
    {
      MeshAdapt,
      Automatic,
      InitialMesh,
      Delaunay,
      FrontalDelaunay,
      BAMG,
      //FrontalDelaunayForQuads,
      //PackingOfParallelograms,
      AlgCount
    };

    ReosGmshGenerator( QObject *parent = nullptr );
    explicit ReosGmshGenerator( const ReosEncodedElement &element, QObject *parent = nullptr );

    ReosMeshGeneratorProcess *getGenerateMeshProcess( ReosPolylinesStructure *structure,
        ReosMeshResolutionController *resolutionControler,
        const QString &destinationCrs = QString() ) const override;

    QString type() const override {return staticType();}
    ReosEncodedElement encode() const override;

    static QString staticType() {return ReosMeshGenerator::staticType() + ':' + QStringLiteral( "gmsh" );}

    Algorithm algorithm() const;
    void setAlgorithm( const Algorithm &algorithm );

    static QString algorithmName( Algorithm alg );

    static QString version();

  private:
    Algorithm mAlgorithm = FrontalDelaunay;

};

class ReosGmshEngine : public ReosModule
{
    Q_OBJECT
  public:
    static ReosGmshEngine *instance();

    ReosMeshFrameData generateMesh( const ReosPolylinesStructure::Data &data,
                                    ReosPolygonStructureValues *resolutionValues,
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
    Q_OBJECT
  public:
    ReosMeshGeneratorGmshProcess( ReosPolylinesStructure *structure,
                                  ReosMeshResolutionController *resolutionControler,
                                  ReosGmshGenerator::Algorithm alg,
                                  const QString &destinationCrs = QString() );

    void start() override;

    ReosMeshFrameData meshResult() const override {return mResult;}

  private:
    ReosMeshFrameData mResult;
    ReosPolylinesStructure::Data mData;
    ReosGmshGenerator::Algorithm mAlgorithm = ReosGmshGenerator::FrontalDelaunay;
    QString mDestinationCrs;
    std::unique_ptr<ReosPolygonStructureValues> mResolutionValues;
};


#endif // REOSGMSHGENERATOR_H
