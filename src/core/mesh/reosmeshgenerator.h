/***************************************************************************
  reosmeshgenerator.h - ReosMeshGenerator

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
#ifndef REOSMESHGENERATOR_H
#define REOSMESHGENERATOR_H

#include <memory>

#include <QObject>
#include <QVector>
#include <QPolygonF>

#include "reoscore.h"

#include "reosdataobject.h"
#include "reosprocess.h"

class ReosPolylinesStructure;
class ReosParameterBoolean;
class ReosParameterDouble;
class ReosPolygonStructure;
class ReosTopographyCollection;

//! Structure that contains mesh frame data
struct ReosMeshFrameData
{
  QVector<double> vertexCoordinates;
  QVector<QVector<int>> facesIndexes;
  QRectF extent;
  bool hasZ = false;
  QVector<QVector<int>> boundaryVertices;
  QVector<QVector<QVector<int>>> holesVertices;
};


class ReosMeshGeneratorProcess: public ReosProcess
{
  public:

    virtual ReosMeshFrameData meshResult() const = 0;
};

/**
 * Class that controls the resolution of a generated mesh
 */
class REOSCORE_EXPORT ReosMeshResolutionController : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosMeshResolutionController( QObject *parent = nullptr, const QString &wktCrs = QString() );
    ReosMeshResolutionController( const ReosEncodedElement &element, QObject *parent = nullptr );

    ~ReosMeshResolutionController();

    ReosMeshResolutionController *clone() const;

    ReosPolygonStructure *resolutionPolygons() const;

    ReosParameterDouble *defaultSize() const;

    ReosEncodedElement encode() const;

  private:
    ReosMeshResolutionController( const ReosMeshResolutionController *other );
    ReosParameterDouble  *mDefaultSize = nullptr;
    std::unique_ptr<ReosPolygonStructure> mPolygonStructure;
};

/**
 * Abstract class used to generate mesh frame
 */
class REOSCORE_EXPORT ReosMeshGenerator : public ReosDataObject
{
    Q_OBJECT
  public:

    virtual ReosMeshGeneratorProcess *getGenerateMeshProcess(
      ReosPolylinesStructure *structure,
      ReosMeshResolutionController *resolutionControler,
      const QString &destinationCrs = QString() ) const = 0;

    ReosParameterBoolean *autoUpdateParameter() const;

    static ReosMeshGenerator *createMeshGenerator( const ReosEncodedElement &element, QObject *parent = nullptr );
    virtual ReosEncodedElement encode() const = 0;

    //! Static method hat return the type of this class
    static QString staticType() {return QStringLiteral( "mesh-generator" );}

  protected:
    ReosMeshGenerator( QObject *parent = nullptr );
    ReosMeshGenerator( const ReosEncodedElement &element, QObject *parent = nullptr );
    ReosEncodedElement encodeBase() const;

  private:
    ReosParameterBoolean *mAutoUpdateParameter = nullptr;
};

class ReosMeshGeneratorPoly2TriProcess: public ReosMeshGeneratorProcess
{
  public:
    ReosMeshGeneratorPoly2TriProcess( const QPolygonF &domain );

    void start();

    ReosMeshFrameData meshResult() const {return mResult;}

  private:
    QPolygonF mDomain;
    ReosMeshFrameData mResult;
};


/**
 * A generator that simply triangulates a domain without internal vertex
 */
class ReosMeshGeneratorPoly2Tri : public ReosMeshGenerator
{
  public:

    virtual ReosMeshGeneratorProcess *getGenerateMeshProcess( ReosPolylinesStructure *structure,
        ReosMeshResolutionController *resolutionControler, const QString &crs = QString() ) const override;

    //! Sets the \a domain to triangulate
    void setDomain( const QPolygonF &domain );

    QString type() const override {return QStringLiteral( "poly2tri" );}
    ReosEncodedElement encode() const override;

  private:
    QPolygonF mDomain;

};


#endif // REOSMESHGENERATOR_H
